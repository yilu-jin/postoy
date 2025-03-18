# Not a final version. 
# Still in progress ... as more functions being developed

# Check that input is a valid dataset
assert_data_frame <- function(df) {
  if (!is.data.frame(df)) {
    stop(
      "The required argument ",
      deparse(substitute(df)),
      " cannot be missing. Please specify a valid input dataset."
    )
  }
}

# Check that all names are among column names
check_all_colnames <- function(df, x, null_ok = TRUE) {
  assert_data_frame(df)
  checkmate::check_character(x, null.ok = null_ok)
  
  diff <- setdiff(x, colnames(df))
  
  if (length(diff) == 0) {
    invisible(NULL)
  } else {
    stop(
      "Expected column names: ",
      toString(diff),
      " not in ",
      deparse(substitute(df))
    )
  }
}

# Check variable is in a data frame
assert_valid_var <- function(df, var, value) {
  assert_data_frame(df)
  checkmate::assert_string(var)
  checkmate::assert_character(value)
  check_all_colnames(df, var)
  
  lvl <- unique(df[[var]])
  diff <- setdiff(value, lvl)
  
  if (length(diff) == 0) {
    invisible(NULL)
  } else {
    stop(
      "Expected levels: ",
      toString(diff),
      " not in the ",
      var, " of ",
      deparse(substitute(df)), "!"
    )
  }
  
}

# Check that column_ arguments are valid
assert_spanner_cols <- function(column_group, column_grouped_name, column_order, treatment_order) {
  if( (!is.null(column_group) | !is.null(column_grouped_name)) && 
      is.null(treatment_order) && is.null(column_order)) {
    stop('Either argument, treatment_order or column_order must not be NULL to implement the spanner column label. ')
  }
}

# Provide default for column_order
assert_column_order <- function(column_order, treatment_order, display_total_list) {
  checkmate::assert_character(column_order, null.ok = TRUE)
  checkmate::assert_character(treatment_order, null.ok = TRUE)
  checkmate::assert_list(display_total_list, null.ok = TRUE)
  
  if(is.null(column_order) && !is.null(treatment_order)) {
    column_order <- c(treatment_order, names(display_total_list))
  }
  return(column_order)
}

# Provide default for outfile
assert_outfile <- function(output_file, tlf_number) {
  if(is.null(output_file)) {
    tlf_nm <- trimws(gsub('table', '', tolower(tlf_number)))
    output_file <- paste0('t_', gsub('\\.', '_', trimws(tlf_nm)), '.rtf')
  }else {
    if (!is.character(output_file)) output_file <- as.character(output_file)
    if (!grepl('.rtf', output_file)) output_file <- paste0(output_file, '.rtf')
  }
  return(output_file)
}

# Define subtype
def_subtype <- function(subgroup_var, subgroup_by_col) {
  case_when(
    is.null(subgroup_var) ~ 'no',
    isFALSE(subgroup_by_col) ~ 'multi',
    TRUE ~ 'same'
  )
}

# Define subgroup order
def_subgroup <- function(subgroup_order, subgroup_var, population_from, null_ok = TRUE) {
  checkmate::assert_character(subgroup_order, null.ok = null_ok)
  checkmate::assert_string(subgroup_var, null.ok = null_ok)
  assert_data_frame(population_from)
  check_all_colnames(population_from, subgroup_var, null_ok = null_ok)
  
  if(!is.null(subgroup_var)) {
    
    if (is.null(subgroup_order))  {
      subgroup_order <- unique(population_from[[subgroup_var]])
      subgroup_order <- rmna(subgroup_order)
    } else {
      assert_valid_var(population_from, subgroup_var, subgroup_order)
    }
    
  }
  
  return(subgroup_order)
}

# Define subgroup labels
def_subgroup_lbl <- function(subgroup_label, subgroup_var, null_ok = TRUE) {
  checkmate::assert_string(subgroup_label, null.ok = null_ok)
  checkmate::assert_string(subgroup_var, null.ok = null_ok)
  if(!is.null(subgroup_var) && is.null(subgroup_label)) subgroup_label <- subgroup_var
  return(subgroup_label)
}

# Take a subset of a data frame
subset <- function(df, where, null_ok = TRUE) {
  assert_data_frame(df)
  checkmate::assert_character(where, null.ok = TRUE)
  
  if (!is.null(where)) {
    filter <- paste0("df %>% dplyr::filter(", toString(where), ")")
    df <- tryCatch({
        eval(parse(text=filter))
      },
      error = function(err){
        message("Error at ", deparse(substitute(where)), ": ", toString(where), ". \n",
                "Original error message: ", conditionMessage(err))
      })
  }
  return(df)
}

# Prepare ID variable
prep_id <- function(df, idvar) {
  assert_data_frame(df)
  check_all_colnames(df, idvar)
  
  df %>% dplyr::mutate(
    id = !!sym(idvar) %>% as.character())
}

# Prepare treatment variable
prep_trt <- function(df, treatment_var, treatment_order) {
  assert_data_frame(df)
  check_all_colnames(df, treatment_var)
  
  df %>% dplyr::mutate(
    trt = !!sym(treatment_var) %>% as.character(),
    trt = dunlin::reformat(trt, missing_rule)
  )  %>% 
    dplyr::filter(trt != '<Missing>') %>% 
    dplyr::filter(trt %in% treatment_order) %>% 
    dplyr::mutate(trt = factor(trt, levels = treatment_order))
}

# Include total groups into treatments
include_total <- function(df, total) {
  assert_data_frame(df)
  checkmate::assert_list(total, null.ok = TRUE)
  check_all_colnames(df, "trt")
  
  if(!is.null(total)) {
  
    # assert_valid_var(df, "trt", unlist(total))
    
    df <- 
      names(total) %>% 
      purrr::map(~ 
        df %>% 
          filter(trt %in% total[[.x]]) %>% 
          mutate(trt = .x)
      ) %>% 
      dplyr::bind_rows() %>% 
      dplyr::bind_rows(df) %>% 
      dplyr::mutate(trt = factor(trt, levels = c(levels(df$trt), names(total))))
  }
  
  return(df)
}

# Create dummy data frames
dummy <- function(x, nrow = 1, ncol = length(x)) {
  matrix(x, nrow = nrow, ncol = ncol) %>% as.data.frame()
}

# Populate actual values into dummy data frames
dummy_df <- function(
    x = "headerN",  # values in first column
    fmts = "xx", # formats of `x`, must be of same length
    ncols = 1 # how many columns repeating for formats
) {
  checkmate::assert_character(x)
  checkmate::assert_character(fmts)
  stopifnot(length(x) == length(fmts))
  
  # values of each cell
  values <- c(x, rep(fmts, ncols))
  # build up dummy data frame
  df <- dummy(values, nrow = length(x), ncol = ncols+1)
  
  return(df)
}


# Missing rule
missing_rule <- dunlin::rule("<Missing>" = c("", NA), .drop = TRUE)


# Prep descriptive summary for continuous variables
desc_cont <- function(
    df,              # input dataset
    analyze_var,     # variable to be analyze, must be numeric
    row_vars = NULL, # row variable(s) - optional
    stats = c('mean', 'sd', 'median', 'min', 'max') # descriptive statistics to include
){
  
  vars <- c("trt", analyze_var, row_vars)
  stats_choices <- c('n', 'mean', 'sd', 'median', 'min', 'max', 'q1', 'q3')
  
  assert_data_frame(df)
  check_all_colnames(df, vars)
  checkmate::assert_subset(stats, choices = stats_choices)
  
  dd <- df %>% 
    dplyr::mutate(value = !!rlang::sym(analyze_var)) %>% 
    dplyr::group_by(across(all_of(c(row_vars, "trt")))) %>%
    dplyr::summarise(
      stats_n      = n(),
      stats_mean   = mean(value, na.rm = TRUE),
      stats_sd     = sd(value, na.rm = TRUE),
      stats_median = median(value, na.rm = TRUE),
      stats_min    = min(value, na.rm = TRUE),
      stats_max    = max(value, na.rm = TRUE),
      stats_q1     = quantile(value, prob = .25, na.rm = TRUE),
      stats_q3     = quantile(value, prob = .75, na.rm = TRUE), 
      .groups = 'drop'
    ) %>% 
    dplyr::mutate(
      # mean function returns NaN when all values are NA
      # set NaN to NA manually
      stats_mean = ifelse(is.nan(stats_mean), NA, stats_mean),
      # min and max functions return Inf/-Inf when all values are NA
      # set infinite values to NA manually
      stats_min = ifelse(is.infinite(stats_min), NA, stats_min),
      stats_max = ifelse(is.infinite(stats_max), NA, stats_max)
    ) %>% 
    tidyr::pivot_longer(dplyr::starts_with('stats_')) %>% 
    dplyr::mutate(stats = gsub('stats_', '', name)) %>% 
    dplyr::filter(stats %in% stats) %>% 
    suppressWarnings() # suppress warnings of min/max applied to numeric of length 0 argument
  
  return(dd)
    
}

# Pivot a data frame into a wide format, one arm per column
pivot_cont <- function(dd) {
  dd %>% 
    tidyr::pivot_wider(
      names_from = trt, 
      values_from = value,
      names_prefix = "n_"
    ) 
}

# Paste numeric stats into strings
paste_stats <- function(df, var = "stats", row_vars = NULL,
                        fmt = list("Mean (SD)" = "{Round(mean, 1)} ({Round(sd, 1)})")
                        ) {
  assert_data_frame(df)
  checkmate::assert_string(var)
  check_all_colnames(df, var)
  checkmate::assert_list(fmt, names = "named")
  
  checkmate::assert_character(row_vars, null.ok = TRUE)
  vars <- c(row_vars, var)
  
  # transpose into a trt (rows) x stats (columns) matrix table
  df2 <- df %>% 
    dplyr::select(any_of(vars), where(is.numeric)) %>% 
    tidyr::pivot_longer(-any_of(vars)) %>% 
    tidyr::pivot_wider(names_from = all_of(var))
  
  # paste stats into desired format and transpose back to a stats x trt matrix table
  purrr::map(
    purrr::set_names(names(fmt)),
    function(this_fmt) {
      df2 %>% 
        dplyr::mutate(value = glue::glue(fmt[[this_fmt]])) %>% 
        dplyr::select(any_of(row_vars), name, value) %>% 
        tidyr::pivot_wider() 
    }
  ) %>% dplyr::bind_rows(.id = "label")
    
}


# Prep descriptive summary for categorical variables
desc_cate <- function(
  df,              # input dataset
  row_vars = NULL, # row variable(s) - optional
  unique = TRUE # if true, distinct id counts will be calculated
                #  otherwise, number of rows will be calculated
  ){
  
  assert_data_frame(df)
  vars <- c(if(isFALSE(unique)) NULL else "id", "trt", row_vars)
  check_all_colnames(df, vars)
  
  dd <- df %>% 
    dplyr::group_by(across(all_of(c(row_vars, "trt")))) %>%
    {if(isTRUE(unique)) {
      dplyr::summarise(., n = n_distinct(id))
    }else{
      dplyr::summarise(., n = n())
    }} %>% 
    suppressMessages()

  return(dd)
    
}

# Header N
big_n <- function(df) {
  nn <- df %>% 
    desc_cate() %>% 
    dplyr::rename(bign = n)
  return(nn)
}

# Calculate percentage in arm groups
percent_cate <- function(dd, nn) {
  ddp <- dd %>% 
    dplyr::left_join(nn, by = 'trt') %>% 
    dplyr::mutate(pct = n/bign*100) %>% 
    dplyr::select(-bign)
  return(ddp)
}

# Pivot into wide tables
pivot_cate <- function(ddp) {
  ddp %>% 
    tidyr::pivot_wider(
      names_from = trt, 
      values_from = c(n, pct)
    ) 
}


# add an extra row before groups
add_grp_row <- function(df, vars) {
  assert_data_frame(df)
  checkmate::assert_character(vars)
  check_all_colnames(df, vars, null_ok = FALSE)
  
  gg <- df %>% 
    group_by(!!!rlang::syms(vars))
  
  gg <- bind_rows(
    group_keys(gg),
    group_split(gg)
  ) %>% 
    arrange(!!!rlang::syms(vars))
  
  return(gg)
}


# Formatting p values
pfmt <- function(x) {
  case_when(
    is.na(x) | x == 0 ~ "",
    x < 0.1 ~ " (<0.1)",
    x > 99.9 & x < 100 ~ " (>99.9)",
    TRUE ~ paste0(" (", trimws(format(Round(as.numeric(x),1), nsmall = 1)), ")")
  )
}

# Pasting two dfs into one cell by cell 
pst2df <- function(df1, df2) {
  purrr::map2_dfc(df1, df2, ~ paste0(.x, .y))
}

# Selecting columns
keep_cols <- function(df, vars, ...) {
  df %>% dplyr::select(any_of(vars), ...)
}

# De-selecting columns
drop_cols <- function(df, vars, ...) {
  df %>% dplyr::select(-any_of(vars), ...)
}

# Add ordering variable
add_ord <- function(df) {
  
  assert_data_frame(df)
  check_all_colnames(df, "name", null_ok = FALSE)
  
  df %>% dplyr::mutate(
    ord = case_when(
      grepl('headern', tolower(name)) ~ 1,
      TRUE ~ 2
      )
  )
}

# processing ae_pt_threshold
th_processing <- function(str, null_ok = TRUE) {
  
  checkmate::assert_string(str, null.ok = null_ok)
  
  if(!is.null(str)){
    th_chr <- NULL
    for(th_sign in c('==', '<=', '>=', '<', '>')){
      if(grepl(th_sign, str)) {
        th_chr <- th_sign 
        break
      } 
    }
    if(is.null(th_chr)) 
      stop(
        "There must be one of '<', '<=', '>', '>=', or '==' in the ",  
        deparse(substitute(str)), 
        " argument!"
        )
    
    th_num <- stringr::str_replace_all(str,paste0(th_chr),'')
    th_num <- as.numeric(th_num)
    stopifnot(th_num >= 0 | th_num <= 100)
    
    str <- list(
      sign = th_chr,
      val = th_num
    )
  } 
  
  str
}

# add leading blank space of length 2
add2ws <- function(x) {
  paste0("  ", x)
}

# Replace zero with empty strings
zero2blank <- function(x) {
  ifelse(x=='0', '', as.character(x))
}

# Replace NAs with zero
na2zero <- function(x) {
  ifelse(is.na(x), '0', as.character(x))
}

# Replace NAs with empty strings
na2blank <- function(x) {
  ifelse(is.na(x), '', as.character(x))
}

# Remove missing or empty strings
rmna <- function(x) {
  x <- x[!is.na(x)]
  x <- x[!x == '']
  return(x)
}



# Rounding
Round <- function (x, digits=0)
{
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  return(z)
}

# Combine RTF files
rtf_assemble1 <- function(path) {
  rtf <- lapply(path, readLines)
  
  n <- length(rtf)
  start <- c(1, rep(2, n - 1))
  end <- vapply(rtf, length, numeric(1))
  end[-n] <- end[-n] - 1
  
  for (i in 1:n) {
    rtf[[i]] <- rtf[[i]][start[i]:end[i]]
    if (i < n) rtf[[i]] <- c(rtf[[i]], r2rtf:::as_rtf_new_page())
  }
  rtf <- do.call(c, rtf)
  
  rtf
}

# Replace \n with \\line, the RTF equivalent
line_break <- function(text) {
  if(!is.null(text)){text <- gsub("\n"," \\\\line ", text)}
  return(text)
}


# export data frame into a RTF file
export_as_rtf <- function(
    tbl,
    col_rel_width = NULL,
    tlf_number = NULL,
    title_text = NULL,
    subtitle_text = NULL,
    studyID = NULL,
    page_header = NULL,
    page_note = NULL,
    page_nrow = 30,
    title_note = NULL,
    foot_note = NULL,
    program_loc = NULL,
    subtype = NULL,
    subgroup_order = NULL,
    subgroup_label = NULL,
    column_order = NULL,
    column_group = NULL,
    column_grouped_name = NULL,
    col_names = rep("",3),        # top-left column headers
    txt_ind_left = c(0, 200, 600) # text indent for col_names
) {
  
  # Validate parameters and set defaults
  if(!is.data.frame(tbl)) {
    stop("The required argument, tbl cannot be missing. Please specify a valid input dataset.")
  }
  
  if(is.null(col_rel_width) | length(col_rel_width) != ncol(tbl) ) {
    col_rel_width <- c(4, rep(2, ncol(tbl)-1))
  }
  
  if(!is.null(tlf_number) && !is.character(tlf_number)) { 
    tlf_number <- as.character(tlf_number) 
  }  
  
  if(is.null(page_nrow) | length(page_nrow) != 1){
    stop('The required argument, page_nrow must be of length 1.')
  }
  
  if(!is.numeric(page_nrow)) {
    page_nrow <- as.integer(page_nrow)
  }
  
  if(!is.null(title_note) && !is.character(title_note)) { 
    title_note <- as.character(title_note) 
  }
  
  if(!is.null(foot_note) && !is.character(foot_note)) { 
    foot_note <- as.character(foot_note)
  }
  
  if(is.null(program_loc)) {  
    program_loc <- rstudioapi::getSourceEditorContext()$path 
  }
  
  if(!subtype %in% c('no', 'multi', 'same')) {
    stop('The required argument, subtype should be one of the no, multi, or same.')
  }
  
  
  # Setting parameters.
  page_note <- page_note %>% paste(collapse = " \\line ") %>% line_break()
  program_loc <- program_loc %>% paste(collapse = " \\line ") %>% line_break()
  title_note <- title_note %>% paste(collapse = " \\line ") %>% line_break()
  foot_note <- foot_note %>% paste(collapse = " \\line ") %>% line_break()
  
  
  if (is.null(title_note) | title_note == "") {
    page_border_first <- 'single'
  } else {
    page_border_first <- ''
  }
  
  
  # feature 1  - subgroup 
  if(subtype == 'same'){
    
    subgroup_header_ws <- vector(length = 2*length(subgroup_order)-1)
    
    for (i in 1:length(subgroup_header_ws)) {
      if((i %% 2) == 0) {
        subgroup_header_ws[i] <- " "
      } else {
        subgroup_header_ws[i] <- subgroup_order[(i %/% 2)+1]
      }
    }
    
    n_trt <- strsplit(names(tbl)[-1],';') %>% 
      lapply(function(x) x[1]) %>% unlist() %>% unique() %>% length() 
    
    sub_cel_width <- unlist(lapply(split(col_rel_width[-1], ceiling(seq_along(col_rel_width[-1])/n_trt)),sum))
    sub_cel_width_ws <- vector(length = length(subgroup_header_ws))
    
    for (i in 1:length(sub_cel_width_ws)) {
      if((i %% 2) == 0) {
        sub_cel_width_ws[i] <- 0.001
      } else if (i != length(sub_cel_width_ws)) {
        sub_cel_width_ws[i] <- sub_cel_width[(i %/% 2)+1]-0.001
      } else if (i == length(sub_cel_width_ws)) {
        sub_cel_width_ws[i] <- sub_cel_width[(i %/% 2)+1]
      }
    }
    
    sub_bottom_border <- ifelse(sub_cel_width_ws == '0.001', '', 'single')
    
  }
  
  
  # feature 2 - spanner column 
  if( !is.null(column_group) | !is.null(column_grouped_name)) {
    
    cols  <- strsplit(names(tbl)[-1],';') %>% 
      lapply(function(x) x[1]) %>% unlist() %>% unique()
    
    df_n_trt <- n_trt <- strsplit(names(tbl)[-1],';') %>% 
      lapply(function(x) x[1]) %>% unlist() %>% unique() %>% length() 
    
    diff <- setdiff(column_order, cols)
    
    if (length(diff) == 0) {
      invisible(NULL)
    } else {
      paste("Expected column names:", toString(diff), "not in", deparse(substitute(tbl)))
    }
    
    if(subtype == 'same') {
      column_grouped_width_t <- col_rel_width[1]
      column_group_ws_t <- " "
      
      for (g in 1:length(subgroup_order)) {
        column_grouped_width <- NULL
        col_rel_width_g <- col_rel_width[(2+(g-1) * df_n_trt) : (1+g * df_n_trt)]
        column_group_ws <- NULL
        
        for (i in 1:length(unique(column_group))) {
          column_grouped_width <- c(column_grouped_width, 0.001, sum(col_rel_width_g[which(column_group == unique(column_group)[i])])-0.001)
          column_group_ws <- c(column_group_ws, " ", unique(column_group)[i])
        }
        
        column_group_ws[which(column_group_ws %in% column_grouped_name)] <- " "
        
        column_grouped_width_t <- c(column_grouped_width_t, column_grouped_width)
        column_group_ws_t <- c(column_group_ws_t, column_group_ws)
      }
      
      column_group_border_t <- ifelse(column_group_ws_t == " " , '', 'single')
      
      if(TRUE) {
        column_group_ws <- column_group_ws_t
        column_grouped_width <- column_grouped_width_t
        column_group_border <- column_group_border_t
        
        column_name_final <- paste0(c(col_names[1], rep(column_grouped_name, length(subgroup_order))), collapse = " | ")
        column_name_align <- c("l", rep("c", length(column_group)*length(subgroup_order)))
      }
    }
    
    
    
    if(subtype != 'same') {
      column_grouped_width <- NULL
      col_rel_width_g <- col_rel_width[-1]
      column_group_ws <- NULL
      
      for (i in 1:length(unique(column_group))) {
        column_grouped_width <- c(column_grouped_width, 0.001, sum(col_rel_width_g[which(column_group == unique(column_group)[i])])-0.001)
        column_group_ws <- c(column_group_ws, " ", unique(column_group)[i])
      }
      
      column_grouped_width <- c(col_rel_width[1], column_grouped_width)
      column_group_ws <- c(" ", column_group_ws)
      column_group_ws[which(column_group_ws %in% column_grouped_name)] <- " "
      column_group_border <- ifelse(column_group_ws == " " , '', 'single')
      
      column_name_final <- paste0(c(col_names[1], column_grouped_name), collapse = " | ")
      column_name_align <- c("l", rep("c", length(column_group)))
    }
    
  }
  
  
  
  # Get column names
  cols <- strsplit(colnames(tbl), ';') %>% 
    lapply(function(x) x[1]) %>% unlist() %>% .[2:ncol(tbl)]
  row_colnames <- paste0(c(col_names[1], cols), collapse = " | ")
  
  # Get bigN then remove it from df
  bigN <- paste0('(N=', tbl[1,-1], ')')
  row_bigN <- paste0(c(col_names[2], bigN), collapse = " | ")
  tbl <- tbl %>% filter(row_number() != 1)
  
  # Get n(%)
  npct <- rep("n (%)", ncol(tbl)-1)
  row_npct <- paste0(c(col_names[3], npct), collapse = " | ")
  
  
  # width of 1st RTF column, in characters
  col1_width <- round(-2.58095 + 
                        0.02954*(length(col_rel_width)-1) + 
                        113.00973*(col_rel_width[1]/sum(col_rel_width)))
  
  # str_wrap
  tbl <- tbl %>% 
    mutate(name = case_when(
      startsWith(name, "    ") ~ name %>%
        trimws() %>% 
        stringr::str_wrap(width = col1_width, 
                          exdent = 4,
                          indent = 4),
      startsWith(name, "  ") ~ name %>%
        trimws() %>% 
        stringr::str_wrap(width = col1_width, 
                          exdent = 2,
                          indent = 2),
      TRUE ~ name %>% 
        stringr::str_wrap(width = col1_width, 
                          exdent = 0,
                          indent = 0)
    ) %>% line_break())
  
  
  # Create RTF table
  rtf <- tbl %>% 
    rtf_page(
      orientation = "landscape",
      margin = c(1, 1, 1, 1, 1, 1),
      nrow = page_nrow,
      border_first = page_border_first,
      border_last = ''
    ) %>%
    rtf_title(
      title = tlf_number,
      subtitle = paste0(title_text, " \n", subtitle_text),
      text_font = 9,
      text_font_size = 9
    ) %>%
    rtf_page_header(
      text = paste0(
        '\\pard \\tqr \\tx13000 ', 
        'ModernaTX, Inc.', 
        ' \\tab Page \\chpgn  of {\\field{\\*\\fldinst  {\\fs18 NUMPAGES }}} \\par',
        '\\pard \\tqr \\tx13000 ',
        studyID,
        ' \\tab ',
        page_header,
        ' \\par'
      ),
      text_font = 9,
      text_font_size = 9,
      text_justification = c("l")
    ) %>%
    rtf_colheader(
      title_note,
      col_rel_width = sum(col_rel_width),
      text_justification = "l",
      text_font = 9,
      border_top = NULL,
      border_left = NULL,
      border_right = NULL,
      border_bottom = 'single'
    ) %>%
    
    {if(subtype == 'same') {
      rtf_colheader(
        .,
        paste0(c(subgroup_label, subgroup_header_ws), collapse = " | "),
        col_rel_width = c(col_rel_width[1], sub_cel_width_ws),
        text_justification = c("l", rep("c", length(subgroup_header_ws))),
        text_font = 9,
        border_left = NULL,
        border_right = NULL,
        border_bottom = c('', sub_bottom_border)
      )
    }else .} %>% 
    
    {if( !is.null(column_group) | !is.null(column_grouped_name)) {
      rtf_colheader(
        .,
        paste0(column_group_ws, collapse = " | "),
        col_rel_width = column_grouped_width,
        text_justification = "c",
        text_font = 9,
        border_top = if(subtype == 'same') NULL else page_border_first,
        border_left = NULL,
        border_right = NULL,
        border_bottom = column_group_border
      ) %>%
        rtf_colheader(
          column_name_final,
          col_rel_width = col_rel_width,
          text_justification = column_name_align,
          text_indent_left = c(txt_ind_left[1], rep(0, ncol(tbl)-1)),
          text_font = 9,
          border_top = NULL,
          border_left = NULL,
          border_right = NULL
        )
    }else {
      
      rtf_colheader(
        .,
        row_colnames,
        col_rel_width = col_rel_width,
        text_justification = c("l", rep("c", ncol(tbl)-1)),
        text_indent_left = c(txt_ind_left[1], rep(0, ncol(tbl)-1)),
        text_font = 9,
        border_top = if(subtype == 'same') NULL else page_border_first,
        border_left = NULL,
        border_right = NULL
      )
      
    }} %>% 
    
    rtf_colheader(
      row_bigN,
      col_rel_width = col_rel_width,
      text_justification = c("l", rep("c", ncol(tbl)-1)),
      text_indent_left = c(txt_ind_left[2], rep(0, ncol(tbl)-1)),
      text_font = 9,
      border_top = NULL,
      border_left = NULL,
      border_right = NULL
    ) %>%
    rtf_colheader(
      row_npct,
      col_rel_width = col_rel_width,
      text_justification = c("l", rep("c", ncol(tbl)-1)),
      text_indent_left = c(txt_ind_left[3], rep(0, ncol(tbl)-1)),
      text_font = 9,
      border_top = NULL,
      border_left = NULL,
      border_right = NULL
    ) %>%
    rtf_body(
      col_rel_width = col_rel_width,
      text_justification = c("l", rep("c", ncol(tbl)-1)),
      text_font = 9,
      border_left = NULL,
      border_right = NULL
    ) %>% 
    rtf_page_footer(
      paste0(page_note, 
             '\\line Program Path: ', 
             paste0(program_loc, ' RunTime: ',
                    toupper(format(Sys.time(), "%d%b%Y %H:%M:%S %Z")) )),
      text_font = 9,
      text_font_size = 9,
      text_justification = 'l',
      text_convert = FALSE
    ) %>%
    rtf_footnote(
      foot_note,
      border_left = "",
      border_right = "",
      border_top = "",
      border_bottom = "",
      text_font = 9,
      text_font_size = 9,
      text_justification = 'l'
    )
  
  return(rtf)
  
}
