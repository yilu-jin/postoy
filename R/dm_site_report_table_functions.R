#' Title
#'
#' @inheritParams toy::dm_site_core
#' @param topleft_label A character vector of top-left column headers.
#' @param subgroup_var A string representing the variable name for subject ID.
#' @param subgroup_order A character vector of sub groups in the desired order.
#' @param subgroup_label A string representing the label for sub groups.
#' @param subgroup_by_col A logical parameter indicating whether to include subgroups on same page.
#' @param rtf A logical parameter indicating whether to write an RTF file.
#' @param col_rel_width A numeric vector of column widths.
#' @param column_order A character vector of column order.
#' @param column_group A character vector of grouped column names on first row.
#' @param column_grouped_name A character vector of customized column names on second row.
#' @param tlf_number A string representing the number of table/listing/figure.
#' @param title_text A string representing the title.
#' @param subtitle_text A string representing the sub title.
#' @param studyID A string representing the study ID.
#' @param page_header A string representing the page header.
#' @param page_note A string representing the page note.
#' @param page_nrow A numeric representing the number of rows per page.
#' @param title_note A string representing the title note.
#' @param foot_note A string representing the foot note.
#' @param program_loc A string representing the program location.
#' @param output_file A string representing the path to the output file.
#'
#' @return A list containing three data frames: one named report.table,
#' one named report.table.n for count results,
#' and the other named report.table.p for percentage results.
#' @importFrom toy dm_site_core
#' @import dplyr, r2rtf
#' @export
#'
#' @examples
#' # Define parameters
#' tlf_number <- "Table 1.2"
#' title_text <- "Number of Participants Randomized by Site"
#' subtitle_text <- "Randomized Set"
#' studyID <- "<Study ID>"
#' page_header <- "<Page Header>"
#' page_note <- "<Page Note>"
#' page_nrow <- 30
#' title_note <- "<Title Note>"
#' foot_note <- "<Foot Note>"
#'
#' # Call the function
#' dm_site_tbl(
#'   population_from = toy::demo_adsl,
#'   country_var = "COUNTRY",
#'   topleft_label = c("Country", "Site"),
#'   # subgroup_var = "SEX",
#'   # subgroup_by_col = TRUE,
#'   rtf = FALSE,
#'   tlf_number = tlf_number,
#'   title_text = title_text,
#'   subtitle_text = subtitle_text,
#'   studyID = studyID,
#'   page_header = page_header,
#'   page_note = page_note,
#'   page_nrow = page_nrow,
#'   title_note = title_note,
#'   foot_note = foot_note
#' )
dm_site_tbl <- function(
    population_from,
    population_where = NULL,
    idvar = "USUBJID",
    treatment_var = "TRT01P",
    treatment_order = NULL,
    site_var = "SITEID",
    country_var = NULL,
    level_col_name = FALSE,
    display_total_list = NULL,
    topleft_label = c("","Site"),
    subgroup_var = NULL,
    subgroup_order = NULL,
    subgroup_label = NULL,
    subgroup_by_col = FALSE,
    rtf = TRUE,
    col_rel_width = NULL,
    column_order = NULL,
    column_group = NULL,
    column_grouped_name = NULL,
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
    output_file = NULL
){

  ## step 0: validate parameters and set defaults
  checkmate::assert_logical(rtf)
  txt_ind_left <- if(is.null(country_var)) rep(0, 3) else c(0, 0, 200)
  topleft_label <- if(length(topleft_label)==1) c("",topleft_label) else topleft_label

  if( (!is.null(column_group) | !is.null(column_grouped_name)) &&
      is.null(treatment_order) && is.null(column_order)) {
    stop('Either argument, treatment_order or column_order should be supplied to implement the spanner column label. ')
  }

  if(is.null(column_order) && !is.null(treatment_order)) {
    column_order <- c(treatment_order, names(display_total_list))
  }


  if(is.null(output_file)) {
    tlf_nm <- trimws(gsub('table', '', tolower(tlf_number)))
    output_file <- paste0('t_', gsub('\\.', '_', trimws(tlf_nm)), '.rtf')
  }else {
    if (!is.character(output_file)) output_file <- as.character(output_file)
    if (!grepl('.rtf', output_file)) output_file <- paste0(output_file, '.rtf')
  }


  # Define subgroup prototype
  subType <-  dplyr::case_when(
    is.null(subgroup_var) ~ 'no',
    isFALSE(subgroup_by_col) ~ 'multi',
    TRUE ~ 'same'
  )


  if(subType != 'no') {

    if(!is.null(subgroup_var) &&
       !subgroup_var %in% names(population_from)) {
      stop("", paste0(subgroup_var), " not found in the population_from dataset.")
    }

    if(!is.null(subgroup_var) && is.null(subgroup_order)) {
      subgroup_order <- unique(population_from[[subgroup_var]])
      subgroup_order <- rmna(subgroup_order)
    }

    if(!is.null(subgroup_var) && !is.null(subgroup_order) &&
       !any(unique(subgroup_order) %in% unique(population_from[[subgroup_var]])) ) {
      stop("The argument subgroup_order not match with the input dataset. ")
    }

    if(!is.null(subgroup_var) && is.null(subgroup_label)) {
      subgroup_label <- subgroup_var
    }


    subgroup_filter <- paste0(subgroup_var, " == '", subgroup_order, "'")
    names(subgroup_filter) <- subgroup_order
    names(subgroup_order) <- subgroup_order

  }

  if(subType == 'no') {
    subgroup_order <- 'all'
    subgroup_label <- NULL
    subgroup_filter <- NULL
    if(!is.null(subgroup_filter)) names(subgroup_filter) <- subgroup_order
    names(subgroup_order) <- subgroup_order
    subgroup_by_col <- FALSE
  }

  message('check pass!')


  ## step 1: generate tables using core function
  vars <- c('name', country_var, site_var) # naming variables
  subg_ord_nmd <- purrr::set_names(subgroup_order) # named subgroup order

  # get n% for subgroups
  lst <-
    subg_ord_nmd %>%
    purrr::map(function(this_grp) {
      dm_site_core(
        population_from = population_from,
        population_where = c(population_where, subgroup_filter[this_grp]),
        idvar = idvar,
        treatment_var = treatment_var,
        treatment_order = treatment_order,
        site_var = site_var,
        country_var = country_var,
        level_col_name = TRUE,
        display_total_list = display_total_list
      )
    })

  # paste n & % for subgroups
  lst_combo <- lst %>%
    purrr::map(~ pst2df(
      .x$report.table.n %>% drop_cols(vars) %>% mutate_all(~na2zero(.x)),
      .x$report.table.p %>% drop_cols(vars) %>% mutate_all(~pfmt(.x))
    ) %>%
      dplyr::bind_cols(.x$report.table.n %>% keep_cols(vars)) %>%
      keep_cols(vars, everything())
    )

  message('preprocess done!')


  ## step 2: combine data frames generated in step 1 to one report table
  if(is.null(column_order)) {
    column_order <- lst_combo %>%
      purrr::map(~.x %>% drop_cols(vars) %>% colnames()) %>%
      unlist() %>% unique()
  }

  tbl <- lst_combo %>% purrr::map(
    ~ .x %>% add_ord() %>% keep_cols(vars, ord, any_of(column_order))
  )

  # Create final table per following scenarios:
  # 1) If multiple subgroups on different page or no subgroup, no change.
  # 2) If multiple subgroups fit into one page, collapse subgroups as `trt;subgroup`.

  # subType %in% c('no', 'multi')
  ftbl <- tbl

  if(subType == 'same') {

    ftbl <- seq_along(tbl) %>%
      purrr::map( function(i) {
        df <- tbl[[i]]
        df1 <- df %>% keep_cols(vars, ord)
        df2 <- df %>% drop_cols(vars, -ord)
        names(df2) <- paste0(names(df2),';',names(tbl)[i])
        df <- dplyr::bind_cols(df1, df2)
        return(df)
      }) %>%
      purrr::reduce(full_join) %>%
      suppressMessages()

    ftbl1 <- ftbl %>% keep_cols(vars, ord)
    ftbl2 <- ftbl %>% drop_cols(vars, -ord)
    ftbl21 <- ftbl2 %>% filter(row_number() == 1)
    ftbl22 <- ftbl2 %>% filter(row_number() != 1)
    ftbl22 <- ftbl22 %>% dplyr::mutate_all(~na2zero(.x))
    ftbl2 <- bind_rows(ftbl21, ftbl22)
    ftbl <- list(bind_cols(ftbl1, ftbl2))

  }

  final.table <-
    ftbl %>%
    purrr::map(~
                 .x %>%
                 group_split(!!!rlang::syms(c('ord', country_var))) %>%
                 purrr::map_dfr(~ .x %>% add_row(ord = NA)) %>% # add blank rows between groups
                 drop_cols(c(country_var, site_var), -ord) %>%
                 dplyr::mutate_all(~ na2blank(.x)) #%>%
               # dplyr::filter(row_number() != n()) # remove extra empty row
    )

  message('final table generated!')




  ## step 3: output in rtf file
  rtf_tbl <-
    seq_along(final.table) %>%
    purrr::map(
      function(s)

        export_as_rtf(
          tbl = final.table[[s]],
          col_rel_width = col_rel_width,
          tlf_number = tlf_number,
          title_text = title_text,
          subtitle_text = subtitle_text,
          studyID = studyID,
          page_header = page_header,
          page_note = page_note,
          page_nrow = page_nrow,
          title_note =
            if(subType %in% c('no', 'same')) title_note
          else if(is.null(title_note)) paste0(subgroup_label, ': ', subgroup_order[s])
          else paste0(title_note, '\n', subgroup_label, ': ', subgroup_order[s]),
          foot_note = if(s<length(final.table)) NULL else foot_note,
          program_loc = program_loc,
          subtype = subType,
          subgroup_order = subgroup_order,
          subgroup_label = subgroup_label,
          column_order = column_order,
          column_group = column_group,
          column_grouped_name = column_grouped_name,
          col_names = c('', topleft_label),
          txt_ind_left = txt_ind_left
        )
    )

  if (rtf) {

    # Output multiple RTF files
    temp_files <- replicate(length(rtf_tbl), tempfile(fileext = '.rtf'))

    seq_along(rtf_tbl) %>%
      purrr::map(
        function(s)
          rtf_tbl[[s]] %>%
          r2rtf::rtf_encode() %>%
          r2rtf::write_rtf(temp_files[s])
      )

    # Assemble multiple RTF files
    rtf_assemble1(temp_files) %>%
      r2rtf::write_rtf(output_file)

    purrr::walk(temp_files, file.remove)

    message('rtf outputted!')

  }

  ## step 4: Get n & p tables

  ntable <- lst %>% purrr::map( ~ .x$report.table.n)
  ptable <- lst %>% purrr::map( ~ .x$report.table.p)

  # Drop level name columns if FALSE
  if(isFALSE(level_col_name)) {
    ntable <- ntable %>% purrr::map(~ .x %>% drop_cols(c(country_var, site_var)))
    ptable <- ptable %>% purrr::map(~ .x %>% drop_cols(c(country_var, site_var)))
  }

  return(list(report.table = final.table,
              report.table.n = ntable,
              report.table.p = ptable))

}## End of dm_site_tbl function



