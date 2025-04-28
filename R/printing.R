# library(checkmate)
# library(dplyr)
# library(glue)
# library(googlesheets4)
# library(purrr)
# library(tidyr)

#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data.
#'   This can be either a URL to a google sheet with multiple sheets
#'   containing the four data types or a path to a folder containing
#'   four `.csv`s with the neccesary data.
#' @param sheet_is_publicly_readable If you're using google sheets for data,
#'   is the sheet publicly available? (Makes authorization easier).
#'
#' @return A new `CV_Printer` object.
#'
#' @noRd
create_cv_object <-  function(
    data_location, #nolint
    sheet_is_publicly_readable = TRUE
  ) {
  checkmate::assert_string(data_location)
  checkmate::assert_flag(sheet_is_publicly_readable)

  cv <- list()

  if (isTRUE(sheet_is_publicly_readable)) {
    googlesheets4::gs4_deauth()
  } else {
    options(gargle_oauth_cache = ".secrets")
  }

  read_gsheet <- function(sheet_id){
    googlesheets4::read_sheet(
      data_location,
      sheet = sheet_id,
      skip = 1,
      col_types = "c"
    )
  }

  cv$entries <- read_gsheet(sheet_id = "entries")
  cv$language_skills <- read_gsheet(sheet_id = "language_skills")
  cv$text_blocks <- read_gsheet(sheet_id = "text_blocks")
  cv$contact_info <- read_gsheet(sheet_id = "contact_info")


  extract_year <- function(dates){
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <-
      lubridate::year(lubridate::ymd(Sys.Date())) + 10

    date_year
  }

  parse_dates <- function(dates) {
    date_month <- stringr::str_extract(
      dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
    date_month[is.na(date_month)] <- "1"

    paste("1", date_month, extract_year(dates), sep = "-") |>
      lubridate::dmy()
  }

  cv$entries <-
    cv$entries |>
    dplyr::filter(in_resume == "TRUE") |>
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) |>
    dplyr::mutate(
      description_bullets = ifelse(
        description_bullets != "",
        paste0("- ", description_bullets),
        ""
        ),
      start = ifelse(start == "NULL", NA, start),
      end = ifelse(end == "NULL", NA, end),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ "N/A",
        no_start  & has_end ~ as.character(end),
        has_start & no_end  ~ paste("Current", "-", start),
        TRUE                ~ paste(end, "-", start)
      )
    ) |>
    dplyr::arrange(desc(parse_dates(end))) |>
    dplyr::mutate(dplyr::across(.fns = ~ ifelse(is.na(.x), 'N/A', .x)))

  cv
}

#' Print a CV entry
#'
#' @description
#'
#' Take a position data frame and the section id desired and prints the section
#' to markdown.
#'
#' @param section_id ID of the entries section to be printed as encoded by the
#'   `section` column of the `entries` table
#'
#' @noRd
print_section <- function(cv, section_id, glue_template = "default") {
  checkmate::assert_list(cv)
  checkmate::assert_string(section_id)

  if (glue_template == "default") {
    glue_template <-
    "
    ### {title}

    {loc}

    {institution}

    {timeline}

    {description_bullets}
    \n\n\n
    "
  }

  section_data <- dplyr::filter(cv$entries, section == section_id)

  print(glue::glue_data(section_data, glue_template))

  invisible(cv)
}

#' Prints out text block identified by a given label.
#'
#' @param label ID of the text block to print as encoded in `label` column of
#'   `text_blocks` table.
#'
#' @noRd
print_text_block <- function(cv, label){
  text_block <-
    dplyr::filter(cv$text_blocks, loc == label) |>
    dplyr::pull(text)

  cat(text_block)

  invisible(cv)
}

#' Construct a bar chart of skills
#'
#' @param out_of The relative maximum for skills. Used to set what a fully
#'   filled in skill bar is.
#'
#' @noRd
print_skill_bars <- function(
    cv, #nolint
    out_of = 5,
    bar_color = "#969696",
    bar_background = "#d9d9d9",
    glue_template = "default"
  ) {
  if (glue_template == "default") {
    glue_template <- "
<div
  class = 'skill-bar'
  style = \"background:linear-gradient(to right,
                                      {bar_color} {width_percent}%,
                                      {bar_background} {width_percent}% 100%)\"
>{skill}</div>"
  }

  cv$language_skills |>
    dplyr::mutate(width_percent = round(100*as.numeric(level)/out_of)) |>
    glue::glue_data(glue_template) |>
    print()

  invisible(cv)
}

#' List of all links in document labeled by their superscript integer.
#'
#' @param cv a [`list`][list()] with the CV content.
#'
#' @noRd
print_links <- function(cv) {
  n_links <- length(cv$links)

  if (n_links > 0) {
    cat(
      "
      ## Links {data-icon=link}

      <br>
      "
    )

    purrr::walk2(
      cv$links,
      1:n_links,
      function(link, index) {print(glue::glue('{index}. {link}'))}
    )
  }

  invisible(cv)
}

#' Contact information section with icons
#'
#' @noRd
print_contact_info <- function(cv) {
  glue::glue_data(
    cv$contact_info,
    "<p>",
    "<i class='{icon}'></i> {contact}",
    "</p>"
  ) |>
    print()

  invisible(cv)
}
