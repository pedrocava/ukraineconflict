
#' @export
#' @name gen_oryx_url

gen_oryx_url <- function(event) {

  "https://raw.githubusercontent.com/scarnecchia/oryx_data/refs/heads/main/event_{event}.csv" %>%
    glue::glue()

}

#' @export
#' @name get_oryx_events
#'
#'

get_oryx_events <- function(event) {

  logger::log_info("Fetching Oryx {event} data.")

  ukraineconflict::gen_oryx_url(event) %>%
    rio::import() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      date = lubridate::ymd(date_recorded),
      downloaded_at = lubridate::now(),
      system = tidyr::replace_na(system, "Unknown")) %>%
    dplyr::select( -c(date_recorded, url)) %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "ID", "_id"),
      .cols = tidyselect::ends_with("ID"))

}

#' @export
#' @name oryx_data

oryx_data <- function() {

  c("destroyed", "captured", "abandoned", "damaged") %>%
    purrr::map(ukraineconflict::get_oryx_events) %>%
    purrr::reduce(dplyr::bind_rows)

}


#' @export
#' @name enriched_oryx_data

enriched_oryx_data <- function() {

  raw_data <- ukraineconflict::oryx_data()

  logger::log_info("Classifying systems.")

  raw_data %>%
    dplyr::mutate(
      class = purrr::map_chr(
        system,
        ukraineconflict::system_class)) %>%
    dplyr::relocate(class, .after = system) ->
    classified

  logger::log_info("Isolating models from systems.")

  classified %>%
    dplyr::mutate(
      model = purrr::map_chr(
        system,
        ukraineconflict::model)) %>%
    dplyr::relocate(model, .after = class) ->
    modelled

  logger::log_info("Assigning events to war phases.")

  modelled %>%
    dplyr::mutate(
      war_phase = purrr::map_chr(
        date,
        ukraineconflict::war_phase))

}

#' @export
#' @name write_enriched_oryx_data
#'

write_enriched_oryx_data <- function() {

  ukraineconflict::enriched_oryx_data() %>%
    saveRDS("data/clean/classified_oryx_data.Rds")

}


#' @export
#' @name read_enriched_oryx_data
#'

read_enriched_oryx_data <- function() {

  readRDS("data/clean/classified_oryx_data.Rds")

}
