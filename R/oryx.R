
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
    glue::glue() %>%
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
    dplyr::relocate(class, .after = system)
  
}






