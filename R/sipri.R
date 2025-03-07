
#' @name get_raw_milex
#' @export
#' 

get_milex_pct_gdp <- function() {
  
  readxl::read_xlsx(
    "data/sipri-milex/raw_milex.xlsx",
    sheet = "Share of GDP",
    skip = 5) %>%
    dplyr::select(-Notes) %>%
    dplyr::rename(country = Country) %>%
    dplyr::group_by(country) %>%
    dplyr::group_nest() %>%
    dplyr::mutate(
      data = purrr::map(
        data,
        ~ tidyr::pivot_longer(
            .x,
            cols = tidyselect::everything()))) %>%
    tidyr::unnest(data) %>%
    dplyr::rename(
      year = name,
      pct_gdp = value) %>%
    dplyr::mutate(
      pct_gdp = dplyr::if_else(
        pct_gdp == "...",
        NA,
        pct_gdp)) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      pct_gdp = pct_gdp %>%
        stringr::str_sub(1, 4) %>%
        stringr::str_replace("E", "") %>%
        readr::parse_double())
  
  # se foder porra eles não têm a série que eu queria
    
}
