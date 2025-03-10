#' @export
#' @name war_phase

war_phase <- function(timestamp) {

  dplyr::case_when(
    lubridate::date(timestamp) <= lubridate::dmy("10-04-2022") ~
      "Early Days",
    lubridate::date(timestamp) <= lubridate::dmy("30-08-2022") ~
      "First Donbass Campaign",
    lubridate::date(timestamp) <= lubridate::dmy("11-11-2022") ~
      "2022 Ukranian Counteroffensives",
    lubridate::date(timestamp) <= lubridate::dmy("07-06-2023") ~
      "First Stalemate",
    lubridate::date(timestamp) <= lubridate::dmy("30-11-2023") ~
      "2023 Ukranian Counteroffensives",
    lubridate::date(timestamp) <= lubridate::today() ~
      "Second Stalemate") ->
    output

  logger::log_info("{timestamp} belongs to {output}.")

  output

}
