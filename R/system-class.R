
#' @export
#' @name is_tank

is_tank <- function(system) {

  c(
    "T-80",
    "T-72",
    "T-55",
    "T-64",
    "T-62",
    "T-90",
    "Leopard",
    "Challenger",
    "Abrams",
    "tank") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}


#' @export
#' @name is_ifv

is_ifv <- function(system) {

  c(
    "BMP-",
    "CV-90",
    "Bradley") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}

#' @export
#' @name is_apc

is_apc <- function(system) {

  c(
    "BMD-",
    "BTR-",
    "MT-LB",
    "M113",
    "APC") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}


#' @export
#' @name is_artillery

is_artillery <- function(system) {

  c(
    "M777",
    "HIMARS",
    "Himars",
    "howitzer",
    "mortar",
    "Akatsiya",
    "Giatsint",
    "Malka",
    "BM-",
    "155mm",
    "152mm",
    "120mm",
    "122mm",
    "203mm",
    "207mm") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}

#' @export
#' @name is_radar

is_radar <- function(system) {

  c(
    "-band",
    "radar",
    "Zoopark") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}

#' @export
#' @name is_drone
#'

is_drone <- function(system) {

  c(
    "Orlan",
    "Lancet",
    "Switchblade",
    "drone") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}

#' @export
#' @name is_aa
#'

is_aa <- function(system) {

  c(
    "Patriot",
    "Iris-T",
    "NASAMs",
    "2K",
    "Tor",
    "S-300",
    "S-400",
    "Pantsir",
    "Buk",
    "Hawk") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}

#' @export
#' @name is_bukhanka
#'

is_bukhanka <- function(system) {

  c(
    "van",
    "UAZ-452") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}

#' @export
#' @name is_motorized
#'

is_motorized <- function(system) {

  c(
    "Ural",
    "KamAZ",
    "GAZ",
    "ZiL",
    "HMMV",
    "UAZ-469",
    "truck") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}

#' @export
#' @name is_plane
#'

is_plane <- function(system) {

  c(
    "Su-",
    "MiG-",
    "Be-",
    "Tu-") %>%
    purrr::some(~ stringr::str_detect(system, .x))

}


#' @export
#' @name system_class
#'

system_class <- function(system) {

  dplyr::case_when(
    ukraineconflict::is_apc(system) ~ "APC",
    ukraineconflict::is_tank(system) ~ "Tank",
    ukraineconflict::is_ifv(system) ~ "IFV",
    ukraineconflict::is_artillery(system) ~ "Artillery",
    ukraineconflict::is_radar(system) ~ "Radar",
    ukraineconflict::is_drone(system) ~ "Drone",
    ukraineconflict::is_aa(system) ~ "AA",
    ukraineconflict::is_bukhanka(system) ~ "Bukhanka",
    ukraineconflict::is_motorized(system) ~ "Military Jeep/Motorized",
    ukraineconflict::is_plane(system) ~ "Plane",
    TRUE ~ "Other") ->
    output

  logger::log_debug(
    "Classifying {system} as {output}.")

  output

}


#' @export
#' @name validate_classification
#'

validate_classification <- function(
    data = ukraineconflict::oryx_data()) {

  tibble::tibble(
    system = data %>%
      dplyr::pull(system) %>%
      unique()) %>%
    dplyr::mutate(
      class = purrr::map_chr(
        system,
        ukraineconflict::system_class)) %>%
    dplyr::arrange(class)

}
