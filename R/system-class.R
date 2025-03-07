
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

#' @export
#' @name

#' @export
#' @name model
model <- function(system) {

  dplyr::case_when(
    # Tanks
    stringr::str_detect(system, "T-80") ~ "T-80",
    stringr::str_detect(system, "T-72") ~ "T-72",
    stringr::str_detect(system, "T-62") ~ "T-62",
    stringr::str_detect(system, "T-64") ~ "T-64",
    stringr::str_detect(system, "T-55") ~ "T-55",
    stringr::str_detect(system, "T-90") ~ "T-90",
    stringr::str_detect(system, "Leopard 2") ~ "Leopard 2",
    stringr::str_detect(system, "Leopard 1") ~ "Leopard 1",
    stringr::str_detect(system, "Challenger") ~ "Challenger",
    stringr::str_detect(system, "Abrams") ~ "Abrams",

    # IFVs
    stringr::str_detect(system, "BMP-") ~ stringr::str_extract(system, "BMP-\\d+"),
    stringr::str_detect(system, "CV-90") ~ "CV-90",
    stringr::str_detect(system, "Bradley") ~ "Bradley",
    stringr::str_detect(system, "Marder") ~ "Marder",

    # APCs
    stringr::str_detect(system, "BMD-") ~ stringr::str_extract(system, "BMD-\\d+"),
    stringr::str_detect(system, "BTR-") ~ stringr::str_extract(system, "BTR-\\d+"),
    stringr::str_detect(system, "MT-LB") ~ "MT-LB",
    stringr::str_detect(system, "M113") ~ "M113",

    # Artillery
    stringr::str_detect(system, "M777") ~ "M777",
    stringr::str_detect(system, "HIMARS|Himars") ~ "HIMARS",
    stringr::str_detect(system, "CAESAR") ~ "CAESAR",
    stringr::str_detect(system, "Akatsiya") ~ "2S3",
    stringr::str_detect(system, "Giatsint") ~ "2S5",
    stringr::str_detect(system, "Malka") ~ "2S7",
    stringr::str_detect(system, "BM-") ~ stringr::str_extract(system, "BM-\\d+"),

    # Radar
    stringr::str_detect(system, "Zoopark") ~ "Zoopark",

    # Drones
    stringr::str_detect(system, "Orlan") ~ "Orlan",
    stringr::str_detect(system, "Lancet") ~ "Lancet",
    stringr::str_detect(system, "Switchblade") ~ "Switchblade",

    # AA
    stringr::str_detect(system, "Iris-T") ~ "Iris-T",
    stringr::str_detect(system, "NASAMs") ~ "NASAMs",
    stringr::str_detect(system, "2K") ~ stringr::str_extract(system, "2K\\d+"),
    stringr::str_detect(system, "Tor") ~ "Tor",
    stringr::str_detect(system, "S-300") ~ "S-300",
    stringr::str_detect(system, "S-400") ~ "S-400",
    stringr::str_detect(system, "Pantsir") ~ "Pantsir",
    stringr::str_detect(system, "Buk") ~ "Buk",
    stringr::str_detect(system, "Hawk") ~ "Hawk",

    # Vehicles
    stringr::str_detect(system, "UAZ-452") ~ "UAZ-452",
    stringr::str_detect(system, "Ural") ~ "Ural",
    stringr::str_detect(system, "KamAZ") ~ "KamAZ",
    stringr::str_detect(system, "GAZ") ~ "GAZ",
    stringr::str_detect(system, "ZiL") ~ "ZiL",
    stringr::str_detect(system, "HMMV") ~ "HMMV",
    stringr::str_detect(system, "UAZ-469") ~ "UAZ-469",

    # Helicopters
    stringr::str_detect(system, "Ka-") ~ stringr::str_extract(system, "Ka-\\d+"),
    stringr::str_detect(system, "Mi-") ~ stringr::str_extract(system, "Mi-\\d+"),

    # Aircraft
    stringr::str_detect(system, "Su-") ~ stringr::str_extract(system, "Su-\\d+"),
    stringr::str_detect(system, "MiG-") ~ stringr::str_extract(system, "MiG-\\d+"),
    stringr::str_detect(system, "Be-") ~ stringr::str_extract(system, "Be-\\d+"),
    stringr::str_detect(system, "Tu-") ~ stringr::str_extract(system, "Tu-\\d+"),

    # Default case
    TRUE ~ "Unknown") ->
    output

  logger::log_info("{system} mapped as {output}")

  output

}
