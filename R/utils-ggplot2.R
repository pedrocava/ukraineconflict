

#' @export
#' @name equipment_color_scale

equipment_color_scale <- function() {

  ggplot2::scale_color_manual(
    values = c(
      "destroyed" = "#e33636",
      "damaged" = "#e3d236",
      "abandoned" = "#a436e3",
      "captured" = "#36e376"))

}

#' @export
#' @name equipment_fill_scale

equipment_fill_scale <- function() {

  ggplot2::scale_fill_manual(
    values = c(
      "destroyed" = "#e33636",
      "damaged" = "#e3d236",
      "abandoned" = "#a436e3",
      "captured" = "#36e376"))

}
