
(ukraineconflict::read_enriched_oryx_data() %>%
  dplyr::filter(
    country == "Russia",
    model != "Unknown",
    class %in% c("Tank", "IFV", "Artillery", "AA"),
    status %in% c("captured", "abandoned"),
    date <= lubridate::dmy(10042022)) %>%
  dplyr::group_by(model) %>%
  dplyr::summarise(
    n = dplyr::n(),
    class = min(class),
    .groups = "drop") %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = forcats::fct_reorder(model, -n),
      y = n,
      fill = class)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    x = "",
    subtitle = "22/02/2022 - 10/04/2022",
    y = "",
    fill = "Classe de equipamento",
    title = "Equipamento russo capturado com confirmação fotográfica, por modelo",
    caption = "Fonte: Oryx Database")) %T>%
  ukraineconflict::save_plot("early_war_captures_by_model.png")
