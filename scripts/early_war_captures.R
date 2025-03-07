

ukraineconflict::read_enriched_oryx_data() %>%
  dplyr::filter(
    country == "Russia",
    date <= lubridate::dmy(10042022)) %>%
  dplyr::group_by(class, status) %>%
  dplyr::summarise(
    status_n = dplyr::n(),
    .groups = "drop") %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(class_n = sum(status_n)) %>%
  dplyr::arrange(dplyr::desc(class_n)) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = forcats::fct_reorder(class, -class_n),
      fill = status,
      y = status_n)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Perdas russas de equipamento, 22/02/2022 - 10/04/2022",
    x = "Classe de Veículo",
    y = "",
    fill = "Tipo de Perda",
    caption = "Fonte: Oryx Database")
