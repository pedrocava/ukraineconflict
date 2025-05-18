

ukraineconflict::read_enriched_oryx_data() %>%
  dplyr::filter(country == "Russia") %>%
  dplyr::mutate(
    ym = lubridate::floor_date(date, unit = "month")) %>%
  dplyr::group_by(ym, class, status) %>%
  dplyr::summarise(
    count = dplyr::n(),
    .groups = "drop") ->
  all_counts

(all_counts %>%
  dplyr::group_by(
    ym,
    class) %>%
  dplyr::summarise(
    count = sum(count),
    .groups = "drop") %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = ym,
      y = count,
      fill = class)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_date(
    date_breaks = "2 month",
    guide = ggplot2::guide_axis(angle = 45),
    date_labels = "%Y-%m") +
  ggplot2::labs(
    title = "Perdas Russas com confirmação fotográfica, por mês e categoria",
    subtitle = glue::glue("Dados atualizadas até {lubridate::today()}"),
    caption = "Fonte: Oryx Database",
    fill = "Classe",
    y = "")) %T>%
  ukraineconflict::save_plot("overall_losses_by_ym_and_class")

(all_counts %>%
  dplyr::group_by(class) %>%
  dplyr::summarise(
    total = sum(count),
    .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(total)) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      y = total,
      x = forcats::fct_reorder(class, -total))) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Perdas Russas com confirmação fotográfica, por categoria",
    subtitle = glue::glue("Dados atualizadas até {lubridate::today()}"),
    caption = "Fonte: Oryx Database",
    x = "Classe",
    y = "")) %T>%
  ukraineconflict::save_plot("overall_losses_by_class")


(ukraineconflict::read_enriched_oryx_data() %>%
  dplyr::filter(country == "Russia", class == "Tank") %>%
  dplyr::mutate(
    year = lubridate::floor_date(date, unit = "year")) %>%
  dplyr::group_by(model, year) %>%
  dplyr::count() %>%
  dplyr::ungroup() %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = year,
      y = n,
      fill = model)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_date(
    date_breaks = "12 month",
    guide = ggplot2::guide_axis(angle = 45),
    date_labels = "%Y") +
  ggplot2::labs(
    title = "Perdas Russas de tanques com confirmação fotográfica, por ano e modelo",
    subtitle = glue::glue("Ressalva: Modelos antigos são continuamente atualizados.
     Por exemplo: um T-72B3 Obr. 2016 é comparável a um T090A.
     Dados atualizadas até {lubridate::today()}"),
    caption = "Fonte: Oryx Database",
    fill = "Modelo",
    y = "")) %T>%
  ukraineconflict::save_plot("total_tank_losses_by_model")


(ukraineconflict::read_enriched_oryx_data() %>%
  dplyr::filter(
    country == "Russia",
    model %in% c("T-55", "T-62", "T-64")) %>%
  dplyr::mutate(
    year = lubridate::floor_date(date, unit = "year")) %>%
  dplyr::group_by(year, model) %>%
  dplyr::count() %>%
  dplyr::ungroup()%>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = year,
      y = n,
      fill = model)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_date(
    date_breaks = "12 month",
    guide = ggplot2::guide_axis(angle = 45),
    date_labels = "%Y") +
  ggplot2::labs(
    title = "Perdas Russas de tanques com confirmação fotográfica, modelos selecionados",
    subtitle = glue::glue("Dados atualizadas até {lubridate::today()}"),
    caption = "Fonte: Oryx Database",
    fill = "Modelo",
    y = "")) %T>%
  ukraineconflict::save_plot("total_tank_losses_selected_models")
