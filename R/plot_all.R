#' Plot Episodes of State Ownership Transformation (ESOT) over time.
#'
#' `plot_all` plots the global number/share of countries undergoing
#'  privatization and statization Episodes of State Ownership Transformation (ESOT)
#'  in a selected time frame.
#'
#' This function is a wrapper for [ggplot2:ggplot()] and produces a plot that shows
#' privatization and statization episodes over time.
#' The function calls the [ESOT:get_eps()] function to identify episodes.
#'
#'#' @param abs Logical value: if TRUE, the absolute number of countries in an episode for each year is plotted.
#' If FALSE, the share of countries (in \%) undergoing privatization or statization is plotted.
#'
#' @param years Vector with two numeric values indicating the minimum and maximum year to be plotted.
#'
#' @param start_incl What is the minimum annual change on v2clstown_osp necessary to trigger an episode?
#' Default is 0.04.
#'
#' @param cum_incl What is the minimum amount of total change necessary to constitute a manifest episode?
#' Default is 0.4.
#'
#' @param year_turn What is the amount of annual change in the opposite direction to trigger the termination of an episode?
#' Default is 0.12.
#'
#' @param cum_turn What is the amount of gradual change in the opposite direction to trigger the termination of an episode?
#' Default is 0.4.
#'
#' @param tolerance What is the number of years considered as tolerance for stasis or a gradual movement in the opposite direction?
#' Default is 5 years.
#'
#' @param lang Language for labels. Default is "en" (English).
#' @return The output of this function is a [ggplot2:ggplot()] object with the number/share of episodes per year.

#' @import ggplot2
#' @import dplyr
#'
#' @export

localizations <- list(
  en = list(
    year = "Year",
    statization = "Statization",
    privatization = "Privatization",
    number_countries = "Number of Countries",
    countries_percent = "Countries (%)",
    no_data = "Error: Data not available for time range",
    no_episodes = "No episodes during selected period."
  ),
  es = list(
    year = "Año",
    statization = "Estatización",
    privatization = "Privatización",
    number_countries = "Número de países",
    countries_percent = "Países (%)",
    no_data = "Error: Datos no disponibles para el rango de tiempo",
    no_episodes = "No hay episodios durante el período seleccionado."
  )
)

get_label <- function(key, lang = "es") {
  if (!lang %in% names(localizations)) lang <- "en"
  label <- localizations[[lang]][[key]]
  if (is.null(label)) return(key)
  label
}

plot_all <- function(abs = T,
                     years = c(1900, 2023),
                     start_incl  = 0.04,
                     cum_incl  = 0.4,
                     year_turn = 0.12,
                     cum_turn = 0.4,
                     tolerance = 5,
                     lang = "en") {

  eps <- ESOT::get_eps(data = ESOT::vdem,
                      start_incl = start_incl,
                      cum_incl = cum_incl,
                      year_turn = year_turn,
                      cum_turn = cum_turn,
                      tolerance = tolerance)


  stopifnot(is.logical(abs), length(abs) == 1)

  stopifnot(is.numeric(years), length(years) == 2, years[2] > years[1])

  stopifnot(is.numeric(start_incl), length(start_incl) == 1)

  stopifnot(is.numeric(cum_incl), length(cum_incl) == 1)

  stopifnot(is.numeric(year_turn), length(year_turn) == 1)

  stopifnot(is.numeric(cum_turn), length(cum_turn) == 1)

  stopifnot(is.numeric(tolerance), length(tolerance) == 1)

  if(min(years)<min(ESOT::vdem$year) | max(years)>max(ESOT::vdem$year))
    get_label("no_data", lang)

  if (isTRUE(abs)) {
    eps_year <- eps %>%
      dplyr::filter(between(year, min(years), max(years))) %>%
      {if(nrow(.) == 0) stop(get_label("no_episodes", lang)) else .} %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(priv_eps = sum(priv_ep),
                       stat_eps = sum(stat_ep)) %>%
      tidyr::pivot_longer(cols = c(priv_eps, stat_eps), names_to = "ep_type", values_to = "countries")

  } else {
    eps_year <- eps %>%
      dplyr::filter(between(year, min(years), max(years))) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(priv_eps = sum(priv_ep) / length(unique(country_id)),
                       stat_eps = sum(stat_ep) / length(unique(country_id))) %>%
      tidyr::pivot_longer(cols = c(priv_eps, stat_eps), names_to = "ep_type", values_to = "countries")
  }

  p <-  ggplot2::ggplot(data = eps_year, aes(x = year, y = countries, group = ep_type, linetype = ep_type)) +
    geom_line() +
    scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
    scale_linetype(name = "", breaks = c("stat_eps", "priv_eps"), labels = c(get_label("statization", lang), get_label("privatization", lang))) +
    xlab(get_label("year", lang)) +
    theme_classic() +
    theme(legend.position = "bottom")

  if (isTRUE(abs)) {
    p +  ylab(get_label("number_countries", lang))
  }  else {
    p +  ylab(get_label("countries_percent", lang))
  }
}
