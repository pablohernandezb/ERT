#' Find the overlap between episodes of privatization and statization.
#'
#' @param episodes The outcome of get_eps(), Episodes of State Ownership Transformation (ESOT),
#' to be used for finding potential overlaps (depending on individual parameter setting). By default with standard parameters.
#'
#' @return A summary message and data frame showing the overlaps between privatization and statization episodes.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @export
#'
#' @examples
#' #Don't run
#' #Find the overlap between privatization and statization episodes
#'
#' #overlap <- find_overlap(episodes)
#'
find_overlap <-function(start_incl  = 0.04,
                        cum_incl  = 0.4,
                        year_turn = 0.12,
                        cum_turn = 0.4,
                        tolerance = 5)
{

  episodes = ESOT::get_eps(data = ESOT::vdem,
                          start_incl = start_incl,
                          cum_incl = cum_incl,
                          year_turn = year_turn,
                          cum_turn = cum_turn,
                          tolerance = tolerance)

  stat_ep <- country_name <- year <- priv_ep <- country_text_id <- stat_ep_id <- priv_ep_id <- year_diff <-
    conseq_episode <- overlap_counter <- overlap_period <- NULL

  # Find overlapping episodes
  merged <- episodes
  stat <- merged %>% filter(stat_ep == 1) %>% dplyr::select(country_name, country_text_id, year, stat_ep_id, priv_ep_id)
  priv <- merged  %>% filter(priv_ep == 1) %>% dplyr::select(country_name, country_text_id, year, stat_ep_id, priv_ep_id)
  overlap1 <- rbind(stat, priv)[duplicated(rbind(stat, priv)),]

  # Stop if there are no overlapping episodes
  if(nrow(overlap1) == 0) {
   print("No overlapping episodes in the data.")
  }

  if(nrow(overlap1) > 0) {

  # Identify sequences of overlapping years
  overlap <- overlap1 %>%
    arrange(country_name, year) %>%
    group_by(country_name) %>%
    mutate(year_diff = year - dplyr::lag(year),
           conseq_episode = ifelse(dplyr::lead(as.numeric(year_diff)) < 2, 0, 1)) %>%
    ungroup()  %>%
    mutate(conseq_episode = ifelse(is.na(conseq_episode), 1, conseq_episode),
           overlap_counter = dplyr::lag(cumsum(conseq_episode == 1L)),
           overlap_counter = ifelse(is.na(overlap_counter), 0, overlap_counter),
           overlap_counter = overlap_counter + 1) %>%
    group_by(overlap_counter) %>%
    mutate(overlap_period = paste0(min(year), "-", max(year))) %>%
    ungroup() %>%
    select(country_name, country_text_id, year, overlap_period, stat_ep_id, priv_ep_id)

  # Inform user about the result
  print(paste0("There are ", length(unique(overlap$overlap_period)), " overlapping episodes (", nrow(overlap), " overlapping country-years) in ",
               length(unique(overlap$country_name)), " countries in the episodes data."))

  return(overlap)
  }
}
