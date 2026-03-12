#' Get episodes of state ownership transformation (ESOT)
#'
#' Identifies episodes of privatization (movement towards market economy) and
#' statization (movement towards state ownership) in the most recent V-Dem data set,
#' using the variable v2clstown_osp (State Ownership of the Economy).
#'
#' \emph{Privatization} is defined as any sustained increase in v2clstown_osp,
#' indicating a movement away from state ownership towards a market economy.
#'
#' \emph{Statization} is defined as any sustained decrease in v2clstown_osp,
#' indicating a movement towards greater state ownership of the economy.
#'
#' The variable v2clstown_osp ranges from 0 to 4, where:
#' \itemize{
#'   \item 0 = High state ownership (planned economy)
#'   \item 4 = Low state ownership (market economy)
#' }
#'
#' @param data The data based on which the episodes are identified.
#' By default the most recent vdem data set.
#'
#' @param start_incl What is the minimum annual change on v2clstown_osp necessary to trigger an episode?
#' This is the absolute value of the first difference required for the onset
#' of either a privatization (+) or statization episode (-).
#' Default is 0.04 (scaled from ERT's 0.01 for the 0-4 range).
#'
#' @param cum_incl What is the minimum amount of total change on v2clstown_osp necessary to constitute a manifest episode?
#' Default is 0.4 (scaled from ERT's 0.1 for the 0-4 range).
#'
#' @param year_turn What is the amount of annual change in the opposite direction to trigger the termination of an episode?
#' Default is 0.12 (scaled from ERT's 0.03 for the 0-4 range).
#'
#' @param cum_turn What is the amount of gradual change in the opposite direction to trigger the termination of an episode?
#' Default is 0.4 (scaled from ERT's 0.1 for the 0-4 range).
#'
#' @param tolerance What is the number of years considered as tolerance for stasis or a gradual movement in the opposite direction?
#' Default is 5 years.
#'
#' @return A data frame specifying episodes of state ownership transformation and their outcomes.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import Rcpp
#' @importFrom hablar s
#' @importFrom plm make.pconsecutive
#' @export
#'
#' @examples
#' #Don't run
#' #Get the episodes with standard parameters:
#' #episodes <- get_eps()
#'
### set the parameters ###
get_eps <- function(data = ESOT::vdem,
                    start_incl = 0.04,
                    cum_incl = 0.4,
                    year_turn = 0.12,
                    cum_turn = 0.4,
                    tolerance = 5)
{

  if(year_turn == 0)
    print("You set year_turn = 0. Did you mean to do this? Doing so means an episode ends when it experiences a year of no annual change on v2clstown_osp. Perhaps, instead, you meant to set its value equal to cum_turn.")


  ### DATA CLEANING AND PREP ###

  # selecting the variables we need to construct the episodes dataframe
  full.df <- data %>%
    dplyr::select(country_name, country_id, country_text_id, year,
                  v2clstown_osp, codingstart, codingend,
                  gapstart1, gapstart2, gapstart3, gapend1, gapend2, gapend3) %>%
    dplyr::filter(year >= 1900) %>%
    dplyr::arrange(country_text_id, year) %>%
    dplyr::group_by(country_id) %>%
    # make codingstart 1900 or first year thereafter
    dplyr::mutate(codingstart2 = min(hablar::s(ifelse(!is.na(v2clstown_osp), year, NA))),
                  # tag original sample for later use
                  origsample = 1) %>%
    # we need to deal with gaps in v-dem coding
    # this balances the dataset
    plm::make.pconsecutive(balanced = TRUE, index = c("country_id", "year")) %>%
    dplyr::group_by(country_id) %>%
    # this fills missing variables we need that are constant within countries
    tidyr::fill(c(country_text_id, country_name, codingend, gapstart1, gapend1, gapstart2, gapend2,
                  gapstart3, gapend3)) %>%
    tidyr::fill(c(country_text_id, country_name, codingend, gapstart1, gapend1, gapstart2, gapend2,
                  gapstart3, gapend3), .direction = "up")  %>%
    # here we need to recode the gaps as only during the period prior to and during the gap
    dplyr::mutate(gapstart = ifelse(year <= gapend1, gapstart1, NA),
                  gapend = ifelse(year <= gapend1, gapend1, NA),
                  gapstart = ifelse(!is.na(gapend2) & year > gapend1 & year <= gapend2, gapstart2, gapstart),
                  gapend = ifelse(!is.na(gapend2) & year > gapend1 & year <= gapend2, gapend2, gapend),
                  gapstart = ifelse(!is.na(gapend3) & year > gapend2 & year <= gapend3, gapstart3, gapstart),
                  gapend = ifelse(!is.na(gapend3) & year > gapend2 & year <= gapend3, gapend3, gapend)) %>%


    ### CODING THE ECONOMY TYPE VARIABLES ###

    # Derive economy type from v2clstown_osp:
    #   0 = Planned economy (v2clstown_osp < 1)
    #   1 = State-dominated mixed economy (1 <= v2clstown_osp < 2)
    #   2 = Market-dominated mixed economy (2 <= v2clstown_osp < 3)
    #   3 = Market economy (v2clstown_osp >= 3)

    dplyr::arrange(country_id, year) %>%
    dplyr::mutate(econ_type = dplyr::case_when(
                    v2clstown_osp < 1 ~ 0L,
                    v2clstown_osp < 2 ~ 1L,
                    v2clstown_osp < 3 ~ 2L,
                    v2clstown_osp >= 3 ~ 3L,
                    TRUE ~ NA_integer_),
                  # here we code whether an economy type change event occurred
                  # 1 = to market-dominant, -1 = to state-dominant
                  econ_regch_event = ifelse(econ_type > 1 & dplyr::lag(econ_type < 2, n = 1), 1, 0),
                  econ_regch_event = ifelse(econ_type < 2 & dplyr::lag(econ_type > 1, n = 1), -1, econ_regch_event),
                  # here we code the year of the most recent economy type change event
                  econ_regch_year = ifelse(econ_regch_event == -1 | econ_regch_event == 1, year, NA),
                  # here we code the filled economy change variable
                  econ_regch_filled = ifelse(!is.na(econ_regch_year), econ_regch_event, NA)) %>%
    # initially we fill everything
    tidyr::fill(c(econ_regch_filled, econ_regch_year)) %>%
    # here we replace with NA for gaps
    dplyr::mutate(econ_regch_filled = ifelse(!is.na(econ_regch_year) & ((!is.na(gapend1) & econ_regch_year<gapstart1 & year>=gapstart1) |
                                                                        (!is.na(gapend2) & econ_regch_year<gapstart2 & year>=gapstart2) |
                                                                        (!is.na(gapend3) & econ_regch_year<gapstart3 & year>=gapstart3)),
                                            NA, econ_regch_filled),
                  econ_regch_year = ifelse(is.na(econ_regch_filled), NA, econ_regch_year)) %>%
    ungroup() %>%
    group_by(country_id, econ_regch_year) %>%
    # here we check whether the economy type change is censored
    dplyr::mutate(econ_regch_censored = ifelse(codingend - econ_regch_year < tolerance, 1, 0),
                  # censored near gap
                  econ_regch_censored = ifelse(!is.na(gapstart) & gapstart - econ_regch_year < tolerance, 1, econ_regch_censored),
                  # here we check if a transition to market economy was sustained
                  mkt_trans_dum = ifelse(econ_regch_event == 1 &
                                          any(econ_type >= 2 & year >= econ_regch_year), 1, NA),
                  mkt_trans_dum = ifelse(econ_regch_event == 1 & is.na(mkt_trans_dum), 0, mkt_trans_dum),
                  econ_regch_censored = ifelse(!is.na(mkt_trans_dum) & mkt_trans_dum == 1, 0, econ_regch_censored),
                  mkt_trans_dum = ifelse(econ_regch_censored == 1 & mkt_trans_dum == 0, NA, mkt_trans_dum),

                  # here we check if a transition to state economy was sustained
                  econ_stabilized = min(hablar::s(ifelse(econ_type == 1 & year == econ_regch_year &
                                                          dplyr::lead(econ_type == 1, n = tolerance), 1, NA))),
                  econ_closed = ifelse(econ_regch_event == -1, 1 - min(hablar::s(econ_type)), NA),
                  state_breakdown_dum = ifelse(econ_regch_event == -1 & ((!is.na(econ_stabilized) & econ_stabilized == 1) |
                                                                          (!is.na(econ_closed) & econ_closed == 1)), 1, NA),
                  state_breakdown_dum = ifelse(econ_regch_event == -1 & is.na(state_breakdown_dum), 0, state_breakdown_dum),
                  econ_regch_censored = ifelse(!is.na(state_breakdown_dum) & state_breakdown_dum == 1, 0, econ_regch_censored),
                  state_breakdown_dum = ifelse(!is.na(econ_regch_censored) & econ_regch_censored == 1, NA, state_breakdown_dum)) %>%
    # here we code the economy regimes based on our criteria
    ungroup() %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    # econ_reg_type: 1 = market-dominant (econ_type >= 2), 0 = state-dominant (econ_type < 2)
    dplyr::mutate(econ_start_year = ifelse(!is.na(mkt_trans_dum) & econ_regch_event == 1, year, NA),
                  econ_start_year = ifelse(!is.na(state_breakdown_dum) & state_breakdown_dum == 1, year, econ_start_year),
                  econ_start_year = ifelse(year == codingstart2, year, econ_start_year),
                  econ_start_year = ifelse(!is.na(gapend1) & year == gapend1 + 1, year, econ_start_year),
                  econ_start_year = ifelse(!is.na(gapend2) & year == gapend2 + 1, year, econ_start_year),
                  econ_start_year = ifelse(!is.na(gapend3) & year == gapend3 + 1, year, econ_start_year)) %>%
    tidyr::fill(econ_start_year) %>%
    dplyr::mutate(econ_start_year = ifelse(!is.na(econ_start_year) & ((!is.na(gapend1) & econ_start_year < gapstart1 & year >= gapstart1) |
                                                                      (!is.na(gapend2) & econ_start_year < gapstart2 & year >= gapstart2) |
                                                                      (!is.na(gapend3) & econ_start_year < gapstart3 & year >= gapstart3)),
                                          NA, econ_start_year)) %>%
    ungroup() %>%
    group_by(country_id, econ_start_year) %>%
    # economy regime type: 1 = market-dominant, 0 = state-dominant
    dplyr::mutate(econ_reg_type = ifelse(year == econ_start_year & econ_type > 1, 1, NA),
                  econ_reg_type = ifelse(year == econ_start_year & econ_type < 2, 0, econ_reg_type),
                  econ_reg_type = min(hablar::s(econ_reg_type))) %>%
    ungroup() %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    # here we look for years where economy type changes
    dplyr::mutate(econ_trans = ifelse(!is.na(econ_reg_type), econ_reg_type - dplyr::lag(econ_reg_type, n = 1), NA),
                  econ_start_year = ifelse(!is.na(econ_trans) & econ_trans != 0, year, NA),
                  econ_start_year = ifelse(year == codingstart2, year, econ_start_year),
                  econ_start_year = ifelse(!is.na(gapend1) & year == gapend1 + 1, year, econ_start_year),
                  econ_start_year = ifelse(!is.na(gapend2) & year == gapend2 + 1, year, econ_start_year),
                  econ_start_year = ifelse(!is.na(gapend3) & year == gapend3 + 1, year, econ_start_year)) %>%
    tidyr::fill(econ_start_year) %>%
    dplyr::mutate(econ_start_year = ifelse(!is.na(econ_start_year) & ((!is.na(gapend1) & econ_start_year < gapstart1 & year >= gapstart1) |
                                                                      (!is.na(gapend2) & econ_start_year < gapstart2 & year >= gapstart2) |
                                                                      (!is.na(gapend3) & econ_start_year < gapstart3 & year >= gapstart3)),
                                          NA, econ_start_year)) %>%
    ungroup() %>%
    group_by(country_id, econ_start_year) %>%
    dplyr::mutate(econ_end_year = dplyr::last(year),
                  econ_id = ifelse(!is.na(econ_start_year), paste(country_text_id, econ_start_year, econ_end_year, sep = "_"), NA),
                  mkt_trans_dum = ifelse(econ_trans == 0 | is.na(econ_trans), 0, mkt_trans_dum),
                  state_breakdown_dum = ifelse(econ_trans == 0 | is.na(econ_trans), 0, state_breakdown_dum)) %>%
    ungroup() %>%
    # make sure the data are sorted and grouped properly before sending to C++
    arrange(country_text_id, year) %>%
    group_by(country_text_id) %>%


    ### CODING THE PRIVATIZATION EPISODES ###

  # detect and save potential episodes with the help of the c++ function find_seqs_priv
  dplyr::mutate(episode_id = find_seqs_priv(v2clstown_osp, econ_type, econ_trans,
                                           start_incl, year_turn = year_turn * -1, cum_turn = cum_turn * -1,
                                           tolerance),
                # set a temporary id for these potential episodes
                character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(character_id) %>%
    # general check: is there a potential privatization episode?
    dplyr::mutate(priv_ep = ifelse(!is.na(episode_id), 1, 0),
                  # check whether the cumulated change is substantial (>= cum_incl), i.e. the episode is manifest
                  priv_ep = ifelse(priv_ep == 1 & max(v2clstown_osp, na.rm = T) - min(v2clstown_osp, na.rm = T) >= cum_incl, 1, 0)) %>%
    dplyr::ungroup() %>%
    # clean out variables for non-manifest episodes
    dplyr::mutate(episode_id = ifelse(priv_ep != 1, NA, episode_id),
                  character_id = ifelse(priv_ep != 1, NA, character_id)) %>%
    dplyr::group_by(character_id) %>%
    dplyr::mutate(priv_ep_end_year = ifelse(priv_ep == 1, last(year), NA),
                  priv_ep_uncertain = ifelse(priv_ep == 1 & codingend - priv_ep_end_year < tolerance, 1, 0),
                  priv_ep_uncertain = ifelse(priv_ep == 1 & !is.na(gapstart) & (gapstart - 1) - priv_ep_end_year < tolerance, 1, priv_ep_uncertain),
                  priv_ep_start_year = ifelse(priv_ep == 1, first(year), NA),
                  priv_ep_id = ifelse(priv_ep == 1, paste(country_text_id, priv_ep_start_year, priv_ep_end_year, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-character_id, -episode_id) %>%
    dplyr::arrange(country_name, year) %>%
    as.data.frame %>%


    # code termination type of privatization episode

    # privatization episodes end when one of five things happens:
    # 0. the outcome is unknown
    # 1. stasis: no annual increase >= start_incl for the tolerance period
    # 2. year drop: annual drop <= -year_turn
    # 3. cumulative drop: gradual drop <= -cum_turn over the tolerance period
    # 4. breakdown: reverted to planned economy (econ_type == 0) or economy transition to state

  # first find the last positive change equal to start_incl
  dplyr::group_by(priv_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2clstown_osp - dplyr::lag(v2clstown_osp, n = 1) >= start_incl, year, NA))),
                  last_ch_year = ifelse(priv_ep == 0, NA, last_ch_year)) %>%

    # check for economy breakdown within the episode period (termination type #4)
    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year) %>%
    dplyr::mutate(breakdown = ifelse((dplyr::lead(econ_type, n = 1) == 0 & econ_type > 0) |
                                       dplyr::lead(econ_trans == -1), year, NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(priv_ep_id) %>%
    dplyr::mutate(breakdown = ifelse(priv_ep == 1 & year >= priv_ep_start_year, breakdown, NA),
                  breakdown = min(hablar::s(breakdown)),
                  priv_ep_end_year = ifelse(!is.na(breakdown) & priv_ep_end_year > breakdown, breakdown, priv_ep_end_year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(priv_ep_start_year = ifelse(priv_ep == 1 & year > priv_ep_end_year, NA, priv_ep_start_year),
                  priv_ep_end_year = ifelse(priv_ep == 1 & year > priv_ep_end_year, NA, priv_ep_end_year),
                  priv_ep = ifelse(priv_ep == 1 & year > priv_ep_end_year, 0, priv_ep),
                  priv_ep_id = ifelse(priv_ep == 1, paste(country_text_id, priv_ep_start_year, priv_ep_end_year, sep = "_"), NA)) %>%
    dplyr::group_by(priv_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2clstown_osp - dplyr::lag(v2clstown_osp, n = 1) >= start_incl, year, NA))),
                  last_ch_year = ifelse(priv_ep == 0, NA, last_ch_year)) %>%

    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year)

  # check termination conditions after last substantive change
  year_drop <- list()
  for (i in 1:tolerance) {
    year_drop[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n = i) == full.df$country_id,
                             dplyr::lead(full.df$v2clstown_osp, n = i) - dplyr::lead(full.df$v2clstown_osp, n = i - 1), NA)
  }
  df1 <- do.call(cbind, lapply(year_drop, data.frame, stringsAsFactors = FALSE))
  names <- paste0('year', seq(1:tolerance))
  colnames(df1) <- names
  my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm = T), NA)
  year_drop <- df1 %>%
    dplyr::mutate(year_drop = ifelse(apply(df1, 1, FUN = my.min) < year_turn * -1, 1, NA))  %>%
    dplyr::select(year_drop)
  stasis <- df1 %>%
    dplyr::mutate(stasis = ifelse(apply(df1, 1, FUN = max) < start_incl & apply(df1, 1, FUN = min) >= year_turn * -1, 1, NA))  %>%
    dplyr::select(stasis)

  cum_drop <- list()
  for (i in 1:tolerance) {
    cum_drop[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n = i) == full.df$country_id,
                            dplyr::lead(full.df$v2clstown_osp, n = i) - full.df$v2clstown_osp, NA)
  }
  df <- do.call(cbind, lapply(cum_drop, data.frame, stringsAsFactors = FALSE))
  names <- paste0('cum', seq(1:tolerance))
  colnames(df) <- names
  cum_drop <- df %>%
    dplyr::mutate(cum_drop = ifelse(apply(df, 1, FUN = my.min) <= cum_turn * -1, 1, NA)) %>%
    dplyr::select(cum_drop)

  # merge these new columns to our full.df
  full.df <- full.df %>%
    tibble::rownames_to_column('newid') %>%
    left_join(tibble::rownames_to_column(year_drop, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(cum_drop, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(stasis, 'newid'), by = 'newid') %>%
    dplyr::select(-newid) %>%

    dplyr::group_by(priv_ep_id) %>%
    dplyr::arrange(priv_ep_id, year) %>%
    dplyr::mutate(stasis = ifelse(priv_ep == 1, max(hablar::s(stasis)), NA),
                  year_drop = ifelse(priv_ep == 1, max(hablar::s(year_drop)), NA),
                  cum_drop = ifelse(priv_ep == 1, max(hablar::s(cum_drop)), NA),
                  # code the termination variable
                  priv_ep_termination = ifelse(priv_ep == 1 & !is.na(stasis) & is.na(year_drop) & is.na(cum_drop)
                                              & is.na(breakdown), 1, NA),
                  priv_ep_termination = ifelse(priv_ep == 1 & !is.na(year_drop) & is.na(breakdown), 2, priv_ep_termination),
                  priv_ep_termination = ifelse(priv_ep == 1 & !is.na(cum_drop) & is.na(year_drop) & is.na(breakdown), 3, priv_ep_termination),
                  priv_ep_termination = ifelse(priv_ep == 1 & !is.na(breakdown), 4, priv_ep_termination),
                  priv_ep_termination = ifelse(priv_ep == 1 & priv_ep_uncertain == 1 & is.na(priv_ep_termination), 0, priv_ep_termination),
                  priv_ep_uncertain = ifelse(priv_ep_termination != 0 & priv_ep == 1, 0, priv_ep_uncertain),
                  priv_ep_end_year = ifelse(priv_ep_uncertain == 0 & priv_ep == 1, last_ch_year, priv_ep_end_year),
                  priv_ep_termination = ifelse(priv_ep == 1 & year > priv_ep_end_year, NA, priv_ep_termination),
                  priv_ep_start_year = ifelse(priv_ep == 1 & year > priv_ep_end_year, NA, priv_ep_start_year),
                  priv_ep_end_year = ifelse(priv_ep == 1 & year > priv_ep_end_year, NA, priv_ep_end_year),
                  priv_ep = ifelse(is.na(priv_ep_end_year), 0, priv_ep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(priv_ep_id = ifelse(priv_ep == 1, paste(country_text_id, priv_ep_start_year, priv_ep_end_year, sep = "_"), NA)) %>%


  # classify privatization episode outcomes
  dplyr::group_by(priv_ep_id) %>%
    dplyr::mutate(priv_ep_prch = ifelse(!is.na(priv_ep_id), 1 - dplyr::first(econ_reg_type, order_by = year), NA),
                  # 1: Market transition (started in state economy, transitioned to market)
                  priv_ep_outcome = ifelse(priv_ep_prch == 1 & econ_trans == 1, 1, NA),
                  # 2: Preempted market transition (economy type change occurred but didn't produce full transition)
                  priv_ep_outcome = ifelse(priv_ep_prch == 1 & any(econ_regch_event == 1) &
                                            year == priv_ep_end_year & priv_ep_uncertain == 0 &
                                            is.na(priv_ep_outcome), 2, priv_ep_outcome),
                  # 3: Stabilized planned economy (stasis in state-dominant economy)
                  priv_ep_outcome = ifelse(priv_ep_prch == 1 & year == priv_ep_end_year & priv_ep_termination == 1
                                          & is.na(priv_ep_outcome), 3, priv_ep_outcome),
                  # 4: Reverted privatization (decline in liberalizing state economy)
                  priv_ep_outcome = ifelse(priv_ep_prch == 1 & year == priv_ep_end_year &
                                            (priv_ep_termination == 2 | priv_ep_termination == 3 | priv_ep_termination == 4) &
                                            is.na(priv_ep_outcome), 4, priv_ep_outcome),
                  # case never moved from planned economy
                  priv_ep_outcome = ifelse(priv_ep_prch == 1 & year == priv_ep_end_year &
                                            max(econ_type) == 0 & priv_ep_uncertain != 1, 4, priv_ep_outcome),
                  # 5: Market economy deepened (started in market economy)
                  priv_ep_outcome = ifelse(priv_ep_prch == 0 & year == priv_ep_end_year &
                                            is.na(priv_ep_outcome), 5, priv_ep_outcome),
                  # 6: Uncertain outcome
                  priv_ep_outcome = ifelse(priv_ep == 1 & priv_ep_uncertain == 1 & is.na(priv_ep_outcome) & year == priv_ep_end_year, 6, priv_ep_outcome),
                  priv_ep_outcome = ifelse(priv_ep == 0, 0, priv_ep_outcome),
                  priv_ep_censored = ifelse(priv_ep == 1 & priv_ep_uncertain == 1, 1, 0)) %>%
    dplyr::arrange(country_id, year) %>%
    dplyr::mutate(priv_ep_outcome = min(hablar::s(priv_ep_outcome)),
                  priv_ep_uncertain = ifelse(priv_ep == 1 & max(priv_ep_outcome) != 6, 0, priv_ep_uncertain)) %>%
    dplyr::mutate(priv_ep_outcome_agg = dplyr::case_when(
      priv_ep_outcome == 2 | priv_ep_outcome == 3 | priv_ep_outcome == 4 ~ 2,
      priv_ep_outcome == 1  ~ 1,
      priv_ep_outcome == 5 ~ 3,
      priv_ep_outcome == 6 ~ 4,
      priv_ep_outcome == 0 ~ 0),
      priv_ep_ptr = dplyr::case_when(
                        priv_ep_outcome == 1 & econ_reg_type == 1 & econ_trans != 1 ~ 1,
                        priv_ep == 0 ~ NA_real_,
                        T ~ 0),
      priv_ep_subdep = dplyr::case_when(
        priv_ep_outcome == 1 & max(hablar::s(priv_ep_ptr == 1)) ~ 1,
        priv_ep == 0 ~ NA_real_,
        T ~ 0)) %>%
    dplyr::group_by(country_text_id) %>%
    dplyr::arrange(country_id, year) %>%
    dplyr::select(-stasis)


  ### CODING THE STATIZATION EPISODES ###

  # detect and save potential episodes with the help of the c++ function find_seqs_stat
  full.df <- full.df %>% dplyr::mutate(episode_id = find_seqs_stat(v2clstown_osp, econ_type, econ_trans,
                                                                  start_incl = start_incl * -1, year_turn, cum_turn, tolerance),
                                       character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(character_id) %>%
    # general check: is there a potential statization episode?
    dplyr::mutate(stat_ep = ifelse(!is.na(episode_id), 1, 0),
                  # check whether the cumulated change is substantial
                  stat_ep = ifelse(stat_ep == 1 & min(hablar::s(v2clstown_osp)) - max(hablar::s(v2clstown_osp)) <= cum_incl * -1, 1, 0)) %>%
    ungroup() %>%
    dplyr::mutate(episode_id = ifelse(stat_ep != 1, NA, episode_id),
                  character_id = ifelse(stat_ep != 1, NA, character_id)) %>%
    group_by(character_id) %>%
    dplyr::mutate(stat_ep_end_year = ifelse(stat_ep == 1, last(year), NA),
                  stat_ep_uncertain = ifelse(stat_ep == 1 & codingend - stat_ep_end_year < tolerance, 1, 0),
                  stat_ep_uncertain = ifelse(stat_ep == 1 & !is.na(gapstart) & (gapstart - 1) - stat_ep_end_year < tolerance, 1, stat_ep_uncertain),
                  stat_ep_start_year = ifelse(stat_ep == 1, first(year), NA),
                  stat_ep_id = ifelse(stat_ep == 1, paste(country_text_id, stat_ep_start_year, stat_ep_end_year, sep = "_"), NA)) %>%
    ungroup() %>%
    dplyr::select(-character_id, -episode_id) %>%
    dplyr::arrange(country_name, year) %>%
    as.data.frame %>%

    # code termination type of statization episode

    # statization episodes end when one of five things happens:
    # 0. the episode outcome is unknown
    # 1. stasis: no annual decrease <= -start_incl for the tolerance period
    # 2. year increase: annual increase >= year_turn
    # 3. cumulative increase: gradual increase >= cum_turn over the tolerance period
    # 4. market transition: the case transitioned to a market economy

  # first find the last negative change equal to -start_incl
  group_by(stat_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2clstown_osp - dplyr::lag(v2clstown_osp, n = 1) <= start_incl * -1, year, NA))),
                  last_ch_year = ifelse(stat_ep == 0, NA, last_ch_year)) %>%

    # check for market transition within the episode period (termination type #4)
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    dplyr::mutate(mkt_trans = ifelse((dplyr::lead(econ_type, n = 1) == 3 & econ_type < 3) |
                                       dplyr::lead(econ_trans == 1), year, NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(stat_ep_id) %>%
    dplyr::mutate(mkt_trans = ifelse(stat_ep == 1 & year >= stat_ep_start_year, mkt_trans, NA),
                  mkt_trans = min(hablar::s(mkt_trans)),
                  stat_ep_end_year = ifelse(!is.na(mkt_trans) & stat_ep_end_year > mkt_trans, mkt_trans, stat_ep_end_year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(stat_ep_start_year = ifelse(stat_ep == 1 & year > stat_ep_end_year, NA, stat_ep_start_year),
                  stat_ep_end_year = ifelse(stat_ep == 1 & year > stat_ep_end_year, NA, stat_ep_end_year),
                  stat_ep = ifelse(stat_ep == 1 & year > stat_ep_end_year, 0, stat_ep),
                  stat_ep_id = ifelse(stat_ep == 1, paste(country_text_id, stat_ep_start_year, stat_ep_end_year, sep = "_"), NA)) %>%
    dplyr::group_by(stat_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2clstown_osp - dplyr::lag(v2clstown_osp, n = 1) <= start_incl * -1, year, NA))),
                  last_ch_year = ifelse(stat_ep == 0, NA, last_ch_year)) %>%

    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year)

  #### then check to see what happened after the episode had its last substantive change

  year_incr <- list()
  for (i in 1:tolerance) {
    year_incr[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n = i) == full.df$country_id,
                             dplyr::lead(full.df$v2clstown_osp, n = i) - dplyr::lead(full.df$v2clstown_osp, n = i - 1), NA)
  }
  df1 <- do.call(cbind, lapply(year_incr, data.frame, stringsAsFactors = FALSE))
  names <- paste0('year', seq(1:tolerance))
  colnames(df1) <- names
  my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm = T), NA)
  year_incr <- df1 %>%
    dplyr::mutate(year_incr = ifelse(apply(df1, 1, FUN = my.max) > year_turn, 1, NA))  %>%
    dplyr::select(year_incr)
  stasis <- df1 %>%
    dplyr::mutate(stasis = ifelse(apply(df1, 1, FUN = min) > start_incl * -1 & apply(df1, 1, FUN = max) <= year_turn, 1, NA))  %>%
    dplyr::select(stasis)

  cum_incr <- list()
  for (i in 1:tolerance) {
    cum_incr[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n = i) == full.df$country_id,
                            dplyr::lead(full.df$v2clstown_osp, n = i) - full.df$v2clstown_osp, NA)
  }
  df <- do.call(cbind, lapply(cum_incr, data.frame, stringsAsFactors = FALSE))
  names <- paste0('cum', seq(1:tolerance))
  colnames(df) <- names
  cum_incr <- df %>%
    dplyr::mutate(cum_incr = ifelse(apply(df, 1, FUN = my.max) >= cum_turn, 1, NA)) %>%
    dplyr::select(cum_incr)

  # merge these new columns to our full.df
  full.df <- full.df %>%
    tibble::rownames_to_column('newid') %>%
    left_join(tibble::rownames_to_column(year_incr, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(cum_incr, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(stasis, 'newid'), by = 'newid') %>%

    ungroup() %>%
    group_by(stat_ep_id) %>%
    dplyr::arrange(stat_ep_id, year) %>%
    dplyr::mutate(stasis = ifelse(stat_ep == 1, max(hablar::s(stasis)), NA),
                  year_incr = ifelse(stat_ep == 1, max(hablar::s(year_incr)), NA),
                  cum_incr = ifelse(stat_ep == 1, max(hablar::s(cum_incr)), NA),
                  # code the termination variable
                  stat_ep_termination = ifelse(stat_ep == 1 & !is.na(stasis) & is.na(year_incr) & is.na(cum_incr)
                                              & is.na(mkt_trans), 1, NA),
                  stat_ep_termination = ifelse(stat_ep == 1 & !is.na(year_incr) & is.na(mkt_trans), 2, stat_ep_termination),
                  stat_ep_termination = ifelse(stat_ep == 1 & !is.na(cum_incr) & is.na(year_incr) & is.na(mkt_trans), 3, stat_ep_termination),
                  stat_ep_termination = ifelse(stat_ep == 1 & !is.na(mkt_trans), 4, stat_ep_termination),
                  stat_ep_termination = ifelse(stat_ep == 1 & stat_ep_uncertain == 1 & is.na(stat_ep_termination), 0, stat_ep_termination),
                  stat_ep_uncertain = ifelse(stat_ep_termination != 0 & stat_ep == 1, 0, stat_ep_uncertain),
                  stat_ep_end_year = ifelse(stat_ep_uncertain == 0 & stat_ep == 1, last_ch_year, stat_ep_end_year),
                  stat_ep_termination = ifelse(stat_ep == 1 & year > stat_ep_end_year, NA, stat_ep_termination),
                  stat_ep_start_year = ifelse(stat_ep == 1 & year > stat_ep_end_year, NA, stat_ep_start_year),
                  stat_ep_end_year = ifelse(stat_ep == 1 & year > stat_ep_end_year, NA, stat_ep_end_year),
                  stat_ep = ifelse(is.na(stat_ep_end_year), 0, stat_ep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(stat_ep_id = ifelse(stat_ep == 1, paste(country_text_id, stat_ep_start_year, stat_ep_end_year, sep = "_"), NA)) %>%
    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year) %>%

  # classify statization episode outcomes
  dplyr::group_by(stat_ep_id) %>%
    dplyr::mutate(stat_ep_prch = ifelse(!is.na(stat_ep_id), dplyr::first(econ_reg_type, order_by = year), NA),
                  # 1: Market collapse (started in market economy, transitioned to state economy)
                  stat_ep_outcome = ifelse(stat_ep_prch == 1 & econ_trans == -1, 1, NA),
                  # 2: Preempted market collapse (economy type change occurred but didn't produce full collapse)
                  stat_ep_outcome = ifelse(stat_ep_prch == 1 & any(econ_regch_event == -1) &
                                            year == stat_ep_end_year & stat_ep_uncertain == 0 &
                                            is.na(stat_ep_outcome), 2, stat_ep_outcome),
                  # 3: Diminished market economy (stasis in declining market economy)
                  stat_ep_outcome = ifelse(stat_ep_prch == 1 & year == stat_ep_end_year & stat_ep_termination == 1
                                          & is.na(stat_ep_outcome), 3, stat_ep_outcome),
                  # 4: Averted statization (improvement in declining market economy)
                  stat_ep_outcome = ifelse(stat_ep_prch == 1 & year == stat_ep_end_year &
                                            (stat_ep_termination == 2 | stat_ep_termination == 3 | stat_ep_termination == 4) &
                                            is.na(stat_ep_outcome), 4, stat_ep_outcome),
                  # case never fell below market economy
                  stat_ep_outcome = ifelse(stat_ep_prch == 1 & year == stat_ep_end_year &
                                            min(econ_type) == 3 & stat_ep_uncertain == 0, 4, stat_ep_outcome),
                  # 5: Deepened planned economy (started in state economy)
                  stat_ep_outcome = ifelse(stat_ep_prch == 0 & year == stat_ep_end_year &
                                            is.na(stat_ep_outcome), 5, stat_ep_outcome),
                  # 6: Uncertain outcome
                  stat_ep_outcome = ifelse(stat_ep == 1 & stat_ep_uncertain == 1 & is.na(stat_ep_outcome) & year == stat_ep_end_year, 6, stat_ep_outcome),
                  stat_ep_outcome = ifelse(stat_ep == 0, 0, stat_ep_outcome),
                  stat_ep_censored = ifelse(stat_ep == 1 & stat_ep_uncertain == 1, 1, 0)) %>%
    dplyr::arrange(country_id, year) %>%
    dplyr::mutate(stat_ep_outcome = min(hablar::s(stat_ep_outcome)),
                  stat_ep_uncertain = ifelse(stat_ep == 1 & max(stat_ep_outcome) != 6, 0, stat_ep_uncertain)) %>%
    dplyr::mutate(stat_ep_outcome_agg = dplyr::case_when(
      stat_ep_outcome == 2 | stat_ep_outcome == 3 | stat_ep_outcome == 4 ~ 2,
      stat_ep_outcome == 1 ~ 1,
      stat_ep_outcome == 5 ~ 3,
      stat_ep_outcome == 6 ~ 4,
      stat_ep_outcome == 0 ~ 0),
      stat_ep_pbr = dplyr::case_when(
        stat_ep_outcome == 1 & econ_reg_type == 0 & econ_trans != -1 ~ 1,
        stat_ep == 0 ~ NA_real_,
        T ~ 0),
      stat_ep_subreg = dplyr::case_when(
        stat_ep_outcome == 1 & max(hablar::s(stat_ep_pbr == 1)) ~ 1,
        stat_ep == 0 ~ NA_real_,
        T ~ 0)) %>%
    dplyr::group_by(country_text_id) %>%
    dplyr::arrange(country_id, year) %>%


    # select the variables we need to keep
    dplyr::filter(!is.na(origsample)) %>%
    dplyr::select(country_id, country_text_id, country_name, year, v2clstown_osp, econ_type,
                  econ_start_year, econ_end_year, econ_id, econ_reg_type, econ_trans, econ_regch_event, econ_regch_censored,
                  priv_ep, priv_ep_id, priv_ep_start_year, priv_ep_end_year, priv_ep_termination,
                  priv_ep_prch, priv_ep_ptr, priv_ep_subdep,
                  priv_ep_outcome, priv_ep_outcome_agg, priv_ep_censored,
                  stat_ep, stat_ep_id, stat_ep_start_year, stat_ep_end_year, stat_ep_termination,
                  stat_ep_prch, stat_ep_pbr, stat_ep_subreg,
                  stat_ep_outcome, stat_ep_outcome_agg, stat_ep_censored) %>%
    ungroup()


  {
    return(full.df)
    }
}
### done ;-) ###
