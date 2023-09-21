#' Calculate stats for ELC Yearchart
#'
#' @param data dataset in the specific format used in elc periodic
#'
#' @return dataset with base stats like n, mean sd, the same qfter removing outliers en the vqlues the previous period
#' @export
#'
calculate_elc_periodic_stats <- function(data) {
  aantallen <- data %>% filter(!is.na(PERIOD)) %>% group_by(PERIOD) %>% summarise(aantal = n())
  data_last <- data %>% filter(PERIOD == "LAST")


  #indien geen data het laatste jaar, stop de routine en vult ook niets in voor de voorgaande jaren
  if (!nrow(data_last)) {
    return(data.frame(n_orig = NA, mean_orig = NA, sd_orig = NA,
                      n = 0, mean = NA, sd = NA,
                      pval_t = NA, pval_f = NA,
                      n_prev = NA, mean_prev = NA, sd_prev = NA))
  }

  #data vorig vorige periode (evenveel elementen als laatste periode)
  data_prev <- data %>% filter(PERIOD != "LAST") %>%
    arrange(rownr) %>%
    slice_tail(n = aantallen %>% filter(PERIOD == "LAST") %>% pull(aantal))

  #bereken eigenschappen zonder waarden buiten 3s weg te gooien
  stats_last <- get_base_stats(data_last$VALUE)

  #itereer tot alle waarden binnen 3s liggen
  kept_values_last <- remove_outliers(data_last$VALUE)
  kept_values_prev <- remove_outliers(data_prev$VALUE)
  stats_last_cleaned <- get_base_stats(kept_values_last)
  stats_prev_cleaned <- get_base_stats(kept_values_prev)

  #neem de laatste waarden (evenveel als het laatste jaar) uit de vorige 2 periodes
  #haal hier ook alle waarden buiten 3s weg

  if (nrow(data_prev) < (nrow(data_last) - 3) | nrow(data_last) < 10) { #indien te weinig waarnemingen, doe geen test
    pval_t = NA
    pval_f = NA
  } else {
    pval_t <- t.test  (kept_values_last, kept_values_prev)$p.value
    pval_f <- var.test(kept_values_last, kept_values_prev)$p.value
  }
  stats_prev <- get_base_stats(data_last$VALUE[stats_last["n"]])
  data.frame(n_orig = round(stats_last["n"]),
             mean_orig = round(stats_last["mean"],4),
             sd_orig = round(stats_last["sd"],4),
             n = round(stats_last_cleaned["n"]),
             mean = round(stats_last_cleaned["mean"],4),
             sd = round(stats_last_cleaned["sd"],4),
             pval_t = round(pval_t,5),
             pval_f = round(pval_f,5),
             n_prev = round(stats_prev_cleaned["n"]),
             mean_prev = round(stats_prev_cleaned["mean"],4),
             sd_prev = round(stats_prev_cleaned["sd"],4))
}






####################################################
