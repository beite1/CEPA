timeMapper <- function(spot_years, horizon_length, first_horizon, last_horizon, first_time, last_time){
  map_df(spot_years, ~tibble(horizon = c(rep(first_horizon, each = horizon_length - first_time + 1),
                                         rep((first_horizon + 1):(last_horizon - 1), each = horizon_length),
                                         rep(last_horizon, each = last_time)),
                             period = c(first_time:horizon_length, rep(1:70, times = last_horizon - first_horizon - 1), 1:last_time),
                             year = .x,
                             date_time = seq(ymd_h(paste0(.x, "-1-1 0")), by = "hour", length.out = 8760)) %>%
           mutate(horizon = paste0("r", horizon)))
}

