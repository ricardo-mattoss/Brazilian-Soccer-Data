devtools::load_all()


`%>%` <- magrittr::`%>%`

# Count of matches without data in current df
rows_df_na <- brasileirao_matches %>%
  dplyr::filter(season == lubridate::year(Sys.Date()) &
                  is.na(home_goal)) %>%
  dplyr::summarise(dplyr::n()) %>%
  as.numeric()

# Getting data of actual season
df_update <-
  purrr::map_dfr(lubridate::year(Sys.Date()), scraping_brasileirao) %>%
  fix_names()

# Count of matches without data in actual season
rows_actual_season_na <- df_update %>%
  dplyr::filter(is.na(home_goal)) %>%
  dplyr::summarise(dplyr::n()) %>%
  as.numeric()

# Checking if there is new data
if (rows_df_na == rows_actual_season_na) {
  print("The data frame is already up to date")

} else {
  brasileirao_matches <- brasileirao_matches %>%
    dplyr::filter(season != lubridate::year(Sys.Date())) %>%
    dplyr::bind_rows(.,
                     df_update)

  readr::write_csv(brasileirao_matches,
                   "data-raw/csv/Brasileirao_Matches.csv",
                   append = FALSE)

  usethis::use_data(brasileirao_matches, overwrite = TRUE)
}
