devtools::load_all()


`%>%` <- magrittr::`%>%`


#---------------- creating df with historical data

brasileirao_matches <-
  purrr::map_dfr(2012:2020, scraping_brasileirao) %>%
  fix_names()

brasileirao_matches %>%
  readr::write_csv("data-raw/csv/brasileirao_matches.csv")

usethis::use_data(brasileirao_matches, overwrite = TRUE)
