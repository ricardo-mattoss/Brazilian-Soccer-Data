source("Docs/Scraping_Brasileirao_function.R")
source("R/fix_names.R")

# Creating df with historical data

# Brasileirao_Matches <-
#   purrr::map_dfr(2012:2021, scraping_brasileirao) %>%
# fix_names()

# Brasileirao_Matches %>%
#   write.csv("Data/Brasileirao_Matches.csv",
#             fileEncoding = "UTF-8",
#             row.names = F)

Brasileirao_Matches <- read.csv("Data/Brasileirao_Matches.csv",
                                fileEncoding = "UTF-8") %>%
  as_tibble() %>%
  mutate(datetime = datetime %>% as_datetime())

# Count of matches without data in current df
rows_df_na <- Brasileirao_Matches %>%
  filter(season == year(Sys.Date()) &
           is.na(home_goal)) %>%
  summarise(n()) %>%
  as.numeric()

# Getting data of actual season
df_update <-
  purrr::map_dfr(year(Sys.Date()), scraping_brasileirao) %>%
  fix_names()

# Count of matches without data in actual season
rows_actual_season_na <- df_update %>%
  filter(is.na(home_goal)) %>%
  summarise(n()) %>%
  as.numeric()

# Checking if there is new data
if (rows_df_na == rows_actual_season_na) {
  print("The data frame is already up to date")

} else {
  Brasileirao_Matches %>%
    filter(season != year(Sys.Date())) %>%
    bind_rows(.,
              df_update) %>%
    write.csv("Data/Brasileirao_Matches.csv",
              fileEncoding = "UTF-8",
              row.names = F)

}
