source("Docs/Scraping_Libertadores_function.R")

# Creating df with historical data
# Libertadores_Matches <-
#   purrr::map_dfr(2013:2021, scraper_libertadores_group)

# Reading DF
Libertadores_Matches <- read.csv("Data/Libertadores_Matches.csv",
                                fileEncoding = "UTF-8") %>%
  as_tibble() %>%
  mutate(datetime = datetime %>% as_datetime())

# Count of matches without data in currente df
rows_df_na <- Libertadores_Matches %>%
  filter(season == year(Sys.Date()) &
           home_goal == '-') %>%
  summarise(n()) %>%
  as.numeric()

# Getting data of actual season
df_update <-
  purrr::map_dfr(year(Sys.Date()), scraper_libertadores_group)

# Count of matches without data in actual season
rows_actual_season_na <- df_update %>%
  filter(home_goal == '-') %>%
  summarise(n()) %>%
  as.numeric()

# Checking if there is new data
if (rows_df_na == rows_actual_season_na) {
  print("The data frame is already up to date")
  
} else {
  Libertadores_Matches %>%
    filter(season != year(Sys.Date())) %>%
    bind_rows(.,
              df_update) %>%
    write.csv("Data/Libertadores_Matches.csv",
              fileEncoding = "UTF-8",
              row.names = F)
  
}
