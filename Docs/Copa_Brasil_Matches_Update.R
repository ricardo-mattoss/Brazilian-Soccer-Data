source("Docs/Scraping_Copa_Brasil_function.R")

 ##### Creating df with historical data
 # Brazilian_Cup_Matches <-
 #   purrr::map_dfr(2012:2021, scraping_copa_br)
 # 
 # Brazilian_Cup_Matches %>%
 #   write.csv("Data/Brazilian_Cup_Matches.csv",
 #             fileEncoding = "UTF-8",
 #             row.names = F)
 
Brazilian_Cup_Matches <- read.csv("Data/Brazilian_Cup_Matches.csv",
                                fileEncoding = "UTF-8") %>%
  as_tibble() %>%
  mutate(datetime = datetime %>% as_datetime())

# Count of matches without data in current df
rows_df_na <- Brazilian_Cup_Matches %>%
  filter(season == year(Sys.Date()) &
           is.na(home_goal)) %>%
  summarise(n()) %>%
  as.numeric()

# Getting data of actual season
df_update <-
  purrr::map_dfr(year(Sys.Date()), scraping_copa_br)

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
