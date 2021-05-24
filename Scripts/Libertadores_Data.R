source("Scripts/Scraping_Libertadores_Groups.R")

Libertadores_Matches <-
  purrr::map_dfr(2013:2021, scraper_libertadores_group)

Libertadores_Matches %>% 
  arrange(datetime) %>% 
  write.csv("Data/Libertadores_Matches.csv",fileEncoding = "UTF-8", row.names = F)
