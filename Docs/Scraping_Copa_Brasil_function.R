## specify the packages of interest
packages = c("rvest",
             "stringr",
             "lubridate",
             "xml2",
             "purrr",
             "tidyverse")


## Now load or install & load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


#################

scraping_copa_br <- function(season) {
  #### URL
  url <-
    paste0(
      "https://www.cbf.com.br/futebol-brasileiro/competicoes/copa-brasil-masculino/",
      season
    )
  
  ##### Getting links of rounds
  links_rounds <- url %>%
    read_html() %>%
    xml_find_all("//ul[@class='group-btns list-unstyled m-t-30 m-b-30 hidden-xs']//a") %>%
    xml_attr("href")
  
  ##### Parse function
  parse_func <- function(link) {
    info <- link %>%
      read_html() %>%
      xml_find_first("//section[@class='m-b-50 p-t-10']//section")
    ############### FALTA AJUSTAR AQUI, A PARTIR DE 2017 AS 2 PRIMEIRAS RODADAS NO TEM
    ############### "hidden-md" NO FINAL DA CLASSE DO TIME DA CASA E VISITANTE
    xpaths <- c(
      "//div[@class= 'text-1 m-b-10 text-center uppercase']",
      "//div[@class= 'text-2 pull-right p-t-15 p-r-10 hidden-xs hidden-sm']",
      "//div[@class= 'text-2 pull-left p-t-15 p-l-20 hidden-xs hidden-sm']",
      "//div[@class= 'text-2 pull-right p-t-15 p-r-10 hidden-xs hidden-sm hidden-md']",
      "//div[@class= 'text-2 pull-left p-t-15 p-l-20 hidden-xs hidden-sm hidden-md']",
      "//div[@class= 'text-3 p-t-15']"
    )
    
    
    list <-  xpaths %>%
      map(~ xml_find_all(info, .x)) 
    
    list[lapply(list, length) > 0] %>% 
    map(xml_text) %>%
      set_names("datetime", "home_team", "away_team", "result") %>%
      as_tibble()
    
  }
  
  
  df <- links_rounds %>%
    map_dfr(parse_func, .id = "round") %>%
    mutate(
      # treating datetime
      datetime = datetime %>%
        gsub("[\r\n]", "", .) %>%
        str_sub(str_locate(., ",")[, 1] + 2, str_locate(., "-")[, 1] - 1) %>%
        str_trim("both") %>%
        parse_date_time(c('%d/%m/%y %H:%M')),
      
      # treating score
      result = result %>%
        gsub("[\r\n]", "", .) %>%
        gsub("\\s*\\([^\\)]+\\)", "", .) %>%
        str_trim("both"),
      
      # Creating Season variable
      season = season
      
    ) %>%
    separate(result,
             sep = "x",
             into = c("home_goal", "away_goal")) %>%
    mutate(
      home_goal = home_goal %>%
        str_trim("both") %>%
        as.numeric(),
      
      away_goal = away_goal %>%
        str_trim("both") %>%
        as.numeric()
    )
}
