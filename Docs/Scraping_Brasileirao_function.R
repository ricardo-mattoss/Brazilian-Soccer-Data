library(glue)
library(rvest)
library(stringr)
library(dplyr)
library(lubridate)

scraping_brasileirao <- function(season) {
  url <-
    glue(
      "https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/{season}"
    )
  
  # Getting node with table matches
  resultados <- url %>%
    html() %>%
    html_nodes(".aside-rodadas")
  
  # Getting and convert in datetime
  datetime <-
    resultados %>%
    html_nodes(".partida-desc.text-1.color-lightgray.p-b-15.block.uppercase.text-center") %>%
    html_text() %>%
    str_sub(., 38, 53) %>%
    parse_date_time(., c('%d/%m/%y %H:%M'))
  
  # Getting home team name
  home_team <- resultados %>%
    html_nodes(".time.pull-left img") %>%
    html_attr("title") %>%
    str_sub(1, str_locate(., "-")[, 1] - 2)
  
  # Getting home team state
  home_team_state <- resultados %>%
    html_nodes(".time.pull-left img") %>%
    html_attr("title") %>%
    str_sub(str_locate(., "-")[, 1] + 2, str_locate(., "-")[, 1] + 4)
  
  # Getting away team name
  away_team <- resultados %>%
    html_nodes(".time.pull-right img") %>%
    html_attr("title") %>%
    str_sub(1, str_locate(., "-")[, 1] - 2)
  
  # Getting away team state
  away_team_state <- resultados %>%
    html_nodes(".time.pull-right img") %>%
    html_attr("title") %>%
    str_sub(str_locate(., "-")[, 1] + 2, str_locate(., "-")[, 1] + 4)
  
  # Getting results of matches
  placar <-  resultados %>%
    html_nodes(".partida-horario") %>%
    html_text() %>%
    str_extract("[0-9]{1}\ x\ [0-9]{1}")
  
  # Getting home team goals
  home_goal <- placar %>%
    str_sub(1, 1) %>% 
    as.integer()
  
  # Getting away team goals
  away_goal <- placar %>%
    str_sub(5, 5) %>% 
    as.integer()
  
  # Getting round of league
  round <- 0:(length(placar) - 1) %/% 10 + 1
  
  # bind cols
  bind_cols(
    datetime = datetime,
    home_team = home_team,
    home_team_state = home_team_state,
    away_team = away_team,
    away_team_state = away_team_state,
    home_goal = home_goal,
    away_goal = away_goal,
    season = rep(season, length(round)),
    round = round
  )

  
}