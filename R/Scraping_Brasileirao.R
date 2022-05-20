scraping_brasileirao <- function(season) {
  url <-
    glue::glue(
      "https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/{season}"
    )

  for (i in 1:100) {
    con <- url %>% httr::GET(httr::timeout(600))
    if (con$status_code == 200) {
      break
    }
    print(paste0("Tentativa de conexão ", i))

  }

  # Getting node with table matches
  resultados <- con %>%
    xml2::read_html() %>%
    rvest::html_nodes(".aside-rodadas")

  # Getting and convert in datetime
  datetime <-
    resultados %>%
    rvest::html_nodes(".partida-desc.text-1.color-lightgray.p-b-15.block.uppercase.text-center") %>%
    rvest::html_text() %>%
    stringr::str_sub(., 38, 53) %>%
    lubridate::parse_date_time(., c('%d/%m/%y %H:%M'))

  # Getting home team name
  home_team <- resultados %>%
    rvest::html_nodes(".time.pull-left img") %>%
    rvest::html_attr("title") %>%
    stringr::str_sub(1, stringr::str_locate(., "-")[, 1] - 2)

  # Getting home team state
  home_team_state <- resultados %>%
    rvest::html_nodes(".time.pull-left img") %>%
    rvest::html_attr("title") %>%
    stringr::str_sub(stringr::str_locate(., "-")[, 1] + 2,
                     stringr::str_locate(., "-")[, 1] + 4)

  # Getting away team name
  away_team <- resultados %>%
    rvest::html_nodes(".time.pull-right img") %>%
    rvest::html_attr("title") %>%
    stringr::str_sub(1, stringr::str_locate(., "-")[, 1] - 2)

  # Getting away team state
  away_team_state <- resultados %>%
    rvest::html_nodes(".time.pull-right img") %>%
    rvest::html_attr("title") %>%
    stringr::str_sub(stringr::str_locate(., "-")[, 1] + 2,
                     stringr::str_locate(., "-")[, 1] + 4)

  # Getting results of matches
  placar <-  resultados %>%
    rvest::html_nodes(".partida-horario") %>%
    rvest::html_text() %>%
    stringr::str_extract("[0-9]{1}\ x\ [0-9]{1}")

  # Getting home team goals
  home_goal <- placar %>%
    stringr::str_sub(1, 1) %>%
    as.integer()

  # Getting away team goals
  away_goal <- placar %>%
    stringr::str_sub(5, 5) %>%
    as.integer()

  # Getting round of league
  round <- 0:(length(placar) - 1) %/% 10 + 1

  # bind cols
  df <-  dplyr::bind_cols(
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

  # Return tibble
  return(df)
}
