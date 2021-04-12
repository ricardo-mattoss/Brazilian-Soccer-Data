library(stringr)
library(rvest)
library(glue)
library(dplyr)
library(readr)
library(lubridate)


df_atual <-
  read_csv(
    "Brasileirao_Matches.csv"
  )

max_ano <- df_atual %>%
  summarise(max(ano)) %>% as.numeric()



for (i in (max_ano + 1):year(Sys.Date())) {
  if (max_ano == year(Sys.Date()))
  {
    print("The data frame is already updated")
  }
  else {
    url <-
      glue(
        "https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/{i}"
      )
    
    
    resultados <- url %>%
      html() %>%
      html_nodes(".aside-rodadas")
    
    casa <- resultados %>%
      html_nodes(".pull-left .time-sigla") %>%
      html_text()
    
    
    fora <- resultados %>%
      html_nodes(".pull-right .time-sigla") %>%
      html_text()
    
    
    placar <-  resultados %>%
      html_nodes(".partida-horario") %>%
      html_text() %>%
      str_extract("[0-9]{1}\ x\ [0-9]{1}")
    
    rodada <- 0:(length(placar) - 1) %/% 10 + 1
    
    df <- if (i == (max_ano + 1))
    {
      data.frame(cbind(
        rodada = rodada,
        casa = casa,
        placar = placar,
        fora = fora,
        ano = rep(i, length(rodada))
      ))
    }
    else{
      data.frame(rbind(
        df,
        cbind(
          rodada = rodada,
          casa = casa,
          placar = placar,
          fora = fora,
          ano = rep(i, length(rodada))
        )
      ))
    }
    
    
  }
}

write.table(
  df,
  "Brasileirao_Matches.csv",
  sep = ",",
  append = TRUE,
  quote = FALSE,
  col.names = FALSE,
  row.names = TRUE
)
