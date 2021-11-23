## specify the packages of interest
packages = c("rvest", "stringr", "glue", "dplyr", "lubridate", "xml2")


## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

scraper_libertadores_group <- function(season) {
  url <-
    glue(
      "https://www.uol.com.br/esporte/futebol/campeonatos/libertadores/{season}/#fase-de-grupos"
    )
  Sys.sleep(1)
  
  if (season < 2019) {
    ########### 2013-2018 GROUP STAGE MATCHES ########################
    
    # Getting only tables with matches
    results <- url %>%
      read_html() %>%
      html_nodes(".rodadas")
    
    # Getting datetime of matches
    data <- results %>%
      html_nodes(".info-partida time") %>%
      html_attr('datetime')
    
    # Getting home team name
    home_team <- results %>%
      html_nodes(".time.time1.clube .brasao") %>%
      html_attr('title')
    
    # Getting away team name
    away_team <- results %>%
      html_nodes(".time.time2.clube .brasao") %>%
      html_attr('title')
    
    # Getting home team goals
    home_goal <-  results %>%
      html_nodes(".time.time1.clube .gols") %>%
      html_text()
    
    # Getting away team goals
    away_goal <- results %>%
      html_nodes(".time.time2.clube .gols") %>%
      html_text()
    
    # Bind columns
    df_group_matches <- bind_cols(
      datetime = data %>%
        str_replace("-0300", ":00") %>%
        as_datetime(),
      
      home_team = home_team,
      
      away_team = away_team,
      
      home_goal = home_goal,
      
      away_goal = away_goal,
      
      season =  year(data %>% as.Date()),
      
      stage = "group stage"
      
    )
    
    ############### 2013-2018 ELIMINATORY MATCHES ###################
    
    url <-
      glue(
        "https://www.uol.com.br/esporte/futebol/campeonatos/libertadores/{season}/jogos/"
      )
    
    # Getting only tables with matches
    results <- url %>%
      read_html() %>%
      html_nodes(".agrupamento.com-chaveamento.com-mata-mata")
    
    # Getting Round
    round <- c(rep("Final", 2),
               rep("Semi", 4),
               rep("Quarter", 8),
               rep("round of 16", 16))
    
    # Getting datetime of matches
    data <- results %>%
      html_nodes(".info-partida time") %>%
      html_attr('datetime')
    
    # Getting home team name
    home_team <- results %>%
      html_nodes(".time.time1.clube .brasao") %>%
      html_attr('title')
    
    # Getting away team name
    away_team <- results %>%
      html_nodes(".time.time2.clube .brasao") %>%
      html_attr('title')
    
    # Getting home team goals
    home_goal <-  results %>%
      html_nodes(".time.time1.clube .gols") %>%
      html_text()
    
    # Getting away team goals
    away_goal <- results %>%
      html_nodes(".time.time2.clube .gols") %>%
      html_text()
    
    
    # Bind columns
    df_group_matches %>%
      bind_rows(
        .,
        bind_cols(
          datetime = data %>%
            str_replace("-0300", ":00") %>%
            as_datetime(),
          
          home_team = home_team,
          
          away_team = away_team,
          
          home_goal = home_goal,
          
          away_goal = away_goal,
          
          season =  year(data %>% as.Date()),
          
          stage = c(
            rep("final", 2),
            rep("semifinals", 4),
            rep("quarterfinals", 8),
            rep("round of 16", 16)
          )
        )
      )
    
  }
  
  else {
    ########### 2019+ GROUP STAGE MATCHES ############
    
    if (season < year(Sys.Date())) {
      # EXCLUDE CURRENT SEASON
      # Getting only tables with matches
      results <- url %>%
        read_html() %>%
        html_nodes(".round-wrapper")
      
      # Extract date and hour
      d <-
        results %>%
        html_nodes(".match-info") %>%
        html_text() %>%
        str_sub(8, 20) %>%
        str_remove_all("-") %>%
        str_replace("h", ":")
      
      # Treating format
      date <- paste0(str_sub(d, 1, 5), "/", season) %>%
        dmy()
      
      # Transform in Datetime
      date_time <- paste0(date,
                          str_sub(d, 6, 12) %>%
                            str_replace(": ", ":00"),
                          ":00") %>%
        as_datetime()
      
      # Getting home team name
      home_team <- results %>%
        html_nodes(".team.team-home .team-name") %>%
        html_text()
      
      # Getting away team name
      away_team <- results %>%
        html_nodes(".team.team-away .team-name") %>%
        html_text()
      
      # Getting home team goals
      home_goal <-  results %>%
        html_nodes(".team.team-home .team-score") %>%
        html_text()
      
      # Getting away team goals
      away_goal <- results %>%
        html_nodes(".team.team-away .team-score") %>%
        html_text()
      
      # Bind columns
      df_group_matches <-    bind_cols(
        datetime = date_time,
        
        home_team = home_team,
        
        away_team = away_team,
        
        home_goal = home_goal,
        
        away_goal = away_goal,
        
        season =  year(date_time %>% as.Date()),
        
        stage = "group stage"
        
      )
      ########### Eliminatory matches 2019+ season ##############
      url <-
        glue(
          "https://www.uol.com.br/esporte/futebol/campeonatos/libertadores/{season}/#quartas-de-final"
        )
      
      # Getting only tables with matches
      results <- url %>%
        read_html() %>%
        html_nodes(".step.type-3, .step.type-4")
      # Only round of 16 ahead
      # stopifnot(length(results) > 3)
      results <- results[4:length(results)]
      
      # Extract date and hour
      d <-
        results %>%
        html_nodes(".match-info") %>%
        html_text() %>%
        str_sub(8, 20) %>%
        str_remove_all("-") %>%
        str_replace("h", ":")
      
      # Treating format
      date <- paste0(str_sub(d, 1, 5), "/", season) %>%
        dmy()
      
      # Transform in Datetime
      date_time <- paste0(date,
                          str_sub(d, 6, 12) %>%
                            str_replace(": ", ":00"),
                          ":00") %>%
        as_datetime()
      
      # Getting home team name
      home_team <- results %>%
        html_nodes(".team.team-home .team-name") %>%
        html_text()
      
      # Getting away team name
      away_team <- results %>%
        html_nodes(".team.team-away .team-name") %>%
        html_text()
      
      # Getting home team goals
      home_goal <-  results %>%
        html_nodes(".team.team-home .team-score") %>%
        html_text()
      
      # Getting away team goals
      away_goal <- results %>%
        html_nodes(".team.team-away .team-score") %>%
        html_text()
      
      df_group_matches %>%
        bind_rows(
          .,
          bind_cols(
            datetime = date_time,
            
            home_team = home_team,
            
            away_team = away_team,
            
            home_goal = home_goal,
            
            away_goal = away_goal,
            
            season =  year(date_time %>% as.Date()),
            
            stage = c(
              rep("round of 16", 16),
              rep("quarterfinals", 8),
              rep("semifinals", 4),
              "final" # since season 2019, the final has become just one match
            )
          )
        )
      
    }
    else{
      ##### CURRENT SEASON
      
      #### Matches already done of current season
      
      # Getting only tables with matches
      results <- url %>%
        read_html() %>%
        html_nodes(".round-wrapper")
      
      # Extract date and hour
      d <-
        results %>%
        html_nodes(".match-info") %>%
        html_text() %>%
        str_sub(8, 20) %>%
        str_remove_all("-") %>%
        str_replace("h", ":")
      
      # Treating format
      date <- paste0(str_sub(d, 1, 5), "/", season) %>%
        dmy()
      
      # Transform in Datetime
      date_time <- paste0(date,
                          str_sub(d, 6, 12) %>%
                            str_replace(": ", ":00"),
                          ":00") %>%
        as_datetime()
      
      # Getting home team name
      home_team <- results %>%
        html_nodes(".team.team-home .team-name") %>%
        html_text()
      
      # Getting away team name
      away_team <- results %>%
        html_nodes(".team.team-away .team-name") %>%
        html_text()
      
      # Getting home team goals
      home_goal <-  results %>%
        html_nodes(".team.team-home .team-score") %>%
        html_text()
      
      # Getting away team goals
      away_goal <- results %>%
        html_nodes(".team.team-away .team-score") %>%
        html_text()
      
      # Bind columns
      df_match_done <-  bind_cols(
        datetime = date_time,
        
        home_team = home_team,
        
        away_team = away_team,
        
        home_goal = home_goal,
        
        away_goal = away_goal,
        
        season =  year(date_time %>% as.Date()),
        
        stage = "group stage"
      ) %>%
        filter(!is.na(datetime))
      
      #### Matches To be done of current season
      
      # Extract date and hour
      d <-
        results %>%
        html_nodes(".match-info-data") %>%
        html_text() %>%
        str_sub(6, 18) %>%
        # str_sub(6, 18) %>%
        str_remove_all("-") %>%
        str_replace("h", ":")
      
      # Treating format
      date <- paste0(str_sub(d, 1, 5), "/", season) %>%
        dmy()
      
      # Transform in Datetime
      date_time <- paste0(
        date,
        ifelse(
          str_sub(d, 6, 12) %>% str_length() < 7,
          str_sub(d, 6, 12) %>%
            str_replace(":", ":00"),
          str_sub(d, 6, 12)
        ),
        ":00"
      ) %>%
        as_datetime()
      
      # Getting home team name
      home_team <- results %>%
        html_nodes(".team.team-home .team-name") %>%
        html_text()
      
      # Getting away team name
      away_team <- results %>%
        html_nodes(".team.team-away .team-name") %>%
        html_text()
      
      # Getting home team goals
      home_goal <-  results %>%
        html_nodes(".team.team-home .team-score") %>%
        html_text()
      
      # Getting away team goals
      away_goal <- results %>%
        html_nodes(".team.team-away .team-score") %>%
        html_text()
      
      # Bind columns
      df_match_tbd <-  bind_cols(
        home_team = home_team,
        
        away_team = away_team,
        
        home_goal = home_goal,
        
        away_goal = away_goal
      ) %>%
        filter(home_goal == "-") %>%
        bind_cols(
          datetime = date_time,
          
          season =  year(date_time %>% as.Date()),
          
          stage = "group stage"
        ) %>%
        relocate(datetime)
      
      # Bind Matches made and to be done
      group_matches <-    bind_rows(df_match_done,
                                    df_match_tbd)
      
      ########### Eliminatory matches of Current season ############
      url <-
        glue(
          "https://www.uol.com.br/esporte/futebol/campeonatos/libertadores/{season}/#quartas-de-final"
        )
      
      # Getting only tables with matches
      results <- url %>%
        read_html() %>%
        html_nodes(".step.type-3, .step.type-4")
      
      # Only round of 16 ahead
      # stopifnot(df_match_tbd = length(results) > 3)
      if (length(results) > 3) {
        results <- results[4:length(results)]
        
        # Extract date and hour
        d <-
          results %>%
          html_nodes(".match-info") %>%
          html_text() %>%
          str_sub(8, 20) %>%
          str_remove_all("-") %>%
          str_replace("h", ":")
        
        # Treating format
        date <- paste0(str_sub(d, 1, 5), "/", season) %>%
          dmy()
        
        # Transform in Datetime
        date_time <- paste0(date,
                            str_sub(d, 6, 12) %>%
                              str_replace(": ", ":00"),
                            ":00") %>%
          as_datetime()
        
        # Getting home team name
        home_team <- results %>%
          html_nodes(".team.team-home .team-name") %>%
          html_text()
        
        # Getting away team name
        away_team <- results %>%
          html_nodes(".team.team-away .team-name") %>%
          html_text()
        
        # Getting home team goals
        home_goal <-  results %>%
          html_nodes(".team.team-home .team-score") %>%
          html_text()
        
        # Getting away team goals
        away_goal <- results %>%
          html_nodes(".team.team-away .team-score") %>%
          html_text()
        
        group_matches %>%
          bind_rows(
            .,
            bind_cols(
              datetime = date_time,
              
              home_team = home_team,
              
              away_team = away_team,
              
              home_goal = home_goal,
              
              away_goal = away_goal,
              
              season =  year(date_time %>% as.Date()),
              
              stage = if (length(results) == 1) {
                c(rep("round of 16", 16))
              } else {
                if (length(results) == 2) {
                  c(rep("round of 16", 16),
                    rep("quarterfinals", 8))
                  
                } else{
                  if (length(results) == 3) {
                    c(rep("round of 16", 16),
                      rep("quarterfinals", 8),
                      rep("semifinals", 4))
                  } else{
                    c(
                      rep("round of 16", 16),
                      rep("quarterfinals", 8),
                      rep("semifinals", 4),
                      "final"
                    )
                  }
                }
              }
            )
          )
        
      }
      else {
        group_matches
      }
    }
  }
}
