#-------- Fix Home and Away team names

fix_names <- function(df) {
  df %>%
    dplyr::mutate_at(
      dplyr::vars(home_team, away_team),
      ~ stringi::stri_trans_general(., 'Latin-ASCII')
    ) %>%
    dplyr::mutate(
      home_team = paste0(home_team, "-", home_team_state),

      away_team = paste0(away_team, "-", away_team_state)

    ) %>%
    dplyr::mutate_at(
      dplyr::vars(home_team, away_team),

      ~ dplyr::case_when(
        stringr::str_detect(., "Athl|Atletico Para")
        ~ "Atletico-PR",

        stringr::str_detect(., "America") &
          stringr::str_detect(., "MG")
        ~ "America-MG",

        stringr::str_detect(., "Atletico") &
          stringr::str_detect(., "MG")
        ~ "Atletico-MG",

        stringr::str_detect(., "Cuiaba")
        ~ "Cuiaba-MT",

        TRUE ~ .
      )
    )
}
