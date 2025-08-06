dwp <- read_csv("danger_w_pitchers.csv")

danger <- dwp %>%
  mutate(
    pressure_scoring_0 = ifelse(pressure_scoring_0 < -0.05, -0.05, pressure_scoring_0),
    danger =
      (`X0` * pressure_scoring_0) +
      (`X1` * pressure_scoring_1) +
      (`X2` * pressure_scoring_2) +
      (`X3` * pressure_scoring_3) +
      (`X4` * pressure_scoring_4) +
      (`X5` * pressure_scoring_5),
    threat = 
      (`X0` * 0) + 
      (`X1` * 1) + 
      (`X2` * 2) + 
      (`X3` * 3) + 
      (`X4` * 4) +
      (`X5` * 5),
    pressure = 
      case_when(
        abs(pitching_team_score_diff) >= 5 ~ pressure_scoring_5 / abs(pitching_team_score_diff),
        abs(pitching_team_score_diff) == 4 ~ pressure_scoring_4 / 4,
        abs(pitching_team_score_diff) == 3 ~ pressure_scoring_3 / 3,
        abs(pitching_team_score_diff) == 2 ~ pressure_scoring_2 / 2,
        abs(pitching_team_score_diff) == 1 ~ pressure_scoring_1 / 1,
        abs(pitching_team_score_diff) == 0 ~ pressure_scoring_1 / 1,
        TRUE ~ NA_integer_
      )
  ) 

hero_inn <- danger %>%
  group_by(game_year, game_pk, inning, pitcher) %>%
  mutate(dWP = last(total_WP_pitching_team_after) - first(total_WP_pitching_team_before),
         hero = dWP - first(danger)) %>%
  ungroup() %>%
  dplyr::select(game_year, game_pk, pitcher, player_name, inning, at_bat_number, outs_before, base_state, pitching_team_score_diff, runs_scored_in_inning_after, threat, pressure, momentum, danger, hero) %>%
  mutate(
    threat_plus = 100 + (threat - mean(threat, na.rm = TRUE)) / ((sd(threat, na.rm = TRUE)) / 10),
    pressure_plus = 100 - (pressure - mean(pressure, na.rm = TRUE)) / ((sd(pressure, na.rm = TRUE)) / 10),
    momentum_plus = 100 + (momentum - mean(momentum, na.rm = TRUE)) / ((sd(momentum, na.rm = TRUE)) / 10),
    danger_plus = 100 - (danger - mean(danger, na.rm = TRUE)) / ((sd(danger, na.rm = TRUE)) / 10),
    hero_plus = 100 + (hero - mean(hero, na.rm = TRUE)) / ((sd(hero, na.rm = TRUE)) / 10)
  )
write_csv(hero_inn, "hero_by_ab.csv")

hero_app_calc <- function(hero_inn) {
  hero_summary <- hero_inn %>%
    group_by(game_year, game_pk, pitcher, player_name) %>%
    summarise(
      batters_faced = n(),
      enter_outs = first(outs_before),
      enter_state = first(base_state),
      enter_inning = first(inning),
      exit_inning = last(inning),
      enter_score_dif = first(pitching_team_score_diff),
      exit_score_dif = last(pitching_team_score_diff),
      runs_allowed = max(runs_scored_in_inning_after),
      across(c(threat:danger, threat_plus:danger_plus), ~first(.x)),
    ) %>%
    ungroup()
  
  hero_aggregated <- hero_inn %>%
    select(game_year, game_pk, pitcher, player_name, hero, hero_plus) %>%
    distinct() %>%
    group_by(game_year, game_pk, pitcher) %>%
    summarise(
      hero = sum(hero, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      hero_plus = 100 + (hero - mean(hero, na.rm = TRUE)) / ((sd(hero, na.rm = TRUE)) / 10)
    )
  
  hero_app <- hero_summary %>%
    left_join(hero_aggregated, by = c("game_year", "game_pk", "pitcher"))
  
  return(hero_app)
}

hero_app <- hero_app_calc(hero_inn)

write_csv(hero_app, "hero_by_app.csv")

hero_year <- hero_app %>%
  select(game_year, pitcher, player_name,
         threat:danger_plus,
         hero) %>%
  group_by(pitcher, player_name, game_year) %>%
  summarise(
    appearences = n(),
    across(c(threat:danger_plus), ~ mean(.x, na.rm = TRUE)),
    hero = sum(hero, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    hero_plus = 100 + (hero - mean(hero, na.rm = TRUE)) / ((sd(hero, na.rm = TRUE)) / 10)
  )

write_csv(hero_year, "hero_by_year.csv")