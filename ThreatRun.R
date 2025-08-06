library(tidyverse)
library(xgboost)
library(mlr)
library(parallel)
library(parallelMap)
library(shapviz)
library(tidymodels)
library(xgboost)
library(dplyr)
library(rsample)
library(purrr)
library(doParallel)
library(ordinalForest)
library(finetune)
library(explore)
library(vip)
library(usemodels)
library(ranger)
library(textrecipes)
library(tidymodels)

data <- read_csv("threat_plus_pressure_MAR102.csv")

## Apply Marginal Effects ##
fd <- data %>%
  rename(pe = 'Park Factor') %>%
  left_join(pa_since_me, by=c("pa_since_last_run")) %>%
  rename(momentum_runs = marginal_effect) %>%
  left_join(hits_me, by=c("pa_since_last_hit")) %>%
  rename(momentum_hits = marginal_effect) %>%
  mutate(
    runs_scored_in_inning_after = ifelse(runs_scored_in_inning_after > 5, 5, runs_scored_in_inning_after),
    momentum_hits = ifelse(is.na(momentum_hits), 0, momentum_hits),
    momentum_runs = ifelse(is.na(momentum_runs), 0, momentum_runs),
    momentum = momentum_hits + momentum_runs,
    same_hand = case_when(
      p_throws == "R" & stand == "R" ~ 1,
      p_throws == "L" & stand == "L" ~ 1,
      TRUE ~ 0
    )
  )

train <- fd %>%
  mutate(season = year(game_date)) %>%
  filter(season <= 2023)

test <- fd %>%
  mutate(season = year(game_date)) %>%
  filter(season >= 2023)

select_columns <- c(
  "runs_scored_in_inning_after",
  "outs_before", "on_1b", "on_2b", "on_3b", "same_hand", "pe",
  "weighted_rolling_xwoba",
  "momentum"
)

#### Classification XGBoost ####

# Tune Model
xgthreat_tune <- train_model(train, test, 100)

# Review the tune
plot_race(xgthreat_tune)
show_best(xgthreat_tune, metric = "mn_log_loss")

# Select Best Model
mod_train <- train %>%
  dplyr::select(select_columns) %>%
  mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
  filter(!is.na(runs_scored_in_inning_after))

mod_test <- test %>%
  dplyr::select(select_columns) %>%
  mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
  filter(!is.na(runs_scored_in_inning_after))

xgthreat_best <- select_best(xgthreat_tune, metric = "mn_log_loss")

rec <-
  recipe(runs_scored_in_inning_after ~ .,
         data = mod_train
  ) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_impute_median(all_numeric_predictors())

base <- xgb_spec <-
  boost_tree(
    trees = 50,
    min_n = 30,
    mtry = 4,
    tree_depth = 5,
    loss_reduction = 0.0000104,
    learn_rate = 0.2
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")  # Changed to classification mode

xgb_wf <- workflow(rec, xgb_spec)

base_xgboost <- fit(xgb_wf, data = mod_train)

pred_probs <- as.data.frame(predict(base_xgboost, mod_test, type = "prob"))
colnames(pred_probs) <- levels(mod_test$runs_scored_in_inning_after)

test <- cbind(test, pred_probs)
val <- test %>% dplyr::select(runs_scored_in_inning_after, '0':'5') %>%
  mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after))
val %>% head(5)

log_loss_val <- yardstick::mn_log_loss_vec(
  truth = val$runs_scored_in_inning_after,  # Explicitly reference the truth column
  estimate = as.matrix(val %>% select(-runs_scored_in_inning_after)),  # Select all columns except truth
)

#### Shap Values ####

# Extract the booster
booster <- extract_fit_engine(base_xgboost)

# Make sure all features here are included in shap_df
names <- colnames(base_xgboost$pre$mold$predictors)

shap_df <- train[, names]
shap_test <- test[, names]

sv <- shapviz::shapviz(
  object = booster, 
  X_pred = as.matrix(shap_df),
  X = shap_df)

sv_test <- shapviz::shapviz(
  object = booster, 
  X_pred = as.matrix(shap_test),
  X = shap_test)

# Create a vector of class names that correspond to the probability of runs scored
class_labels <- c("0 Runs Scored", "1 Run Scored", "2 Runs Scored", 
                  "3 Runs Scored", "4 Runs Scored", "5 Runs Scored")

new_feature_names <- c("Outs", "On 1st Base", "On 2nd Base", "On 3rd Base", 
                       "Same Hand", "PE", "Batter Skill", "Momentum")

# Create a list of the SHAP matrices for each class
shap_data <- lapply(1:6, function(class_num) {
    # Import the SHAP importance for the given class
    sv_class <- shapviz::sv_importance(sv[[paste0("Class_", 0)]], kind = "beeswarm")
    
    # Extract original feature names
    original_features <- unique(sv_class$data$feature)
    
    # Ensure `new_feature_names` is in the correct order by matching original features
    feature_name_mapping <- c(
      "outs_before" = "Outs",
      "on_1b" = "On 1st Base",
      "on_2b" = "On 2nd Base",
      "on_3b" = "On 3rd Base",
      "same_hand" = "Same Hand",
      "pe" = "PE",
      "weighted_rolling_xwoba" = "Batter Skill",
      "momentum" = "Momentum"
    )
    
    # Rename the variables/features in the plot while keeping the correct mapping
    # sv_class$data$feature <- factor(sv_class$data$feature, 
    #                                 levels = original_features, 
    #                                 labels = feature_name_mapping)
    
    # Calculate feature importance (mean absolute SHAP values)
    # feature_importance <- aggregate(abs(sv_class$data$value), 
    #                                 by = list(feature = sv_class$data$feature), 
    #                                 FUN = mean)
    
    # Order features by importance (descending)
    #ordered_features <- feature_importance$feature[order(feature_importance$x)]
    
    # Reorder factor levels based on importance ranking
    #sv_class$data$feature <- factor(sv_class$data$feature, levels = ordered_features)
    
    # Add the class label to the plot
    sv_class <- sv_class + 
      scale_y_discrete(labels = feature_name_mapping) +
      labs(title = class_labels[class_num], y = "Features", x = "Impact (From the Mean)") +
      theme_bw() +
      theme(
        axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
      )
    
    return(sv_class)
})

library(patchwork)
combined_plot <- wrap_plots(shap_data, ncol = 2)
shap_data[[2]]

predict_player <- function(game_pk, at_bat_number) {
  
  feature_name_mapping <- c(
    "outs_before" = "Outs",
    "on_1b" = "On 1st Base",
    "on_2b" = "On 2nd Base",
    "on_3b" = "On 3rd Base",
    "same_hand" = "Same Hand",
    "pe" = "PE",
    "weighted_rolling_xwoba" = "Batter Skill",
    "momentum" = "Momentum"
  )
  
  # Extract SHAP values for the selected at-bat
  sv_test1 <- test %>%
    select(game_pk, at_bat_number) %>%
    cbind(as.data.frame(sv_test$Class_1$S)) %>%
    filter(game_pk == 746370 & at_bat_number == 37) %>%
    pivot_longer(-c(game_pk, at_bat_number), names_to = "Feature", values_to = "SHAP_Value")
  
  sv_test2 <- test %>%
    select(game_pk, at_bat_number) %>%
    cbind(as.data.frame(sv_test$Class_2$S)) %>%
    filter(game_pk == 746370 & at_bat_number == 37) %>%
    pivot_longer(-c(game_pk, at_bat_number), names_to = "Feature", values_to = "SHAP_Value")
  
  sv_test3 <- test %>%
    select(game_pk, at_bat_number) %>%
    cbind(as.data.frame(sv_test$Class_3$S)) %>%
    filter(game_pk == 746370 & at_bat_number == 37) %>%
    pivot_longer(-c(game_pk, at_bat_number), names_to = "Feature", values_to = "SHAP_Value")
  
  # Define a function to create a waterfall plot
  plot_waterfall <- function(data, title) {
    ggplot(data, aes(x = SHAP_Value, y = reorder(Feature, SHAP_Value), fill = SHAP_Value > 0)) +
      scale_y_discrete(labels = feature_name_mapping) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("red", "blue")) +
      labs(title = title, y = "Features", x = "Impact (From the Mean)") +
      theme_bw() +
      theme(
        axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 11, colour = "black"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
      )
  }
  
  plot1 <- plot_waterfall(sv_test1, "Probability of 0 Runs")
  plot2 <- plot_waterfall(sv_test2, "Probability of 1 Runs")
  plot3 <- plot_waterfall(sv_test3, "Probability of 2 Runs")
  
  # Display plots side by side
  p <- plot1 + plot2 + plot3
  
  print(p)
  
  return(p)
}

# Example usage:
predict_player(game_pk = 746370, at_bat_number = 37)

## Plot Distributions ##
graph_dist <- function () {
  
  tl <- test %>%
    select(`0`:`5`) %>%
    pivot_longer(cols = `0`:`5`, names_to = "Predicted_Value", values_to = "Prediction")
  
  # Calculate the true average occurrences (mean) for each Predicted_Value
  true_avg <- test %>%
    group_by(runs_scored_in_inning_after) %>%
    summarise(True_Avg = n() / nrow(test), .groups = "drop") %>%
    rename(Predicted_Value = runs_scored_in_inning_after)
  
  # Create ggplot
  facet_labels <- c("0" = "0 Runs", "1" = "1 Run", "2" = "2 Runs", 
                    "3" = "3 Runs", "4" = "4 Runs", "5" = "5+ Runs")
  
  dist_plot <- ggplot(tl, aes(x = Prediction, fill = as.factor(Predicted_Value))) +
    geom_density(alpha = 0.5) +  # Show distribution with transparency
    geom_vline(data = true_avg, aes(xintercept = True_Avg), 
               linetype = "dashed", color = "black", size = 1) +  # Single vertical line per facet
    facet_wrap(~Predicted_Value, scales = "free", labeller = labeller(Predicted_Value = facet_labels)) +  
    labs(title = "Distribution of Predicted Values with True Averages",
         x = "Predicted Value", y = "Density") +
    scale_fill_discrete(name = "Predicted Runs") + 
    theme_bw() +
    theme(
      strip.text = element_text(size = 12),  # Adjust title size for facets
      axis.title.y = element_text(face = "bold", size = 14),
      axis.title.x = element_text(face = "bold", size = 14),  # Axis labels
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Centered title
    )
    #scale_color_manual(values = rep("red", length(unique(true_avg$runs_scored_in_inning_after))))  # Keep line color red
  
  print(dist_plot)
}

graph_dist()

## Create Danger ##
dwp <- read_csv("danger_w_pitchers.csv")

pbp_2024 <- read_csv("pbp_2024.csv")
pbp_2023 <- read_csv("pbp_2023.csv")

pbp_2024_pitcher <- pbp_2024[, c("game_pk", "at_bat_number", "batter", "pitcher", "player_name", "game_year")]
pbp_2023_pitcher <- pbp_2023[, c("game_pk", "at_bat_number", "batter", "pitcher", "player_name", "game_year")]
players <- rbind(pbp_2024_pitcher, pbp_2023_pitcher) %>% distinct()

test2 <- test %>% left_join(players,
                            by=c("game_pk",
                            "at_bat_number",
                            "batter",
                            "game_year"))
rm(players, pbp_2023, pbp_2023_pitcher, pbp_2024, pbp_2024_pitcher)


danger <- test2 %>%
  # left_join(players,
  #           by=c("game_pk",
  #                "at_bat_number",
  #                "batter",
  #                "game_year")) %>%
  mutate(
    pressure_scoring_0 = ifelse(pressure_scoring_0 < -0.05, -0.05, pressure_scoring_0),
    danger =
      (`0` * pressure_scoring_0) +
      (`1` * pressure_scoring_1) +
      (`2` * pressure_scoring_2) +
      (`3` * pressure_scoring_3) +
      (`4` * pressure_scoring_4) +
      (`5` * pressure_scoring_5),
    threat = 
      (`0` * 0) + 
      (`1` * 1) + 
      (`2` * 2) + 
      (`3` * 3) + 
      (`4` * 4) +
      (`5` * 5),
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

get_roles <- function(data) {
  data %>% group_by(pitcher) %>% 
    mutate(season_batters_faced = n()) %>% ungroup() -> data
  
  data %>% group_by(pitcher) %>% 
    mutate(season_appearances = n_distinct(game_pk)) -> data
  
  data %>% mutate(bf_per_app = season_batters_faced / season_appearances) -> data
  
  data %>% group_by(pitcher) %>% 
    summarize(pitcher_name = first(player_name), 
              avg_bf_per_app = first(bf_per_app), 
              total_appearances = first(season_appearances)) -> avg_bf_pitchers
  
  avg_bf_pitchers %>% mutate(role = if_else(avg_bf_per_app < 15, "RP", "SP")) -> pitcher_roles
}

proles <- get_roles(danger)
danger <- danger %>%
  left_join(proles)

hero_inn <- danger %>%
  group_by(game_year, game_pk, inning, pitcher) %>%
  mutate(
    is_winning = ifelse(pitching_team_score_diff > 0, 1, 0),
    is_tied = ifelse(pitching_team_score_diff == 0, 1, 0),
    is_losing = ifelse(pitching_team_score_diff < 0, 1, 0),
    dWP = last(total_WP_pitching_team_after) - first(total_WP_pitching_team_before),
    hero = dWP - first(danger)) %>%
  ungroup() %>%
  dplyr::select(game_year, game_pk, pitcher, player_name, role, inning, inning_topbot, at_bat_number, 
                outs_before, base_state, pitching_team_score_diff, runs_scored_in_inning_after, 
                is_winning, is_tied, is_losing,
                threat, pressure, momentum, danger, hero) %>%
  group_by(game_year) %>%
  mutate(
    threat_plus = 100 + (threat - mean(threat, na.rm = TRUE)) / ((sd(threat, na.rm = TRUE)) / 10),
    pressure_plus = 100 - (pressure - mean(pressure, na.rm = TRUE)) / ((sd(pressure, na.rm = TRUE)) / 10),
    momentum_plus = 100 + (momentum - mean(momentum, na.rm = TRUE)) / ((sd(momentum, na.rm = TRUE)) / 10),
    danger_plus = 100 - (danger - mean(danger, na.rm = TRUE)) / ((sd(danger, na.rm = TRUE)) / 10),
    hero_plus = 100 + (hero - mean(hero, na.rm = TRUE)) / ((sd(hero, na.rm = TRUE)) / 10)
  ) %>%
  ungroup()

write_csv(hero_inn, "hero_by_ab.csv")

ggplot(hero_inn, aes(x = hero)) +
  geom_density(alpha = 0.5, fill = "#f76900ff") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Corrected
  geom_vline(data = hero_inn, aes(xintercept = mean(hero, na.rm = TRUE)), 
             linetype = "dashed", color = "red", size = 1) +  # Added missing +
  labs(title = "Distribution of Inning by Inning HERO",
       x = "HERO", y = "Density") +
  theme_bw() +
  xlim(-1, 1) +
  theme(
    strip.text = element_text(size = 12),  
    axis.title.y = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 14),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
)


hero_app_calc <- function(hero_inn) {
  hero_summary <- hero_inn %>%
    group_by(game_year, game_pk, pitcher, player_name, role) %>%
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
    select(game_year, game_pk, pitcher, player_name, role, inning,
           is_winning, is_tied, is_losing,
           hero, hero_plus) %>%
    group_by(game_year, game_pk, pitcher, player_name, role, inning) %>%
    summarise(
      is_winning = first(is_winning),
      is_tied = first(is_tied),
      is_losing = first(is_losing),
      hero = first(hero)
    ) %>%
    distinct() %>%
    group_by(game_year, game_pk, pitcher) %>%
    summarise(
      is_winning_innings = sum(is_winning, na.rm = TRUE),  # Changed from n()[is_winning == 1]
      is_tied_innings = sum(is_tied, na.rm = TRUE),        # Changed from n()[is_tied == 1]
      is_losing_innings = sum(is_losing, na.rm = TRUE),
      hero_save = sum(hero[is_winning == 1], na.rm = TRUE),
      hero_hold = sum(hero[is_tied == 1], na.rm = TRUE),
      hero_losing = sum(hero[is_losing == 1], na.rm = TRUE),
      hero_total = sum(hero, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(game_year) %>%
    mutate(
      hero_plus = 100 + (hero_total - mean(hero_total, na.rm = TRUE)) / ((sd(hero_total, na.rm = TRUE)) / 10)
    ) %>%
    ungroup()
  
  hero_app <- hero_summary %>%
    left_join(hero_aggregated, by = c("game_year", "game_pk", "pitcher"))
  
  return(hero_app)
}

hero_app <- hero_app_calc(hero_inn)

ggplot(hero_app, aes(x = hero_total)) +
  geom_density(alpha = 0.5, fill = "#041e42ff") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Corrected
  geom_vline(data = hero_app, aes(xintercept = mean(hero_total, na.rm = TRUE)), 
             linetype = "dashed", color = "#f76900ff", size = 1) +  # Added missing +
  labs(title = "Distribution of Inning by Appearance HERO",
       x = "HERO", y = "Density") +
  theme_bw() +
  xlim(-1, 1) +
  theme(
    strip.text = element_text(size = 12),  
    axis.title.y = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 14),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

write_csv(hero_app, "hero_by_app.csv")

hero_year <- hero_app %>%
  select(game_year, pitcher, player_name, role,
         threat:hero_plus) %>%
  group_by(pitcher, player_name, game_year, role) %>%
  summarise(
    appearences = n(),
    across(c(is_winning_innings:is_losing_innings), sum, na.rm = TRUE),
    across(c(threat:danger), mean, na.rm = TRUE),
    across(c(hero_save:hero_total), sum, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  filter(role == "RP" & appearences >= 15) %>%
  group_by(game_year) %>%
  mutate(
    hero_win_rate = hero_save / is_winning_innings,
    hero_tied_rate = hero_hold / is_tied_innings,
    hero_losing_rate = hero_losing / is_losing_innings,
    threat_plus = 100 + (threat - mean(threat, na.rm = TRUE)) / ((sd(threat, na.rm = TRUE)) / 10),
    pressure_plus = 100 - (pressure - mean(pressure, na.rm = TRUE)) / ((sd(pressure, na.rm = TRUE)) / 10),
    momentum_plus = 100 + (momentum - mean(momentum, na.rm = TRUE)) / ((sd(momentum, na.rm = TRUE)) / 10),
    danger_plus = 100 - (danger - mean(danger, na.rm = TRUE)) / ((sd(danger, na.rm = TRUE)) / 10),
    hero_save_plus = 100 + (hero_save - mean(hero_save, na.rm = TRUE)) / ((sd(hero_save, na.rm = TRUE)) / 10),
    hero_hold_plus = 100 + (hero_hold - mean(hero_hold, na.rm = TRUE)) / ((sd(hero_hold, na.rm = TRUE)) / 10),
    hero_losing_plus = 100 + (hero_losing - mean(hero_losing, na.rm = TRUE)) / ((sd(hero_losing, na.rm = TRUE)) / 10),
    hero_plus = 100 + (hero_total - mean(hero_total, na.rm = TRUE)) / ((sd(hero_total, na.rm = TRUE)) / 10),
    hero_tpl = hero_hold + hero_losing,
    hero_tpl_plus = 100 + (hero_tpl - mean(hero_tpl, na.rm = TRUE)) / ((sd(hero_tpl, na.rm = TRUE)) / 10),
    reliever_role = case_when(
      pressure_plus >= 110 & threat_plus <= 100 ~ "Closer",
      #threat_plus >= 110 & pressure_plus <= 100 ~ "Relief Pitcher",
      pressure_plus >= 110 & threat_plus >= 110 ~ "Extinguisher",
      TRUE ~ "Relief Pitcher"
    )
  ) %>%
  ungroup()

hero_year %>% mutate(temp = (hero_total - hero_save) / appearences) %>% view()
write_csv(hero_year, "hero_by_year.csv")

# Function to plot radar chart
library(tidyverse)
library(ggplot2)

# Prepare the dataset
plot_df <- hero_year %>%
  filter(pitcher %in% c(661403, 657649) & game_year == 2024) %>%
  select(pitcher, momentum_plus, threat_plus, pressure_plus, danger_plus, 
         hero_save_plus, hero_hold_plus) %>%
  pivot_longer(cols = momentum_plus:hero_hold_plus, 
               names_to = "var", values_to = "Percentile") 

# Fix variable names
var_labels <- c("momentum_plus" = "Momentum+", "threat_plus" = "Threat+", 
                "pressure_plus" = "Pressure+", "danger_plus" = "Danger+", 
                "hero_save_plus" = "Hero Saves+", "hero_hold_plus" = "Hero Holds+")

plot_df$var <- factor(plot_df$var, levels = names(var_labels), labels = var_labels)

# Plot
ggplot(plot_df) +
  geom_hline(
    aes(yintercept = y),
    data.frame(y = c(80, 100, 120, 140, 160)),  
    color = "#2F241D", alpha = 0.5
  ) +
  geom_col(
    aes(x = var, y = Percentile, fill = factor(pitcher)),
    position = "identity", alpha = 0.9
  ) +
  scale_y_continuous(
    limits = c(min(plot_df$Percentile) - 5, max(plot_df$Percentile) + 5),
    expand = c(0, 0),
    breaks = seq(80, 160, by = 20)
  ) +
  scale_fill_manual(values = c("#FFC425", "#00A5CF")) +
  coord_polar(clip = "off") +
  theme_minimal() +
  
  # Bold 100 line
  geom_hline(yintercept = 100, color = "black", size = 1) +
  
  # Labels
  annotate("text", x = 1, y = 80, label = "80", size = 4, color = "#2F241D", fontface = "bold") +
  annotate("text", x = 2, y = 100, label = "100", size = 4, color = "#2F241D", fontface = "bold") +
  annotate("text", x = 3, y = 120, label = "120", size = 4, color = "#2F241D", fontface = "bold") +
  annotate("text", x = 4, y = 140, label = "140", size = 4, color = "#2F241D", fontface = "bold") +
  annotate("text", x = 5, y = 160, label = "160", size = 4, color = "#2F241D", fontface = "bold") +
  
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "#2F241D", size = 12),
    legend.position = "bottom",
    plot.margin = margin(.1, .1, .1, .1),
    panel.grid = element_blank()
  ) +
  labs(title = "Radar Chart Comparison", subtitle = "Standardized Percentiles for 2024", fill = "Pitcher")



# Example Usage:
plot_radar(hero_year, 661403, 657649)

ggplot(hero_year, aes(x = threat_plus, y = pressure_plus)) +
  geom_point()
  

comparison <- danger %>%
  dplyr::select(pitcher, player_name, role, inning, outs_before, base_state, pitching_team_score_diff, threat, pressure, momentum, danger, runs_scored_in_inning_after) %>%
  mutate(
    threat_plus = 100 + (threat - mean(threat, na.rm = TRUE)) / ((sd(threat, na.rm = TRUE)) / 10),
    pressure_plus = 100 - (pressure - mean(pressure, na.rm = TRUE)) / ((sd(pressure, na.rm = TRUE)) / 10),
    momentum_plus = 100 + (momentum - mean(momentum, na.rm = TRUE)) / ((sd(momentum, na.rm = TRUE)) / 10),
    danger_plus = 100 - (danger - mean(danger, na.rm = TRUE)) / ((sd(danger, na.rm = TRUE)) / 10)
  )

danger_data_2024 %>% group_by(game_pk, inning, pitcher, role) %>% 
  mutate(actual_delta_WP_inning = last(total_WP_pitching_team_after) - first(total_WP_pitching_team_before)) %>% 
  mutate(stat_inning_level = actual_delta_WP_inning - first(danger)) -> danger_data_2024

danger_data_2024 %>% group_by(game_pk, pitcher, role, inning) %>% 
  summarize(pitcher_name = first(player_name), stat_inning_level_for_sum = first(stat_inning_level)) %>% 
  mutate(stat_appearance_level = sum(stat_inning_level_for_sum)) %>% ungroup() -> inn_by_inn_leaderboard

inn_by_inn_leaderboard

# Appearance Level

inn_by_inn_leaderboard %>% group_by(game_pk, pitcher) %>% 
  summarize(pitcher_name = first(pitcher_name), stat_appearance_level = first(stat_appearance_level)) -> app_by_app_leaderboard

# Season Level

app_by_app_leaderboard %>% group_by(pitcher) %>% 
  summarize(pitcher_name = first(pitcher_name), stat_season_level = sum(stat_appearance_level)) -> season_leaderboard


write_csv(danger, "danger.csv")

danger %>%
  select(pitching_team_score_diff, inning, pressure_scoring_0) %>%
  distinct() %>%
  view()

## Robustness Tune ##

xgthreat_robust <- optimize_rounds(train, test, 5, xgthreat_best, 50)

ggplot(rtune, aes(x = rounds, y = rmse)) +
  geom_point() +
  geom_line()

# xgthreat_model <- fit_final_model(train, test, xgthreat_robust, , xgthreat_best)
# final_threat_model <- xgthreat_model$model

#### Ordinal Forest ####

# Use optimization to tune minbucket and maxdepth
counter <- 0  # To keep track of iteration number
nodes <- optim(c(10, 20), tune_nodes, control = list(maxit = 100))

# Extract the best parameters
min_obs <- ceiling(nodes$par[1])
max_depth <- ceiling(nodes$par[2])

# Print best node size and depth
print(paste("Minimum Node Size =", min_obs, 
            "Maximum Depth =", max_depth))

# Base level tree tuning
counter <- 0  # To keep track of iteration number
n_trees_overview <- seq(1000, 10000, by = 1000)
tree_test <- tibble(
  n_tree = n_trees_overview,
  loss = map_dbl(n_trees_overview, ~ tune_rf(.x))
)

# Plot logloss against number of trees
ggplot(tree_test, aes(x = n_tree, y = loss)) +
  geom_point() +
  geom_line()

rf <- tune_rf(5000)

save.image("tune2.RData")


# In-depth tree tuning
n_trees_seq <- seq(1, 200, by = 5)
tree_performance <- tibble(
  n_tree = n_trees_seq,
  loss = map_dbl(n_trees_seq, ~ trees(c(1, 200), .x))
)

# Plot in-depth tree tuning results
ggplot(tree_performance, aes(x = n_tree, y = loss)) +
  geom_point() +
  geom_line()
