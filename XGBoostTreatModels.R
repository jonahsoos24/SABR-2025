## Models ##

train_model <- function(train_data, test_data, grids = 100) {
  
  # Select relevant columns and remove missing values
  mod_train <- train_data %>%
    dplyr::select(select_columns) %>%
    mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
    filter(!is.na(runs_scored_in_inning_after))
  
  mod_test <- test_data %>%
    dplyr::select(select_columns) %>%
    mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
    filter(!is.na(runs_scored_in_inning_after))
  
  message("Model Data Created")
  
  # Compute class weights (inverse frequency)
  class_counts <- table(mod_train$runs_scored_in_inning_after)
  total_samples <- nrow(mod_train)
  num_classes <- length(class_counts)
  
  class_weights <- total_samples / (num_classes * class_counts)
  weight_vector <- class_weights[as.character(mod_train$runs_scored_in_inning_after)]
  
  # Convert data to XGBoost DMatrix with class weights
  dtrain <- xgb.DMatrix(
    data = as.matrix(mod_train %>% dplyr::select(-runs_scored_in_inning_after)), 
    label = mod_train$runs_scored_in_inning_after,
    weight = weight_vector
  )
  
  dtest <- xgb.DMatrix(
    data = as.matrix(mod_test %>% dplyr::select(-runs_scored_in_inning_after)), 
    label = mod_test$runs_scored_in_inning_after
  )
  
  message("Data Converted to DMatrix")
  
  # Set initial parameters for tuning
  test_eta = 0.2
  param <- list(
    eta = test_eta,
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = length(unique(mod_train$runs_scored_in_inning_after)) + 1
    # scale_pos_weight = class_weights
  )
  
  message("Initial Tuning to find Rounds:ETA ratio")
  
  # Cross-validation to find optimal number of rounds
  xgbcv <- xgb.cv(
    params = param,
    data = dtrain,
    nrounds = 200,
    nfold = 5, 
    showsd = TRUE,
    print_every_n = 1,
    early_stopping_rounds = 30,
    maximize = FALSE
  )
  
  n_rounds <- min(xgbcv$best_ntreelimit)
  message("Optimal number of rounds is ", n_rounds, " for ", test_eta)
  
  # Preprocessing recipe
  rec <- recipe(runs_scored_in_inning_after ~ ., data = mod_train) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_impute_median(all_numeric_predictors())
  
  message("Recipe Created")
  
  # Create cross-validation folds
  folds <- rsample::vfold_cv(mod_train, strata = runs_scored_in_inning_after, v = 3)
  
  message("Folds Created")
  
  # Define XGBoost model
  xgb_spec <- boost_tree(
    trees = n_rounds,
    min_n = tune(),
    mtry = tune(),
    tree_depth = tune(),
    loss_reduction = tune(),
    learn_rate = test_eta
  ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
  # Define workflow
  xgb_wf <- workflow(rec, xgb_spec)
  
  message("Workflow set, time to start tuning (this may take a while).")
  message(Sys.time())
  
  doParallel::registerDoParallel()
  
  # Hyperparameter tuning with ANOVA-based racing
  xgb_rs <- tune_race_anova(
    xgb_wf,
    resamples = folds,
    grid = grids,
    metrics = metric_set(roc_auc, mn_log_loss),
    control = control_race(verbose_elim = TRUE, allow_par = TRUE, burn_in = 2)
  )
  
  # Stop Parallel Processing
  parallelStop()
  
  message("Finished!")
  message(Sys.time())
  
  return(xgb_rs)
}

## Function to find optimal ETA & Rounds ##
optimize_rounds <- function(train_data, test_data, depth, tuned_model, n_rounds) {
  depth <- as.numeric(depth)
  
  # With ETA & Rounds, decrease eta and increase rounds to find optimized numbers for both
  learning_vals <- data.frame(
    eta = 1 / (10 * ((1:depth))),
    nrounds = (10 * (n_rounds) * (1:depth))
  )
  
  # Set up model once again
  mod_train <- train_data %>%
    dplyr::select(select_columns) %>%
    mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
    filter(!is.na(runs_scored_in_inning_after))
  
  mod_test <- test_data %>%
    dplyr::select(select_columns) %>%
    mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
    filter(!is.na(runs_scored_in_inning_after))
  
  # Compute class weights (inverse frequency)
  class_counts <- table(mod_train$runs_scored_in_inning_after)
  total_samples <- nrow(mod_train)
  num_classes <- length(class_counts)
  class_weights <- total_samples / (num_classes * class_counts)
  weight_vector <- class_weights[as.character(mod_train$runs_scored_in_inning_after)]
  
  # Recipe (Model function)
  rec <-
    recipe(runs_scored_in_inning_after ~ .,
           data = mod_train
    ) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_impute_median(all_numeric_predictors())
  
  message("Recipe Created")
  
  folds <- rsample::vfold_cv(mod_train, strata = runs_scored_in_inning_after, v = 3) # 3 CVs
  
  message("Start optimizing rounds (This may take a while)")
  message(Sys.time())
  
  doParallel::registerDoParallel()
  
  # Initialize progress counter
  progress_counter <- 0
  total_iterations <- nrow(learning_vals)
  
  expand_mod <- function(eta, nrounds) {
    # Track progress
    progress_counter <<- progress_counter + 1
    
    xgb_spec <-
      boost_tree(
        trees = nrounds,
        min_n = tuned_model$min_n,
        mtry = tuned_model$mtry,
        tree_depth = tuned_model$tree_depth,
        loss_reduction = tuned_model$loss_reduction,
        learn_rate = eta
      ) %>%
      set_engine("xgboost") %>%
      set_mode("classification")  # Changed to classification mode
    
    xgb_wf <- workflow(rec, xgb_spec)
    
    mod <- fit(xgb_wf, data = mod_train)  # Directly fitting the model
    
    message("Model Done Fitting")
    message(Sys.time())
    
    # Ensure predictions are a dataframe
    pred_probs <- as.data.frame(predict(mod, mod_test, type = "prob"))
    
    # Rename columns to match actual class levels
    colnames(pred_probs) <- levels(mod_test$runs_scored_in_inning_after)
    
    # Compute log loss
    test <- cbind(mod_test, pred_probs)
    val <- mod_test %>% dplyr::select(runs_scored_in_inning_after, '0':'5') %>%
      mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after))
    
    log_loss_val <- yardstick::mn_log_loss_vec(
      truth = val$runs_scored_in_inning_after,  # Explicitly reference the truth column
      estimate = as.matrix(val %>% select(-runs_scored_in_inning_after)),  # Select all columns except truth
    )
    
    # Print progress
    message(sprintf("Iteration %d/%d - ETA: %.4f, Rounds: %d, Log Loss: %.6f",
                    progress_counter, total_iterations, eta, nrounds, log_loss_val))
    
    # DF with results
    result_df <- data.frame(eta = eta, rounds = nrounds, log_loss = log_loss_val)
    
    return(result_df)
  }
  
  message("Running nround tuning, this may take a while")
  
  # Run the function for each combination of eta and rounds
  results <- pmap_dfr(learning_vals, expand_mod)
  
  return(results)
}

## Final Model Fitting ##
fit_final_model <- function(train_data, test_data, model_results, iteration, tuned_model) {
  
  # Set up model once again
  mod_train <- train_data %>% dplyr::select(select_columns) %>%
    mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
    filter(!is.na(runs_scored_in_inning_after))
  
  mod_test <- test_data %>% dplyr::select(select_columns) %>%
    mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
    filter(!is.na(runs_scored_in_inning_after))
  
  # Compute class weights (inverse frequency)
  class_counts <- table(mod_train$runs_scored_in_inning_after)
  total_samples <- nrow(mod_train)
  num_classes <- length(class_counts)
  class_weights <- total_samples / (num_classes * class_counts)
  weight_vector <- class_weights[as.character(mod_train$runs_scored_in_inning_after)]
  
  # Recipe (Model function)
  rec <-
    recipe(runs_scored_in_inning_after ~ .,
           data = mod_train
    ) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_impute_median(all_numeric_predictors())
  
  message("Recipe Created")
  
  # Gather best result and parameters
  best_result <- model_results[iteration, ]
  best_eta <- best_result$eta
  best_rounds <- best_result$rounds
  
  message("Fitting Final Model")
  message(Sys.time())
  
  doParallel::registerDoParallel()
  
  xgb_spec <- boost_tree(
    trees = best_rounds,
    min_n = tuned_model$min_n,
    mtry = tuned_model$mtry,
    tree_depth = tuned_model$tree_depth,
    loss_reduction = tuned_model$loss_reduction,
    learn_rate = best_eta
  ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")  # Changed to classification mode
  
  xgb_wf <- workflow(rec, xgb_spec)
  
  mod <- fit(xgb_wf, data = mod_train)  # Directly fitting the model
  
  # Extract log loss on test set
  preds <- predict(mod, mod_test, type = "prob") %>%
    bind_cols(mod_test %>% dplyr::select(runs_scored_in_inning_after))
  
  # Calculate multi-class log loss
  log_loss_val <- mlr3::logloss(preds, mod_test$runs_scored_in_inning_after)
  
  message("Final Log Loss: ", log_loss_val)
  message(Sys.time())
  
  return(list(model = mod, test_preds = preds))
}

calculate_shap_values <- function(mod, data) {
  
  # Extract the booster
  booster <- extract_fit_engine(mod)
  
  # Make sure all features here are included in shap_df
  names <- colnames(mod$pre$mold$predictors)
  
  # Manipulate the df
  shap_df <- data %>% dplyr::select(select_columns) %>%
    mutate(runs_scored_in_inning_after = as.factor(runs_scored_in_inning_after)) %>%
    filter(!is.na(runs_scored_in_inning_after))
  
  # Use shapviz with the xgboost model and feature data to get the shap values
  sv <- shapviz::shapviz(object = booster, X_pred = as.matrix(shap_df), interactions = FALSE)
  
  # Create the Beeswarm visualization of Shap Values
  p2 <- shapviz::sv_importance(sv, kind = "beeswarm") + theme_bw() +
    labs(title = "Parameter Impact on Probability of a Whiff",
         y = "Features", x = "Impact (From the Median)") +
    theme(
      axis.title.y = element_text(face = "bold", size = 14),
      axis.title.x = element_text(face = "bold", size = 14),
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
    ) #+
  # annotate("text", y = 1, x = -1.5, 
  #          label = bquote(bold("High Whiff Probability")), size = 3) +
  # annotate("text", y = 1, x = 0.7, 
  #          label = bquote(bold("Low Whiff Probability")), size = 3)
  return(p2)
}
