DecisionTree <- function(df, treeDepth = 3) {
  # Implementation from Datacamp
  # https://www.datacamp.com/tutorial/decision-trees-R
  salesData = df %>%
    rsample::initial_split(.)
  
  tree_spec = parsnip::decision_tree(tree_depth = treeDepth) %>%
    parsnip::set_engine("rpart") %>%
      parsnip::set_mode("regression")
  
  # Fit the model to the training data
  tree_fit = tree_spec %>%
    parsnip::fit(y ~ ., data = rsample::training(salesData))
  
  # Make predictions on the testing data
  predictions = tree_fit %>%
    predict(rsample::testing(salesData)) %>%
      dplyr::pull(.pred)
  
  # Calculate RMSE and R-squared
  metrics = yardstick::metric_set(yardstick::rmse, yardstick::rsq)
  model_performance = rsample::testing(salesData) %>%
    dplyr::mutate(predictions = predictions) %>%
      yardstick::metrics(truth = y, estimate = predictions) %>%
        dplyr::mutate(tree_depth = treeDepth)
  
  list(tree_fit, predictions, model_performance)
}

AnalyzeTree <- function(tree, df) {
  pred = predict(tree, newdata = df)
  results = yardstick::metrics(cbind(df, pred), truth = y, estimate = pred)
  
  list(tree, pred, results)
}
