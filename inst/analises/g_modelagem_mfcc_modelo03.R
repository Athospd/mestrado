set.seed(1)

tic("modelo03 tune")
# MFCCs
mfccs <- read_rds("data/mfcc_500ms.rds")

# dados
x_dim <- dim(mfccs[[1]])
y_dim <- dim(mfccs[[2]])
modelo03_df <- mfccs[[1]] %>% 
  rray::rray_reshape(dim = c(x_dim[1], prod(x_dim[-1]))) %>% 
  as.data.frame() %>%
  mutate(
    audio_id = str_remove_all(mfccs[[3]], "(@.*@)|(^.*/)"),
    slice_id = mfccs[[3]],
    y = as.factor(mfccs[[2]] %*% seq(y_dim[2]))
  )
write_rds(modelo03_df, "data/modelos_mfcc/modelo03_df.rds")

# Treino/Teste
audio_ids_train <- read_rds("data/audio_ids_train.rds")

modelo03_train <- modelo03_df %>% semi_join(audio_ids_train, by = "audio_id") %>% select(-audio_id, -slice_id)
modelo03_val <- modelo03_df %>% anti_join(audio_ids_train, by = "audio_id") %>% select(-audio_id, -slice_id)
rm(modelo03_df);gc()
write_rds(modelo03_train, "data/modelos_mfcc/modelo03_train.rds")
write_rds(modelo03_val, "data/modelos_mfcc/modelo03_val.rds")

# Receita
modelo03_recipe <- modelo03_train %>%
  recipe(y ~ .) %>%
  prep()
write_rds(modelo03_recipe, "data/modelos_mfcc/modelo03_recipe.rds")

# Modelo
modelo03_model <- boost_tree(
  trees = tune(), 
  mtry = tune(), 
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()
) %>%
  set_engine("xgboost", verbose = 1) %>%
  set_mode("classification")

# Workflow
modelo03_wflow <- workflow() %>%
  add_model(modelo03_model) %>%
  add_recipe(modelo03_recipe)

# Parâmetros
# modelo03_params <- parameters(modelo03_wflow, mtry = finalize(mtry(), modelo03_train %>% select(-y)))
set.seed(1)
modelo03_params <- expand_grid(
  mtry = c(20, 100),
  trees = c(1000, 1500),
  min_n = c(5, 10),
  tree_depth = c(10, 15, 20),
  learn_rate = c(0.001, 0.1),
  loss_reduction = c(1),
  sample_size = c(0.8)
) %>%
  sample_n(20)

# Amostras CV
modelo03_train_cv <- vfold_cv(modelo03_train, v = 5)

# Grid Search
set.seed(1)
doMC::registerDoMC(6)

modelo03_search_res <- tune_grid(
  modelo03_wflow, 
  resamples = modelo03_train_cv,
  grid = modelo03_params,
  metrics = metric_set(roc_auc, accuracy, recall, precision, kap),
  control = control_grid(verbose = TRUE, allow_par = TRUE)
)

# Best Model
tune::select_best(modelo03_search_res, metric = "accuracy")

modelo03_search_res_table <- modelo03_search_res %>%
  select(.metrics) %>%
  unnest_legacy() %>%
  select(-.estimator) %>%
  group_by_at(vars(-.estimate)) %>%
  summarise(
    estimate = mean(.estimate),
    se = sd(.estimate)/sqrt(n())
  )


modelo03_search_res_table %>%
  ggplot(aes(x = mtry, y = estimate, colour = .metric)) +
  geom_errorbar(aes(ymin = estimate - 2*se, ymax = estimate + 2*se)) +
  facet_grid(.metric ~ learn_rate + trees + tree_depth)

modelo03_best_params <- tune::select_best(modelo03_search_res, "kap")
write_rds(modelo03_search_res, "data/modelos_mfcc/modelo03_search_res.rds")
write_rds(modelo03_best_params, "data/modelos_mfcc/modelo03_best_params.rds")

toc()




# Final Model --------------------------------------------------------------------------------
tic("modelo03 final fit")
# data
modelo03_best_params <- read_rds("data/modelos_mfcc/modelo03_best_params.rds")
modelo03_model_final <- do.call(boost_tree, as.list(modelo03_best_params)) %>%
  set_engine("xgboost", verbose = 1) %>%
  set_mode("classification")

modelo03_recipe <- read_rds("data/modelos_mfcc/modelo03_recipe.rds")
modelo03_wflow_final <- workflow() %>%
  add_model(modelo03_model_final) %>%
  add_recipe(modelo03_recipe)

audio_ids_train <- read_rds("data/audio_ids_train.rds")
modelo03_df <- read_rds("data/modelos_mfcc/modelo03_df.rds")
modelo03_train <- read_rds("data/modelos_mfcc/modelo03_train.rds")
modelo03_val <- read_rds("data/modelos_mfcc/modelo03_val.rds")

# fit
modelo03 <- fit(modelo03_wflow_final, data =  modelo03_train)

# performace
modelo03_obs_vs_pred_val <- bind_cols(
  modelo03_df %>% anti_join(audio_ids_train, by = "audio_id") %>% select(audio_id, slice_id),
  predict(modelo03, modelo03_val, type = "prob"),
  predict(modelo03, modelo03_val, type = "class"),
  modelo03_val %>% select(y)
)
modelo03_obs_vs_pred_train <- bind_cols(
  modelo03_df %>% semi_join(audio_ids_train, by = "audio_id") %>% select(audio_id, slice_id),
  predict(modelo03, modelo03_train, type = "prob"),
  predict(modelo03, modelo03_train, type = "class"),
  modelo03_train %>% select(y)
)
modelo03_obs_vs_pred <- bind_rows(
  modelo03_obs_vs_pred_train %>% mutate(base = "train"),
  modelo03_obs_vs_pred_val %>% mutate(base = "val")
)

# confusion matrices
modelo03_obs_vs_pred %>%
  count(base, .pred_class, y) %>%
  spread(y, n, fill = 0)

# metrics
modelo03_obs_vs_pred %>%
  select(y, matches("pred_"), base) %>%
  group_by(base) %>%
  nest_legacy() %>%
  mutate(
    roc_auc = map_dbl(data, ~roc_auc(.x, y, matches("pred_[0-9]"))$.estimate),
    accuracy = map_dbl(data, ~accuracy(.x, y, .pred_class)$.estimate),
    kappa = map_dbl(data, ~kap(.x, y, .pred_class)$.estimate),
    recall = map_dbl(data, ~recall(.x, y, .pred_class)$.estimate),
    precision = map_dbl(data, ~precision(.x, y, .pred_class)$.estimate)
  )


# save
write_rds(modelo03, "data/modelos_mfcc/modelo03.rds")
write_rds(modelo03_wflow_final, "data/modelos_mfcc/modelo03_wflow_final.rds")
write_rds(modelo03_obs_vs_pred, "data/modelos_mfcc/modelo03_obs_vs_pred.rds")

toc()



