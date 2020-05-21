set.seed(1)

tic("modelo02 tune")
# MFCCs
mfccs <- read_rds("data/mfcc_500ms.rds")

# dados
x_dim <- dim(mfccs[[1]])
y_dim <- dim(mfccs[[2]])
modelo02_df <- mfccs[[1]] %>% 
  rray::rray_reshape(dim = c(x_dim[1], prod(x_dim[-1]))) %>% 
  as.data.frame() %>%
  mutate(
    audio_id = str_remove_all(mfccs[[3]], "(@.*@)|(^.*/)"),
    y = as.factor(mfccs[[2]] %*% seq(y_dim[2]))
  )
write_rds(modelo02_df, "data/modelos_mfcc/modelo02_df.rds")

# Treino/Teste
audio_ids_train <- read_rds("data/audio_ids_train.rds")

modelo02_train <- modelo02_df %>% semi_join(audio_ids_train, by = "audio_id") %>% select(-audio_id)
modelo02_val <- modelo02_df %>% anti_join(audio_ids_train, by = "audio_id") %>% select(-audio_id)
rm(modelo02_df);gc()
write_rds(modelo02_train, "data/modelos_mfcc/modelo02_train.rds")
write_rds(modelo02_val, "data/modelos_mfcc/modelo02_val.rds")

# Receita
modelo02_recipe <- modelo02_train %>%
  recipe(y ~ .) %>%
  prep()
write_rds(modelo02_recipe, "data/modelos_mfcc/modelo02_recipe.rds")

# Modelo
modelo02_model <- boost_tree(
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
modelo02_wflow <- workflow() %>%
  add_model(modelo02_model) %>%
  add_recipe(modelo02_recipe)

# Parâmetros
# modelo02_params <- parameters(modelo02_wflow, mtry = finalize(mtry(), modelo02_train %>% select(-y)))
set.seed(1)
modelo02_params <- expand_grid(
  mtry = c(5, 10, 20, 40, 60, 120, 200),
  trees = c(1000),
  min_n = c(10),
  tree_depth = c(2, 5, 10, 15, 20),
  learn_rate = c(0.001, 0.00001),
  loss_reduction = c(1, 0.1),
  sample_size = c(0.8)
) %>%
  sample_n(15)

# Amostras CV
modelo02_train_cv <- vfold_cv(modelo02_train, v = 5)

# Grid Search
set.seed(1)
doMC::registerDoMC(6)

modelo02_search_res <- tune_grid(
  modelo02_wflow, 
  resamples = modelo02_train_cv,
  grid = modelo02_params,
  metrics = metric_set(roc_auc, accuracy, recall, precision, kap),
  control = control_grid(verbose = TRUE, allow_par = TRUE)
)

# Best Model
tune::select_best(modelo02_search_res, metric = "accuracy")

modelo02_search_res_table <- modelo02_search_res %>%
  select(.metrics) %>%
  unnest_legacy() %>%
  select(-.estimator) %>%
  group_by_at(vars(-.estimate)) %>%
  summarise(
    estimate = mean(.estimate),
    se = sd(.estimate)/sqrt(n())
  )

  
modelo02_search_res_table %>%
  ggplot(aes(x = mtry, y = estimate, colour = .metric)) +
  geom_errorbar(aes(ymin = estimate - 2*se, ymax = estimate + 2*se)) +
  facet_wrap(~.metric)

modelo02_best_params <- tune::select_best(modelo02_search_res, "kap")
write_rds(modelo02_search_res, "data/modelos_mfcc/modelo02_search_res.rds")
write_rds(modelo02_best_params, "data/modelos_mfcc/modelo02_best_params.rds")

toc()




# Final Model --------------------------------------------------------------------------------
tic("modelo02 final fit")
# data
modelo02_best_params <- read_rds("data/modelos_mfcc/modelo02_best_params.rds")
modelo02_model_final <- do.call(boost_tree, as.list(modelo02_best_params)) %>%
  set_engine("xgboost", verbose = 1) %>%
  set_mode("classification")

modelo02_recipe <- read_rds("data/modelos_mfcc/modelo02_recipe.rds")
modelo02_wflow_final <- workflow() %>%
  add_model(modelo02_model_final) %>%
  add_recipe(modelo02_recipe)

audio_ids_train <- read_rds("data/audio_ids_train.rds")
modelo02_df <- read_rds("data/modelos_mfcc/modelo02_df.rds")
modelo02_train <- read_rds("data/modelos_mfcc/modelo02_train.rds")
modelo02_val <- read_rds("data/modelos_mfcc/modelo02_val.rds")

# fit
modelo02 <- fit(modelo02_wflow_final, data =  modelo02_train)

# performace
modelo02_obs_vs_pred_val <- bind_cols(
  modelo02_df %>% anti_join(audio_ids_train, by = "audio_id") %>% select(audio_id),
  predict(modelo02, modelo02_val, type = "prob"),
  predict(modelo02, modelo02_val, type = "class"),
  modelo02_val %>% select(y)
)
modelo02_obs_vs_pred_train <- bind_cols(
  modelo02_df %>% semi_join(audio_ids_train, by = "audio_id") %>% select(audio_id),
  predict(modelo02, modelo02_train, type = "prob"),
  predict(modelo02, modelo02_train, type = "class"),
  modelo02_train %>% select(y)
)
modelo02_obs_vs_pred <- bind_rows(
  modelo02_obs_vs_pred_train %>% mutate(base = "train"),
  modelo02_obs_vs_pred_val %>% mutate(base = "val")
)

# confusion matrices
modelo02_obs_vs_pred %>%
  count(base, .pred_class, y) %>%
  spread(y, n, fill = 0)

# metrics
modelo02_obs_vs_pred %>%
  select(y, matches("pred_[0-9]"), base) %>%
  group_by(base) %>%
  nest_legacy() %>%
  mutate(
    auc = map_dbl(data, ~roc_auc(.x, y, matches("pred_[0-9]"))$.estimate)
  )


# save
write_rds(modelo02, "data/modelos_mfcc/modelo02.rds")
write_rds(modelo02_wflow_final, "data/modelos_mfcc/modelo02_wflow_final.rds")
write_rds(modelo02_obs_vs_pred, "data/modelos_mfcc/modelo02_obs_vs_pred.rds")

toc()



