set.seed(1)

tic("modelo01 tune")
# MFCCs
mfccs <- read_rds("data/mfcc_500ms.rds")

# dados
x_dim <- dim(mfccs[[1]])
y_dim <- dim(mfccs[[2]])
modelo01_df <- mfccs[[1]] %>% 
  rray::rray_reshape(dim = c(x_dim[1], prod(x_dim[-1]))) %>% 
  as.data.frame() %>%
  mutate(
    audio_id = str_remove_all(mfccs[[3]], "(@.*@)|(^.*/)"),
    y = as.factor(mfccs[[2]] %*% seq(y_dim[2]))
  )
write_rds(modelo01_df, "data/modelos_mfcc/modelo01_df.rds")

# Treino/Teste
audio_ids_train <- read_rds("data/audio_ids_train.rds")

modelo01_train <- modelo01_df %>% semi_join(audio_ids_train, by = "audio_id") %>% select(-audio_id)
modelo01_val <- modelo01_df %>% anti_join(audio_ids_train, by = "audio_id") %>% select(-audio_id)
rm(modelo01_df);gc()
write_rds(modelo01_train, "data/modelos_mfcc/modelo01_train.rds")
write_rds(modelo01_val, "data/modelos_mfcc/modelo01_val.rds")

# Receita
modelo01_recipe <- modelo01_train %>%
  recipe(y ~ .) %>%
  prep()
write_rds(modelo01_recipe, "data/modelos_mfcc/modelo01_recipe.rds")

# Modelo 
modelo01_model <- rand_forest(trees = tune(), mtry = tune(), mode = "classification") %>%
  set_engine("ranger", max.depth = 20, min.node.size = 5)

# Workflow
modelo01_wflow <- workflow() %>%
  add_model(modelo01_model) %>%
  add_recipe(modelo01_recipe)

# Parâmetros
# modelo01_params <- parameters(modelo01_wflow, mtry = finalize(mtry(), modelo01_train %>% select(-y)))
modelo01_params <- expand_grid(
  mtry = c(5, 10, 20, 40, 60, 120, 200),
  trees = c(500)
) %>%
  sample_n(7)

# Amostras CV
modelo01_train_cv <- vfold_cv(modelo01_train, v = 5)

# Grid Search
set.seed(1)
if(Sys.info()["sysname"] == "Windows") {
  doParallel::registerDoParallel(4)
} else {
  doMC::registerDoMC(4)
}

modelo01_search_res <- tune_grid(
  modelo01_wflow, 
  resamples = modelo01_train_cv,
  grid = modelo01_params,
  metrics = metric_set(roc_auc, accuracy, recall, precision, kap),
  control = control_grid(verbose = TRUE, allow_par = TRUE)
)

# Best Model
tune::select_best(modelo01_search_res, metric = "accuracy")

modelo01_search_res_table <- modelo01_search_res %>%
  select(.metrics) %>%
  unnest_legacy() %>%
  select(-.estimator) %>%
  group_by_at(vars(-.estimate)) %>%
  summarise(
    estimate = mean(.estimate),
    se = sd(.estimate)/sqrt(n())
  )

modelo01_search_res_table
modelo01_search_res_table %>%
  ggplot(aes(x = mtry, y = estimate, colour = .metric)) +
  geom_errorbar(aes(ymin = estimate - 2*se, ymax = estimate + 2*se)) +
  facet_wrap(~.metric)

modelo01_best_params <- tune::select_best(modelo01_search_res, "kap")
write_rds(modelo01_search_res, "data/modelos_mfcc/modelo01_search_res.rds")
write_rds(modelo01_best_params, "data/modelos_mfcc/modelo01_best_params.rds")
toc()




# Final Model --------------------------------------------------------------------------------
tic("modelo01 final fit")
# data
modelo01_best_params <- read_rds("data/modelos_mfcc/modelo01_best_params.rds")
modelo01_model_final <- do.call(rand_forest, as.list(modelo01_best_params) %>% append(list(mode = "classification"))) %>%
  set_engine("ranger", max.depth = 20, min.node.size = 5)

modelo01_recipe <- read_rds("data/modelos_mfcc/modelo01_recipe.rds")
modelo01_wflow_final <- workflow() %>%
  add_model(modelo01_model_final) %>%
  add_recipe(modelo01_recipe)

audio_ids_train <- read_rds("data/audio_ids_train.rds")
modelo01_df <- read_rds("data/modelos_mfcc/modelo01_df.rds")
modelo01_train <- read_rds("data/modelos_mfcc/modelo01_train.rds")
modelo01_val <- read_rds("data/modelos_mfcc/modelo01_val.rds")

# fit
modelo01 <- fit(modelo01_wflow_final, data =  modelo01_train)

# performace
modelo01_obs_vs_pred_val <- bind_cols(
  modelo01_df %>% anti_join(audio_ids_train, by = "audio_id") %>% select(audio_id),
  predict(modelo01, modelo01_val, type = "prob"),
  predict(modelo01, modelo01_val, type = "class"),
  modelo01_val %>% select(y)
)
modelo01_obs_vs_pred_train <- bind_cols(
  modelo01_df %>% semi_join(audio_ids_train, by = "audio_id") %>% select(audio_id),
  predict(modelo01, modelo01_train, type = "prob"),
  predict(modelo01, modelo01_train, type = "class"),
  modelo01_train %>% select(y)
)
modelo01_obs_vs_pred <- bind_rows(
  modelo01_obs_vs_pred_train %>% mutate(base = "train"),
  modelo01_obs_vs_pred_val %>% mutate(base = "val")
)

# confusion matrices
modelo01_obs_vs_pred %>%
  count(base, .pred_class, y) %>%
  spread(y, n, fill = 0)

# metrics
modelo01_obs_vs_pred %>%
  select(y, matches("pred_[0-9]"), base) %>%
  group_by(base) %>%
  nest_legacy() %>%
  mutate(
    auc = map_dbl(data, ~roc_auc(.x, y, matches("pred_[0-9]"))$.estimate)
  )
  

# save
write_rds(modelo01, "data/modelos_mfcc/modelo01.rds")
write_rds(modelo01_wflow_final, "data/modelos_mfcc/modelo01_wflow_final.rds")
write_rds(modelo01_obs_vs_pred, "data/modelos_mfcc/modelo01_obs_vs_pred.rds")

toc()



