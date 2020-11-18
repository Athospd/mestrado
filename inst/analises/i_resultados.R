# This code should run with TensorFlow >= 1.13.
# It was designed to run in graph as well as eager modes.
# If running in eager mode only, some pieces of code can be substantially simplified.
library(keras)
library(magrittr)
library(tidyverse)
library(tensorflow)
library(tfdatasets)
library(stringr)
library(dplyr)

model <- keras::load_model_tf("data/modelos_arquitetura1/arq1_0037")
model_utils <- readr::read_rds("data/modelos_arquitetura1/arq1_0037.rds")

ds_val <- model_utils$data_generator(model_utils$df_val,
                         samples_per_window = model_utils$samples_per_window,
                         stride_samples = model_utils$stride_samples)
predictions <- predict_generator(
  model,
  ds_val,
  steps = 32
)
str(predictions)