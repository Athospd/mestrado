library(tidyverse)

# carrega segmentos dos cantos dos passarinhos
especie <- "Glaucidium-minutissimum"
segmentacoes <- list.files(glue::glue("data/{especie}/"), full.names = TRUE) %>% purrr::map_dfr(~{
  arq <- readRDS(.x)
  arq$segmentacao
})

segmentacoes <- segmentacoes %>%
  drop_na() %>%
  as_tibble()

# monta base de fatias de 1 segundo
fatias_de_1s <- tibble(fatia_id = list.files("data-raw/wav_12khz_1s/", pattern = especie)) %>%
  mutate(
    sound.files = str_replace(fatia_id, "_[0-9]*s\\.wav$", ".wav"),
    instante_ini = str_extract(fatia_id, "_[0-9]*s") %>% str_extract("[0-9]+") %>% as.numeric + 0.03,
    instante_fim = instante_ini + 0.97
  )

fatias_de_1s_labels <- left_join(
  fatias_de_1s %>% group_by(sound.files) %>% nest(.key = "fatias_de_1s_data"),
  segmentacoes %>% group_by(sound.files) %>% nest(.key = "segmentacoes_data"),
  by = "sound.files"
) %>%
  mutate(
    interval_join = map2(fatias_de_1s_data, segmentacoes_data, ~{
      if(is.null(.y)) return(tibble(fatia_id = NA, instante_ini = NA, instante_fim = NA))
      fuzzyjoin::interval_semi_join(
        x = .x,
        y = .y,
        by = c("instante_ini" = "start", "instante_fim" =  "end")
      )
    })
  ) %>%
  select(
    interval_join
  ) %>%
  unnest() %>%
  drop_na() %>%
  right_join(
  fatias_de_1s,
  by = "fatia_id"
) %>%
  mutate(
    label = if_else(is.na(instante_ini.x), "Desconhecido", especie)
  ) %>%
  select(
    fatia_id,
    label
  )

saveRDS(fatias_de_1s_labels, glue::glue("data/labels_{especie}.rds"))
write.csv(fatias_de_1s_labels, glue::glue("data/labels_{especie}.csv"), row.names = FALSE)
fatias_de_1s_labels %>% count(label)
