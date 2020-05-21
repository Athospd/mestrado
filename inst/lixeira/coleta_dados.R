library(ggplot2)
library(tuneR)
library(viridis)
library(tidyverse)
library(mestrado)

# dados do Xeno-canto
base_xeno_canto_metadados <- tibble::tibble(
  # apelido = c("Corujinha-do-mato", "Coruja-listrada", "Murucututu-de-barriga-amarela", "Corujinha-sapo", "Caburé-miudinho"),
  especie = c("megascops choliba",
              "strix hylophila",
              "pulsatrix koeniswaldiana",
              "megascops atricapilla",
              "glaucidium minutissimum")
) %>% 
  mutate(
    mp3_originais = purrr::map(especie, ~warbleR::querxc(.x, download = TRUE, path = "data-raw/mp3_originais"))
  )

saveRDS(base_xeno_canto_metadados, "data/base_xeno_canto_metadados.rds")

# dados do Wikiaves
# tarefas:
# - pegar o apelido do pássaro por meio do nome da espécie.
# - pegar o codigo_wikiaves do pássaro por meio do nome da espécie.

# Interface para a API `getTaxonsJSON` do wikiaves
wa_get_taxons_json <- function(term) {
  jsonlite::fromJSON(URLencode(glue::glue("https://www.wikiaves.com.br/getTaxonsJSON.php?term={term}")))
}

# Interface para a API `getRegistrosJSON` do wikiaves
wa_get_registros_json <- function(id, pagina = 1, som_ou_foto = c("s", "f"), t = "s") {
  glue::glue("https://www.wikiaves.com.br/getRegistrosJSON.php?tm={som_ou_foto[1]}&t={t}&s={id}&o=mp&p={pagina}") %>%
    jsonlite::fromJSON()
}

# Descobre o total de registros de uma especie encontradas pela `wa_get_registros_json()`
wa_get_registros_json_total_de_registros <- function(id, som_ou_foto = c("s", "f")) {
  result <- wa_get_registros_json(
    id = id,
    pagina = 1,
    som_ou_foto = som_ou_foto[1]
  )

  as.numeric(result$registros$total)
}

wa_metadados_de_todos_os_registros <- function(id) {
  total_de_registros <- wa_get_registros_json_total_de_registros(id)
  paginas <- ceiling(total_de_registros / 21)
  pb <- progress::progress_bar$new(total = paginas, format = glue::glue("id {id} [:bar] pg :current de :total" ))

  registros <- list()
  for(pagina in 1:paginas) {
    pb$tick()
    Sys.sleep(0.1)
    registros[[pagina]] <- wa_get_registros_json(id, pagina)  %>%
      purrr::transpose() %>%
      purrr::map_df(simplify) %>%
      tidyr::unnest() %>%
      dplyr::mutate(itens = map(itens, as.data.frame, stringsAsFactors = FALSE)) %>%
      tidyr::unnest()
  }
  Sys.sleep(1)

  return(registros)
}

base_wikiaves_metadados <- tibble::tibble(
  especie = c("megascops choliba", "strix hylophila", "pulsatrix koeniswaldiana", "megascops atricapilla", "glaucidium minutissimum")
) %>%
  dplyr::mutate(
    taxonomia = map(especie, wa_get_taxons_json)
  ) %>%
  tidyr::unnest() %>%
  mutate(
    registros = map(id, wa_metadados_de_todos_os_registros)
  ) %>%
  tidyr::unnest() %>%
  tidyr::unnest()  %>%
  mutate(
    mp3_nome = glue::glue("{label}-{id1}.mp3") %>% stringr::str_replace(" ", "-"),
    mp3_link = link1 %>% str_replace("jpg$", "mp3") %>% str_replace("#_", "_")
  )

saveRDS(base_wikiaves_metadados, "data/base_wikiaves_metadados.rds")

# baixa mp3 do wikiaves
library(magrittr)
base_wikiaves_metadados %$% purrr::walk2(mp3_link, mp3_nome, ~{
  cat(glue::glue("baixando {.y}"))
  cat("\n\n")
  Sys.sleep(0.0001)
  if(!file.exists(glue::glue("data-raw/{.y}"))) {
    httr::GET(.x, httr::write_disk(glue::glue("data-raw/{.y}")))
  } else{
    cat("já existe\n\n")
  }
})
