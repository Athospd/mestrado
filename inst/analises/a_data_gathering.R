library(warbleR)
library(fs)

# list here all the species.
bird_species <- c("megascops choliba",
                  "strix hylophila",
                  "pulsatrix koeniswaldiana",
                  "megascops atricapilla",
                  "glaucidium minutissimum")

# Should the MP3 be downloaded to the hard disk?
download <- TRUE

# insert here which folder do you want to store the mp3 files.
dir <- "data-raw/mp3_originals"
if(!dir.exists(dir)) fs::dir_create(dir)

# set donwload = TRUE for download the files to your hard disk.
metadata_xc <- tibble::tibble(species = bird_species)
metadata_xc$mp3_xc = purrr::map(bird_species, ~querxc(.x, download = download, path = dir))

# install.packages("remotes")
remotes::install_github("Athospd/wikiaves")
library(wikiaves)

# set donwload = TRUE for download the files to your hard disk.
metadata_wa <- bird_species %>% 
  wa_metadata(download = download, path = dir) %>% 
  tidyr::nest(mp3_wa = c(-term))