library(tidyverse)
library(lubridate)

# Chargement des données

## Création d'un vecteur pour contenir notre liste de csv
liste_csv <- list.files(path = "data/tidy",
                        pattern = "*.csv",
                        full.names = TRUE)


## Construction d'une liste avec les data frame
raw_liste_df <- lapply(liste_csv, read_csv)

## Jointure des csv
raw_df <- do.call("rbind", raw_liste_df)

#Jointure des données avec la liste des condidats du MI

##Chargement de la liste des candidats du MI

raw_cand_mi <- read_tsv("data/raw/infos_elus/livre-des-listes-et-candidats.txt",
                        col_types = cols(.default = "c"),
                        skip = 1,
                        locale = locale(encoding = "ISO-8859-1"))


##Chargement nuancier
nuances_cand <- read_delim("data/raw/infos_elus/codes_nuances_2020.csv",
                           delim = ";") %>%
  select(CodNua, LibNua, Bloc)
