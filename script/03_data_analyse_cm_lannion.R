library(tidyverse)
library(lubridate)

# Chargement des données

## Création d'un vecteur pour contenir notre liste de csv
liste_csv <- list.files(path = "data/tidy/cm_lannion",
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


# Chargement RNE

raw_rne_cm <- read_tsv("data/raw/infos_elus/cm.csv")


# Chargement CSP

csp_lvl1 <- read_csv("data/raw/insee_csp/pcs2003_liste_n1.csv")

index_csp <- read_csv("data/raw/insee_csp/pcs2003_c_n4_n1.csv") %>%
  select(N3, N1) %>%
  left_join(csp_lvl1, by = c("N1" = "Code")) %>%
  select(-N1) %>%
  rename(csp_code=N3, csp_n1=Libellé) %>%
  distinct()

# Jointure des données

## Filtre candidats ville de Lannion
raw_lannion_2020 <- raw_cand_mi %>%
  filter(`Libellé commune`== "Lannion")

## Tri de la liste candidats et jointure avec le data frame nuances_cand

tidy_lannion_2020 <- raw_lannion_2020 %>%
  select(prenom=12, nom=11, nuance=8, sexe=10) %>%
  mutate(nom_candidat = paste(prenom, nom, sep = " ")) %>%
  mutate(nom_candidat = str_to_lower(nom_candidat)) %>%
  select(nom_candidat, nuance, sexe) %>%
  left_join(nuances_cand, by = c("nuance" = "CodNua")) %>%
  mutate(nom_candidat = ifelse(nom_candidat=="louis noël",
                               "louison noël",
                               nom_candidat))
    

# Jointure entre la liste des candidats et notre fichier présentiel

tidy_df <- raw_df %>%
  filter(date>=dmy("25-05-2020")) %>%
  mutate(nom = case_when(nom == "marie christine barac'h" ~ "marie-christine barac'h",
                         nom == "gérard falezan" ~ "gérard falézan",
                         nom == "louison noel" ~ "louison noël",
                         TRUE ~ nom)) %>%
  mutate(nom = str_replace(nom, "- ", "-")) %>%
  left_join(tidy_lannion_2020, by = c("nom" = "nom_candidat")) %>%
  filter(!is.na(LibNua))


# Jointure avec le RNE

## Tri du RNE pour la ville de Lannion
tidy_rne_cm <- raw_rne_cm %>%
  filter(`Libellé de la commune`=="Lannion") %>%
  select(prenom=8, nom=7, annais=10, csp_code=11, csp_lib=12) %>%
  mutate_at(c("prenom", "nom"), str_to_lower) %>%
  mutate(nom_cm=paste(prenom, nom, sep=" ")) %>%
  select(nom_cm, annais, csp_code, csp_lib) %>%
  mutate(nom_cm = ifelse(nom_cm=="louis noël",
                               "louison noël",
                               nom_cm))

## Jointure RNE et tidy_df

tidy_df_rne <- tidy_df %>%
  left_join(tidy_rne_cm, by = c("nom" = "nom_cm")) %>%
  mutate(csp_lib = ifelse(nom == "gaël cornec",
                "Cadre de la fonction publique",
                csp_lib),
         csp_code = ifelse(nom == "gaël cornec",
                          33,
                          csp_code)) %>%
  mutate(annais=dmy(annais)) %>%
  mutate(annais = year(annais)) %>%
  mutate(annais = ifelse(nom == "gaël cornec",
                         1974,
                         annais)) %>%
  mutate(age=year(Sys.Date())-annais) %>%
  left_join(index_csp, by = "csp_code") %>%
  distinct()


# Écriture fichier

write_delim(tidy_df_rne, "data/tidy/tidy_df_cm_lannion.csv",
            delim = ";")

  
  






