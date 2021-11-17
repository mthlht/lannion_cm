library(tidyverse)
library(lubridate)
library(pdftools)

# Liste des pdf
liste_pdf <- list.files("data/raw/pdf",
                        full.names = TRUE)

liste_pdf <- liste_pdf[liste_pdf != "data/raw/pdf/Procès verbal du conseil municipal du 25 mai.pdf"]

for(un_pdf in liste_pdf) {
  
  #1e étape - lecture du fichier pdf
  
  raw_pdf <- enc2utf8(pdf_text(un_pdf))
  
  #2e étape - aller chercher les pages qui nous intéressent
  
  first_pages <- raw_pdf[1:2] %>%
    paste(collapse = "\n")
  
  #3e étape - vectoriser les pages
  
  lines_vector <- first_pages %>%
    str_split("\n") %>%
    unlist()
  
  #4e étape - nettoyage du vecteur
  
  tidy_lines <- lines_vector %>%
    str_trim() %>%
    str_to_lower()
  
  tidy_lines <- tidy_lines[tidy_lines != ""]
  
  #5e étape - prendre les index de chaque groupe
  
  raw_index_present <- which(str_detect(tidy_lines, "présents"))
  raw_index_procuration <- which(str_detect(tidy_lines, "procuration"))
  raw_index_absent <- which(str_detect(tidy_lines, "absent"))
  raw_index_fin <- which(str_detect(tidy_lines, "question"))
  
  #6e étape - affinement variables index
  
  ## - tri index présent
  if(!is_empty(raw_index_present)) {
    
    index_present <- min(raw_index_present)
    
  }
  
  ## tri index fin
  if(!is_empty(raw_index_fin)) {
    
    index_fin <- min(raw_index_fin)
    
  }
  
  ## tri index absent
  if(!is_empty(raw_index_absent) & min(raw_index_absent)<index_fin) {
    
    index_absent <- min(raw_index_absent)
    
  } else { index_absent <- FALSE }
  
  ## tri index procuration
  if(!is_empty(raw_index_procuration) & min(raw_index_procuration)<index_fin) {
    
    index_procuration <- min(raw_index_procuration)
    
  } else { index_procuration <- FALSE }
  
  # 7e étape - définition des séquences d'index
  
  if(index_present & index_absent & index_procuration) {
    
    ## scénario - présent / procuration / absent
    scenario <- "p-a-p"
    
    ## séquences
    ### présent
    range_present <- seq(from = index_present+1,
                         to = index_absent-1,
                         by = 1)
    ### absent
    if(index_procuration-index_absent>1) {
    range_absent <- seq(from = index_absent+1,
                         to = index_procuration-1,
                         by = 1)
    } else { range_absent = index_absent}
    
    ### procuration
    if(index_fin-index_procuration>1) {
    range_procuration <- seq(from = index_procuration+1,
                        to = index_fin-1,
                        by = 1)
    } else { range_procuration = index_procuration }
    
  } else if(index_present & index_absent & !index_procuration) {
    
    ## scénario - présent / absent
    scenario <- "p-a"
    
    ## séquences
    ### présent
    range_present <- seq(from = index_present+1,
                         to = index_absent-1,
                         by = 1)
    ### absent
    if(index_fin-index_absent>1) {
    range_absent <- seq(from = index_absent+1,
                        to = index_fin-1,
                        by = 1)
    } else { range_absent <- index_absent }
    
  } else if(index_present & !index_absent & index_procuration) {
    
    ## scénario - présent / prcuration
    scenario <- "p-p"
    
    ## séquences
    ### présent
    range_present <- seq(from = index_present+1,
                         to = index_procuration-1,
                         by = 1)
    ### absent
    if(index_fin-index_procuration>1) {
    range_procuration <- seq(from = index_procuration+1,
                        to = index_fin-1,
                        by = 1)
    } else { range_procuration = index_procuration }
    
  } else {
    
    ## scénario - présent / prcuration
    scenario <- "p"
    
    ## séquences
    ### présent
    range_present <- seq(from = index_present+1,
                         to = index_fin-1,
                         by = 1)
    
  }
  
  #8e étape - les groupes de conseillers municipaux
  
  if(scenario == "p-a-p") {
    
    raw_groupe_present <- tidy_lines[range_present] %>%
      paste(collapse = " ") %>%
      str_split(" - ") %>%
      unlist()
    
    raw_groupe_absent <- tidy_lines[range_absent] %>%
      paste(collapse = " ") %>%
      str_split(" - ") %>%
      unlist()
    
    raw_groupe_procuration <- tidy_lines[range_procuration] %>%
      paste(collapse = " ") %>%
      str_split(" - ") %>%
      unlist()
    
  } else if(scenario == "p-a") {
    
    raw_groupe_present <- tidy_lines[range_present] %>%
      paste(collapse = " ") %>%
      str_split(" - ") %>%
      unlist()
    
    raw_groupe_absent <- tidy_lines[range_absent] %>%
      paste(collapse = " ") %>%
      str_split(" - ") %>%
      unlist()
    
  } else if(scenario == "p-p") {
    
    raw_groupe_present <- tidy_lines[range_present] %>%
      paste(collapse = " ") %>%
      str_split(" - ") %>%
      unlist()
    
    raw_groupe_procuration <- tidy_lines[range_procuration] %>%
      paste(collapse = " ") %>%
      str_split(" - ") %>%
      unlist()
    
  } else if(scenario == "p") {
    
    raw_groupe_present <- tidy_lines[range_present] %>%
      paste(collapse = " ") %>%
      str_split(" - ") %>%
      unlist()
    
  }
  
  # 9e étape - transformation en data frame
  
  ## data frame des présents
  df_present <- data.frame(nom = raw_groupe_present) %>%
    mutate(nom = as.character(nom)) %>%
    mutate(etat = "présent")
  
  ## data frame des procurations
  if(scenario %in% c("p-a-p", "p-p")) {
    
    df_procuration <- data.frame(nom = raw_groupe_procuration) %>%
      mutate(nom = as.character(nom)) %>%
      mutate(etat = "procuration")
    
  }
  
  ## data frame des absents
  if(scenario %in% c("p-a-p", "p-a")) {
    
    df_absent <- data.frame(nom = raw_groupe_absent) %>%
      mutate(nom = as.character(nom)) %>%
      mutate(etat = "absent")
    
  }
  
  #10e étape - jointure data frames
  
  if(scenario == "p-p") {
    
    raw_df <- rbind(df_present, df_procuration)
    
  }
  
  if(scenario == "p-a-p") {
    
    raw_df <- rbind(df_present, df_procuration, df_absent)
    
  }
  
  if(scenario == "p-a") {
    
    raw_df <- rbind(df_present, df_absent)
    
  }
  
  if(scenario == "p") {
    
    raw_df <- df_present
    
  }
  
  #11e étape - ajoute de la date
  
  ## sélection de l'index de la ligne qui contient la date
  index_date <- min(which(str_starts(tidy_lines,
                                 "lundi|mardi|mercredi|jeudi|vendredi|samedi|dimanche")))
  
  ## sélection de la ligne qui contient la date
  raw_date_cm <- tidy_lines[index_date]
  
  ## transformation de la chaine de caractères en date
  tidy_date <- dmy(raw_date_cm)
  
  #12e étape - ajout date et tri des procurations
  
  tidy_df <- raw_df %>%
    mutate(date = tidy_date) %>%
    separate(col = nom, into = c("nom" ,"procuration"),
             sep = "\\(procuration à",
             fill = "right") %>%
    mutate(procuration = str_remove(procuration, "[:punct:]")) %>%
    mutate_at(c("nom", "procuration"), str_trim)
  
  
  #13e étape - écriture du fichier
  
  file_path <- paste0("data/tidy/cm_lannion-", tidy_date, ".csv")
  
  write_csv(tidy_df, file_path)
 
}

