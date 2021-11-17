library(tidyverse)
library(rvest)

#url de la page web

url <- "https://www.lannion.bzh/mairie/conseil-municipal"

#aspiration de la page

raw_html <- read_html(url)

#filtre des textes des liens a

liens_pages_html <- raw_html %>%
  html_nodes("a") %>%
  html_text()

#filtre des urls des liens

href_pages_html <- raw_html %>%
  html_nodes("a") %>%
  html_attr("href")

#binding des noms des liens et des url
names(href_pages_html) <- liens_pages_html

#boucle for
for(n in names(href_pages_html)) {
  
  ##n en bas de casse
  name_low_case <- str_to_lower(n)
  
  ##lien
  lien <- href_pages_html[n]
  
  ##condition if pour ne télécharger que les CR à partir de 2020
  if(str_detect(name_low_case, "conseil municipal") & str_starts(lien, "/images/pdf/conseil-municipal/")) {
    
    ## extraction de l'année
    annee_lien <- as.integer(str_sub(lien, 31, 34))
    
    ## vérification année supérieur ou égale à 2020
    if(annee_lien>=2020) {
    
      path_to_pdf <- paste0("https://www.lannion.bzh", lien)
      
      file_path <- paste0("data/raw/pdf/", n, ".pdf")
      
      download.file(path_to_pdf, file_path,
                    extra = options(timeout=100))
    
    }
    
  }
  
}


list.files("data/raw/pdf")
