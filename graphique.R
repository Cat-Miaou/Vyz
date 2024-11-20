library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
#Importation données
dta <- read.csv("donnees-dnb.csv", dec = ",", stringsAsFactors = TRUE)
dta <- dta[-1]

library(readr)
dnb <- read_delim("Données brutes/dnb.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
dnb <- dnb%>%
  select(UAI, Session,  `Taux de réussite`)

dnb2 <- dnb %>%
  pivot_wider(names_from = Session, values_from = `Taux de réussite`)


#Importation des données de carte
url <- "https://opendata.paris.fr/explore/dataset/arrondissements/download/?format=geojson"
paris_sf <- st_read(url)

college_pos <- fread("fr-en-adresse-et-geolocalisation-etablissements-premier-et-second-degre.csv")
college_pos <- college_pos %>%
  mutate(numero_uai = as.factor(numero_uai)) %>%
  select(numero_uai, longitude, latitude)

dta <- data.table::merge.data.table(dta, college_pos, by.x = "numero_college", by.y = "numero_uai")

dta <- merge(dta, dnb2, by.x = "numero_college", by.y = "UAI")

#Obtention données pour la carte taux de réussite par arrondissement
dta_paris <- dta %>% filter(code_departement == "075") %>% 
  mutate(taux_ab = nombre_d_admis_mention_ab/inscrits,
         taux_ss = admis_sans_mention/inscrits,
         taux_b  = admis_mention_bien/inscrits,
         taux_tb = admis_mention_tres_bien/inscrits) %>% 
  select(commune, taux_de_reussite, ips, secteur_d_enseignement,
         taux_ab, taux_ss, taux_b, taux_tb) %>%
  rename("c_ar" = "commune") %>% 
  group_by(c_ar) %>% 
  summarise(taux_moyen = mean(taux_de_reussite), ips_moyen = mean(ips),
            taux_moyen_ss = mean(taux_ss), taux_moyen_ab = mean(taux_ab),
            taux_moyen_b = mean(taux_b), taux_moyen_tb = mean(taux_tb))


dta_paris$c_ar <- substr(as.character(dta_paris$c_ar), 4, nchar(as.character(dta_paris$c_ar)))
dta_paris$c_ar <- as.numeric(dta_paris$c_ar)

dta_paris_carte <- paris_sf %>%
  left_join(dta_paris, by = "c_ar")

#Création de la carte
#carte_taux <- 
dta_paris_carte %>% ggplot() +
  geom_sf(aes(fill = taux_moyen), color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Taux de réussite") +  
  theme_void() +
  labs(title = "Taux de réussite moyen au brevet par arrondissement de Paris",
       caption = "Source: GeoJSON + data.gouv")

#Obtention des données pour le graph taux de réussite public/privé par arrondissement
dta_paris_pu_pr <- dta %>% filter(code_departement == "075") %>% 
  select(commune, secteur_d_enseignement, taux_de_reussite) %>% 
  group_by(commune,secteur_d_enseignement) %>% 
  summarise(taux_moyen = mean(taux_de_reussite))

dta_paris_pu_pr$commune <- substr(as.character(dta_paris_pu_pr$commune), 4, nchar(as.character(dta_paris_pu_pr$commune)))
dta_paris_pu_pr$commune <- as.numeric(dta_paris_pu_pr$commune)

dta_paris_pu_pr %>% ggplot() +
  aes(x = commune, y = taux_moyen, fill = secteur_d_enseignement) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Taux de réussite au brevet en fonction de l'arrondissement et du secteur d'enseignement")
#oh putain j'ai une idée
#on peut juste faire une échelle divergente non ?

#Proportion fille/garcon en 3ème et taux de reussite + coloration privé public
dta_paris_f_g <- dta %>% filter(code_departement == "075") %>% 
  mutate(pourcentage_FG = X3emes_garcons/(X3emes_garcons+X3eme_filles)) %>% 
  select(commune, pourcentage_FG, taux_de_reussite, secteur_d_enseignement)

#probleme des collège non mixte

dta_paris_f_g %>% ggplot() +
  aes(y = pourcentage_FG, x = taux_de_reussite, color = secteur_d_enseignement)+
  geom_point()+
  geom_smooth()

