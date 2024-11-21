library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(data.table)
library(scales)

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

academie_sf <- st_read("Données brutes/fr-en-contour-academies-2020.geojson")

college_pos <- fread("fr-en-adresse-et-geolocalisation-etablissements-premier-et-second-degre.csv")
college_pos <- college_pos %>%
  mutate(numero_uai = as.factor(numero_uai)) %>%
  select(numero_uai, longitude, latitude)

dta <- data.table::merge.data.table(dta, college_pos, by.x = "numero_college", by.y = "numero_uai")

dta <- merge(dta, dnb2, by.x = "numero_college", by.y = "UAI")

names(dta)[c(92:107)] <- c("taux_moyen_2008", "taux_moyen_2009", "taux_moyen_2010", "taux_moyen_2011", "taux_moyen_2006",
                           "taux_moyen_2007", "taux_moyen_2012", "taux_moyen_2017", "taux_moyen_2015", "taux_moyen_2016",
                           "taux_moyen_2013", "taux_moyen_2014", "taux_moyen_2019", "taux_moyen_2020", "taux_moyen_2018",
                           "taux_moyen_2021")

for(k in names(dta)[c(92:107)]) {
  dta[[k]] <- as.numeric(gsub(",", ".", gsub("%", "", as.character(dta[[k]])))) / 100
}

# academie <- filter(academie, ! name %in% c("La Réunion","Martinique","Guadeloupe","Guyane","Mayotte"))

academie_sf <- st_crop(academie_sf,xmin = -8 , xmax = 10, ymin =41 , ymax = 52)
academie_sf <- rename(academie_sf, "libelle_academie" = "libelle_aca_majuscules")

#On prend les données par académie
dta_academie <- dta %>% 
  group_by(libelle_academie) %>% 
  summarise(taux_moyen = mean(taux_de_reussite)) %>% 
  select(libelle_academie, taux_moyen)

dta_academie_pu_pr <- dta %>% 
  group_by(libelle_academie,secteur_d_enseignement) %>% 
  summarise(nombre_inscrits = sum(inscrits)) %>% 
  select(libelle_academie, secteur_d_enseignement,nombre_inscrits)

dta_academie_pu_pr <- dta_academie_pu_pr %>% 
  pivot_wider(names_from = secteur_d_enseignement, values_from = nombre_inscrits)

academie_carte <- academie_sf %>% 
  left_join(dta_academie, "libelle_academie")

academie_carte <- academie_carte %>% 
  left_join(dta_academie_pu_pr, "libelle_academie")


academie_barres <- academie_carte %>%
  mutate(
    total_inscrits = PUBLIC + PRIVE,           
    ratio_public = PUBLIC / total_inscrits,   
    ratio_prive = PRIVE / total_inscrits
    )      

academie_graph <-  
  ggplot() +
  geom_sf(data=academie_carte,aes(fill=taux_moyen),color="black")+
  theme_minimal()+
  scale_fill_distiller(palette = "RdBu",
                       direction = -1,
                       name = "Taux de réussite",
                       values = scales::rescale(c(min(academie_carte$taux_moyen),87.7,max(academie_carte$taux_moyen)))) +
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white")) +
  labs(title = "Taux de réussite moyen au brevet par academies de France Métropolitaine")+
  geom_col(
  data = academie_barres,
  aes(
    x = libelle_academie,
    y = -0.5,                  
    fill = "public",
    height = ratio_public
  ),
  position = "stack",         
  width = 0.4)

academie_graph




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
carte_taux <- dta_paris_carte %>% ggplot() +
  geom_sf(aes(fill = taux_moyen), color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Taux de réussite") +  
  theme_void() +
  labs(title = "Taux de réussite moyen au brevet par arrondissement de Paris",
       caption = "Source: GeoJSON + data.gouv")



#Obtention des données pour le graph taux de réussite public/privé par arrondissement

test <- dta %>% filter(str_starts(code_departement, "0")) %>% 
  select(code_academie, libelle_academie, secteur_d_enseignement, taux_de_reussite) %>% 
  group_by(code_academie, libelle_academie, secteur_d_enseignement) %>% 
  summarise(taux_moyen = mean(taux_de_reussite), eff_eta = n())

test2 <- pivot_wider(test, names_from = secteur_d_enseignement, values_from = c(taux_moyen, eff_eta))
test2 <- test2 %>% mutate(proportion_pu_pr = eff_eta_PUBLIC/(eff_eta_PUBLIC + eff_eta_PRIVE))
test2 <- test2 %>% rename(PRIVE = taux_moyen_PRIVE, PUBLIC = taux_moyen_PUBLIC)
proportion <- test2[c(2,7)]
test2 <- test2[-c(5,6,7)]

dif <- test2[2:4]
dif <- dif %>% mutate(difference = PRIVE-PUBLIC)
dif <- dif[-c(2,3)]

test2 <- pivot_longer(test2, cols = c(PUBLIC, PRIVE), names_to = "secteur_d_enseignement", values_to = "taux_moyen")

test2 <- left_join(test2, proportion)
test2 <- left_join(test2, dif)
test2 <- test2 %>% mutate(proportion_pu_pr = proportion_pu_pr *100)

test2 <- test2 %>% mutate(libelle_academie = paste(libelle_academie, " (", round(proportion_pu_pr, 0), "%)", sep = ""))
test2$libelle_academie <- as.factor(test2$libelle_academie)

test2 <- test2 %>% arrange(difference)

test2$libelle_academie <- fct_reorder(test2$libelle_academie, test2$difference)

test2$libelle_academie <- droplevels(test2$libelle_academie)


test2 %>% ggplot() +
  geom_point(aes(x = libelle_academie, y = taux_moyen, color = secteur_d_enseignement), size = 5) +
  geom_line(aes(x = libelle_academie, y = taux_moyen, group = libelle_academie), color = "black", size = 0.5) +
  geom_hline(yintercept = 88, color = "purple", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Taux de réussite au brevet par académie en fonction du secteur d'enseignement",
       subtitle = "La ligne en pointillés violette correspond à la moyenne nationale",
       
       x = "Académie (pourcentage de collège public)",
       color = "Secteur d'Enseignement",
       y = "Taux moyen de réussite au brevet"
  ) +
  ylim(80, 100)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "grey60"),
        axis.text.y = element_text(color = "grey60"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 11, color = "purple4"), #grey40
        plot.margin = margin(t = 10, r = 10, b = 10, l = 50),
        legend.title = element_blank()
  ) 


#Proportion fille/garcon en 3ème et taux de reussite + coloration privé public
dta_paris_f_g <- dta %>% filter(code_departement == "075") %>% 


#probleme des collège non mixte

dta_paris_f_g %>% ggplot() +
  aes(y = pourcentage_FG, x = taux_de_reussite, color = secteur_d_enseignement)+

