library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(data.table)
library(scales)
library(patchwork)
library(ggrepel)
library(grid)

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

best_school_forever <- dta %>%
  mutate(moyenne_15ans = rowMeans(across(92:107), na.rm = TRUE)) %>%
  arrange(desc(moyenne_15ans))

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

# On garde les écoles qui ont plus de 25 élèves inscrits au brevet
best_school <- best_school_forever %>% 
  filter(inscrits > 25)

#On retient les écoles qui ont toujours communiqué leurs résultats
school_complete <- which(complete.cases(best_school[,c(1,92:107)]))
best_school <- best_school[school_complete,]

best_school_2 <- best_school[,c(1,4,5,7,108)]
top20_school <- as.data.frame(best_school_2[1:20,1])
colnames(top20_school) <- "numero_college"

ecole_carte <- top20_school %>% 
  left_join(best_school_forever, by ="numero_college")

ecole_paris <- ecole_carte %>% 
  filter(str_starts(libelle_commune,"PARIS"))

ecole_paris[7,] <- ecole_paris[6,]
ecole_paris[7,]$secteur_d_enseignement <- "PUBLIC"

levels(ecole_paris$secteur_d_enseignement) <- c("Privé","Public")

ecole_academie <- ecole_carte %>% 
  filter(! str_starts(libelle_commune,"PARIS"))


ecole_academie_labels <- data.frame(
  cluster = c(1,2,3,4,5),
  latitude = c(50.657, 46, 44.2,48.5, 42.697),
  longitude = c(2.88, 4, 5.6, 0, 2.88),
  patronyme = c("COLLEGE PRIVE DE MARCQ \n SAINT PAUL",
                "INSTITUTION DES CHARTREUX",
                "NOTRE-DAME DES MISSIONS \n NOTRE DAME DE LA JEUNESSE \n CHEVREUL BLANCARDE",
                "MADELEINE DANIELOU \n ST JEAN HULST \n FRANCO ALLEMAND \n SAINT FRANCOIS D'ASSISE \n MERKAZ HATORAH FILLES \n EPIN \n NOTRE DAME DE LA PROVIDENCE",
                "JEANNE D'ARC"))


#Graphique taux de réussite moyen par académie
academie_graph <- ggplot() +
  geom_sf(data=academie_carte,aes(fill=taux_moyen),color="black")+
  theme_minimal()+
  scale_fill_distiller(palette = "RdBu",
                       direction = -1,
                       name = "Taux de réussite",
                       values = scales::rescale(c(min(academie_carte$taux_moyen,dta_paris_carte$taux_moyen),87.7,max(academie_carte$taux_moyen,dta_paris_carte$taux_moyen))))+
  theme(panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  geom_point(data = ecole_academie, aes(x = longitude, y  = latitude, group = patronyme ,color=secteur_d_enseignement), size = 3)+
  scale_color_manual(values = c("PRIVE" = "black", "PUBLIC" = "red"))

#Graphique taux de réussite au sein de l'académie de Paris
paris_graph <- ggplot(dta_paris_carte) +
  geom_sf(aes(fill = taux_moyen), color = "black") +
  scale_fill_distiller(palette = "RdBu",
                       direction = -1,
                       name = "Taux de réussite",
                       values = scales::rescale(c(min(academie_carte$taux_moyen),87.7,max(academie_carte$taux_moyen)))) +
  theme_minimal() +
  theme(legend.position = "right",
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text.x =  element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(face = "bold")) +
  geom_point(data = ecole_paris, aes(x = longitude, y  = latitude, group = patronyme ,color=secteur_d_enseignement, alpha=secteur_d_enseignement), size = 3)+
  scale_color_manual(values = c("Privé" = "black", "Public" = "red"), name = "Secteur d'enseignement") +
  scale_alpha_manual(values = c("Privé" = 1, "Public" = 0), guide = "none") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         fill = guide_colorbar(                                        
           barheight = 9.5,                                     
           frame.colour = "black",                               
           frame.linewidth = 0.5                                   
         ))

academie_paris <- academie_graph +
  paris_graph +
  plot_layout(ncol = 2, widths = c(6, 3))+
  plot_annotation(
    title = "Taux de réussite en France métropolitaine par académie et dans Paris - 2021",
    subtitle = "Les 20 meilleurs collèges sur les 15 dernières années sont représentés",
    caption = "Source: GeoJSON + data.gouv",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", margin = margin(t = 0, b = 0)),  # Titre principal
      plot.subtitle = element_text(size = 12, hjust = 0, vjust= 0),             # Sous-titre
      plot.caption = element_text(size = 10, hjust = 1),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  )

grid.newpage()
print(academie_paris)

# Positions relatives pour flèches (manuelles)
grid.segments(
  x0 = unit(0.312, "npc"), y0 = unit(0.707, "npc"),   # Point de départ (dans academie_graph)
  x1 = unit(0.598, "npc"), y1 = unit(0.639, "npc"),   # Point d'arrivée (dans paris_graph)
  gp = gpar(col = "black", lwd = 1, lty = "solid"))

grid.segments(
  x0 = unit(0.312, "npc"), y0 = unit(0.707, "npc"),   # Point de départ (dans academie_graph)
  x1 = unit(0.598, "npc"), y1 = unit(0.357, "npc"),   # Point d'arrivée (dans paris_graph)
  gp = gpar(col = "black", lwd = 1, lty = "solid"))

grid.segments(
  x0 = unit(0.312, "npc"), y0 = unit(0.707, "npc"),   # Point de départ (dans academie_graph)
  x1 = unit(0.878, "npc"), y1 = unit(0.639, "npc"),   # Point d'arrivée (dans paris_graph)
  gp = gpar(col = "black", lwd = 1, lty = "solid"))











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
