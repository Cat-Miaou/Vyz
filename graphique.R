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
  select(UAI, Session,  `Taux de réussite`, Admis, `Admis sans mention`, `Admis Mention bien`, `Admis Mention très bien`, Nombre_d_admis_Mention_AB)

dnb2 <- dnb %>%
  pivot_wider(names_from = Session, values_from = c(`Taux de réussite`, Admis, `Admis sans mention`, `Admis Mention bien`, `Admis Mention très bien`, Nombre_d_admis_Mention_AB))


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

names(dta)[c(92:107)] <- c("taux_2008", "taux_2009", "taux_2010", "taux_2011", "taux_2006",
                           "taux_2007", "taux_2012", "taux_2017", "taux_2015", "taux_2016",
                           "taux_2013", "taux_2014", "taux_2019", "taux_2020", "taux_2018",
                           "taux_2021")

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
    fill = "public"
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

carte_taux

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
test2$secteur_d_enseignement <- as.factor(test2$secteur_d_enseignement)
levels(test2$secteur_d_enseignement) <- c("Privé", "Public")

test2 %>% ggplot() +
  geom_line(
    aes(
      x = libelle_academie,
      y = taux_moyen,
      group = libelle_academie),
    color = "black",
    size = 0.8) +
  geom_point(
    aes(
      x = libelle_academie,
      y = taux_moyen,
      color = secteur_d_enseignement),
    size = 6) +
  
  geom_hline(
    aes(yintercept = 88,
        color = "Moyenne\nnationale"), # Associer une esthétique à la ligne
    linetype = "dashed",
    size = 1
  )+
  theme_minimal() +
  labs(
    title = "Taux de réussite au brevet par académie\nen fonction du secteur d'enseignement en 2021",
    x = "Académie (pourcentage de collège public)",
    color = "Secteur d'Enseignement",
    y = "Taux moyen de réussite au brevet",
    caption = "Source : Data.gouv.fr"
  ) +
  scale_color_manual(values = c("Privé" = "darkgoldenrod1",
                                "Public" = "dodgerblue2",
                                "Moyenne\nnationale" = "mediumorchid2")) +
  ylim(80, 100)+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   size = 14,
                                   color = "grey45"),
        axis.text.y = element_text(color = "grey60",
                                   size = 14),
        axis.title.x = element_text(size = 17,
                                    color = "gray20"),
        axis.title.y = element_text(size = 17,
                                    color = "gray20"),
        plot.title = element_text(hjust = 0.5,
                                  size = 25),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 20,
                                     color = "gray30"), #grey40
        plot.margin = margin(t = 10,
                             r = 10,
                             b = 10,
                             l = 55),
        legend.key.spacing.y = unit(0.25,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15)
  ) 



# Supposons que votre dataframe s'appelle `df`

#calcul le poucentage de toutes les mentions pour toytes les années
for (year in 2006:2021) {
  # Construire les noms des colonnes existantes
  col_admis <- paste0("Admis_", year)
  col_bien <- paste0("Admis Mention bien_", year)
  col_tres_bien <- paste0("Admis Mention très bien_", year)
  col_assez_bien <- paste0("Nombre_d_admis_Mention_AB_", year)
  
  # Noms des nouvelles colonnes pour les proportions
  col_prop_bien <- paste0("Proportion_bien_", year)
  col_prop_tres_bien <- paste0("Proportion_très_bien_", year)
  col_prop_assez_bien <- paste0("Proportion_assez_bien_", year)
  # Calcul des proportions
  dta[[col_prop_bien]] <- dta[[col_bien]] / dta[[col_admis]]
  dta[[col_prop_tres_bien]] <- dta[[col_tres_bien]] / dta[[col_admis]]
  dta[[col_prop_assez_bien]] <- dta[[col_assez_bien]] / dta[[col_admis]]
}



# Calcul de la moyenne nationale pour chaque année
moyennes_nationales <- dta %>%
  summarise(
    across(
      starts_with("Proportion_"),
      mean,
      na.rm = TRUE, # Ignorer les valeurs manquantes
      .names = "Moyenne_{.col}"
    )
  )

# Ajouter une colonne pour les années


#pivot le dta pour avoir une colonne avec l'année + mention (ab, b ou tb) et une autre avec le pourcentage de personne ayant la mention
moyennes_long <- moyennes_nationales %>%
  pivot_longer(
    cols = starts_with("Moyenne_"), # Colonnes des moyennes
    names_to = "Mention",
    values_to = "Proportion"
  )
#garde que les colonnes indiquant le taux de réussité du dnb des établissements
dta_admis <- dta[, c(92:107)]
#dta avec le pourcentage de réussite nationale du brevet
dta_admis_mean <- dta_admis %>%
  summarise(across(everything(), ~mean(. , na.rm = TRUE)))

#pivot le dta pour avoir 2 colonnes, une avec la mention taux_annee et l'autre avec la porportion de réussite du dnb en fonction de l'année
dta_admis_mean2 <- dta_admis_mean %>%
  pivot_longer(
    cols = starts_with("taux_"), # Colonnes des moyennes
    names_to = "year",
    values_to = "Proportion"
  )

#enleve taux_ dans la colonne year pour ne garder que les années + conversion en numérqiue
dta_admis_mean2$year <- as.numeric(gsub("taux_", "", dta_admis_mean2$year))


# enleve la mention Moyenne_proportion dans la colonne Mention car ne sert à rien
moyennes_long$Mention <- gsub("Moyenne_Proportion_", "", moyennes_long$Mention)
# Nettoyer la colonne Mention en enlevant l'année pour que toutes les valeurs soient les mêmes
moyennes_long$Mention <- gsub("_\\d{4}$", "", moyennes_long$Mention)

#ajoute l'année pour les mentions, il y a 3 mentions par années, donc l'année est répétée 3 fois
moyennes_long$year <- rep(2006:2021, each = 3)[1:nrow(moyennes_long)]

# Ajouter une colonne factice dans dta_admis_mean2
dta_admis_mean2 <- dta_admis_mean2 %>%
  mutate(Mention = "Moyenne générale")  # Une mention unique pour ce dataset

ggplot() +
  # Lignes et points pour moyennes_long
  geom_point(data = moyennes_long, size = 3, aes(x = year, y = Proportion, color = Mention, shape = Mention)) +
  geom_line(data = moyennes_long, aes(x = year, y = Proportion, color = Mention)) +
  # Lignes et points pour dta_admis_mean2
  geom_point(data = dta_admis_mean2, size = 3, aes(x = year, y = Proportion, color = Mention)) +
  geom_line(data = dta_admis_mean2, aes(x = year, y = Proportion, color = Mention)) +
  # Lignes verticales de référence
  geom_vline(aes(xintercept = 2011, color = "2011"), linetype = "dashed") +
  geom_vline(aes(xintercept = 2017, color = "2017"), linetype = "dashed") +
  geom_vline(aes(xintercept = 2018, color = "2018"), linetype = "dashed") +
  geom_vline(aes(xintercept = 2013, color = "2013"), linetype = "dashed") +
  geom_vline(aes(xintercept = 2020, color = "2020"), linetype = "dashed") +
  # Mise en place des flèches pour 2011, 2017, et 2018
  annotate("segment", x = 2008.5, xend = 2010.95, y = 0.6, yend = 0.7, 
           arrow = arrow(length = unit(0.2, "cm")), color = "#666666") +
  annotate("segment", x = 2015, xend = 2016.95, y = 0.6, yend = 0.7, 
           arrow = arrow(length = unit(0.2, "cm")), color = "#666666") +
  annotate("segment", x = 2014.5, xend = 2013.05, y = 0.45, yend = 0.58, 
           arrow = arrow(length = unit(0.2, "cm")), color = "#666666") +
  annotate("segment", x = 2018.5, xend = 2018.05, y = 0.42, yend = 0.62, 
           arrow = arrow(length = unit(0.2, "cm")), color = "#666666") +
  # Mise en place des labels pour 2011, 2017 et 2018
  geom_label(aes(x = 2008.5, y = 0.58, label = "Histoire des arts"), color = "#666666", fill = "white", size = 5, label.size = 0.5, label.padding = unit(0.3, "lines")) +
  geom_label(aes(x = 2015, y = 0.58, label = "Mise en place \ndu contrôle continu"), color = "#666666", fill = "white", size = 5, label.size = 0.5, label.padding = unit(0.3, "lines")) +
  geom_label(aes(x = 2014.5, y = 0.43, label = "2 séries : générale et \nprofessionnelle"), color = "#666666", fill = "white", size = 5, label.size = 0.5, label.padding = unit(0.3, "lines")) +
  geom_label(aes(x = 2018.5, y = 0.4, label = " Épreuves finales \nplus importantes"), color = "#666666", fill = "white", size = 5, label.size = 0.5, label.padding = unit(0.3, "lines")) +  # Configuration de l'axe Y et des labels
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0.25, 0.5, 0.75, 1),
                     limits = c(0, 1)) +
  scale_color_manual(
    values = c(
      "assez_bien" =  "#D95F02", 
      "bien" = "#E6AB02",
      "très_bien" = "#66A61E", 
      "Moyenne générale" =  "#7570B3",
      "2006" = "#1B9E77", 
      "2011" = "#1B9E77", 
      "2017" = "#1B9E77", 
      "2018" = "#1B9E77", 
      "2013" = "#1B9E77",
      "2020" = "#E7298A"
    ),
    breaks = c("Moyenne générale", "assez_bien", "bien", "très_bien", "2011", "2020"),
    labels = c(
      "assez_bien" = "Mention Assez Bien",
      "bien" = "Mention Bien",
      "très_bien" = "Mention Très Bien",
      "Moyenne générale" = "Pourcentage de réussite",
      "2011" = "Réformes du brevet",
      "2020" = "Covid-19"
    )
  ) +
  scale_x_continuous(
    breaks = seq(min(moyennes_long$year), max(moyennes_long$year), by = 1) # Toutes les années
  ) +
  scale_shape_manual(values = c(17, 18, 15, 16, 17, 18)) +  # Ensure you have a shape for each category
  guides(
    shape = "none", # Make sure the shapes are visible
    color = guide_legend(override.aes = list(shape = c(16, 17, 18, 15, 17, 18)))  # Use correct shapes for color legend
  ) +
  labs(
    title = "Pourcentage de réussite du brevet et des mentions dans le temps",
    color = "Légende",
    linetype = "Légende",
    caption = "Source : Data.education.gouv.fr"
  ) +
  theme_minimal() +
  theme(
    # Garder les barres verticales principales
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_line(color = "gray80", linetype = "solid"),
    # Conserver les axes
    axis.line = element_line(color = "black"),
    plot.title = element_text(face = "bold", size = 25),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.5, "lines"),
    legend.key.spacing.y = unit(0.17, "cm"),
    legend.title = element_text(size = 15, face = "bold"),
    plot.caption = element_text(size = 12, hjust = 1, face = "italic"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_blank()
    
  )




  colors_set2 <- brewer.pal(n = 8, name = "Dark2")  # "Set2" a jusqu'à 8 couleurs
  print(colors_set2)  # Liste des couleurs en format hexadécimal

