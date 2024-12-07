required_packages <- c("sf", "leaflet","dplyr","stringr","htmlwidgets")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
sapply(required_packages,library,character.only = TRUE)

rm(list=ls())

data_sante <-read.csv2("donnees/liste_professionnels_sante_sxm.csv")
colnames(data_sante)<- tolower(colnames(data_sante))
# Nettoyer et renommer les colonnes (lat et long sont inversées)
names(data_sante)[names(data_sante) == "colonne1"] <- "denomination"

data_sante<- data_sante%>%
    filter(latitude != "")%>%
    mutate(
        longitude_save = longitude,
        longitude = latitude,
        latitude = longitude_save,
        longitude_save = NULL
    )
    
data_sante$longitude <- data_sante$longitude |>
    gsub(pattern= "°O$",replacement= "")|> 
    gsub(pattern= ",",replacement= ".")|>
    as.numeric()*-1

data_sante$latitude <- data_sante$latitude |>
    gsub(pattern= "°N$",replacement= "")|> 
    gsub(pattern= ",",replacement= ".")|>
    as.numeric()

# Normalisation des types
data_sante <- data_sante %>%
  mutate(type = tolower(type)) %>%  # Tout mettre en minuscule d'abord
  mutate(type = case_when(
    # Dentistes
    grepl("dentiste", type) ~ "Dentiste",
    
    # Matériel médical
    grepl("mat[ée]riel[s]? m[ée]dical", type) ~ "Matériel médical",
    
    # Ostéopathes
    grepl("os[th][ée]opathe", type) ~ "Ostéopathe",
    
    # Laboratoires
    grepl("labo", type) ~ "Laboratoire",
    
    # Psychologues
    grepl("psych?[ol]", type) ~ "Psychologue",
    
    # Ophtalmologistes
    grepl("ophtalm", type) ~ "Ophtalmologiste",
    
    # Cas particuliers
    type == "kiné" ~ "Kinésithérapeute",
    type == "cardio" ~ "Cardiologue",
    type == "gastro" ~ "Gastro-entérologue",
    type == "gynéco" ~ "Gynécologue",
    type == "pediatre" ~ "Pédiatre",
    type == "dermato" ~ "Dermatologue",
    type == "podologuue" ~ "Podologue",
    
    # Pour tous les autres cas, garder la valeur d'origine avec première lettre en majuscule
    TRUE ~ str_to_title(type)
  ))

type_to_categorie <- read.csv("donnees/type_to_categorie.csv")
data_sante_clean <- merge(data_sante ,type_to_categorie,by="type")

# Convertir en objet SF
data_sante_sf <- st_as_sf(data_sante_clean, 
                         coords = c("longitude", "latitude"),
                         crs = 4326)

color_palette <- c("#348888","#22BABB","#9EF8EE","#FA7F08","#F24405","red")

categories <- unique(data_sante_sf$categorie[data_sante_sf$type != "Non catégorisé"])
categorie_to_col <- setNames(c("black", color_palette),categories)
colors <- categorie_to_col[data_sante_sf$categorie]%>%unname()
# Créer la carte leaflet avec groupes
map <- leaflet(data_sante_sf) %>% 
  # Fonds de carte
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Mode") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  # Ajouter les points par type
  addCircleMarkers(
    color = "black",  # Contour noir
    weight = 2,       # Épaisseur du contour
    fillColor = colors,
    radius = 6,
    fillOpacity = 1,  # Opacité maximale du remplissage
    popup = ~sprintf("<b>Dénomination:</b> %s<br><b>Adresse:</b> %s<br><b>Type:</b> %s<br><b>Catégorie:</b> %s",
                    denomination, quartiers, type, categorie),
    group = ~type
  ) %>%
 # Contrôle des couches
  addLayersControl(
    baseGroups = c("Dark Mode","OpenStreetMap","Satellite"),
    overlayGroups = c(
      unique(data_sante_sf$type)
    ),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  
  # Masquer tous les groupes sauf Médecin Généraliste, Dentiste et Laboratoire
  hideGroup(setdiff(unique(data_sante_sf$type), 
                   c("Medecin Généraliste", "Dentiste", "Laboratoire"))) %>%
  
  # Légende en bas à gauche
  addLegend(
    colors = c("black", color_palette), 
    labels = categories,
    title = "Catégories",
    position = "bottomleft"
  )

# Sauvegarder la carte

saveWidget(
          map, 
          file = "index.html", 
          selfcontained = TRUE
          )
file.copy("index.html","docs/index.html")
file.remove("index.html")
