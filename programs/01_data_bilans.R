# DATA --------------------------------------------------------------------

# requete export a plat Spipoll
query <- read_sql_query(here::here("sql", "spipoll_export_a_plat.sql"))

# liste des champs non utilises pour l'instant (a modifier en fonction des ajouts)
champs_non_utilises <- c("user_email", "commentaire", "plante_fr", "plante_precision", "plante_caractere", "photo_fleur", 
                         "photo_plante", "photo_feuille", "photo_lieu", "distance_ruche", "grande_culture", 
                         "collection_heure_debut", "collection_heure_fin", "nebulosite", "temperature", "vent", "fleur_ombre", "insecte_vu_sur_fleur")

# import des donnees
dt_spipoll <- import_from_mosaic(query, 
                                 database_name = "spgp",
                                 force_UTF8 = TRUE) %>%
  #retirer les champs non utilises pour l"instant
  select(-any_of(champs_non_utilises)) %>%
  #conserver uniquement les données depuis 2010
  filter(year(collection_date) > 2009) %>%
  mutate(#colonne annee de la collection
    annee = year(collection_date),
    mois = month(collection_date),
    num_semaine = week(collection_date),
    #colonne groupe taxonomique
    groupes = ifelse(insecte_ordre %in% c("Blattodea",
                                          "Dermaptera",
                                          "Mecoptera",
                                          "Neuroptera",
                                          "Opiliones",
                                          "Orthoptera",
                                          "Ephemeroptera",
                                          "Collembola",
                                          "Raphidioptera"), 
                     "Autres", 
                     NA))

# Modification des niveaux de facteur
dt_spipoll[which(dt_spipoll$insecte_ordre == "Diptera"),]$groupes <- "Diptères"
dt_spipoll[which(dt_spipoll$insecte_ordre == "Hymenoptera"),]$groupes <- "Hyménoptères"
dt_spipoll[which(dt_spipoll$insecte_ordre == "Coleoptera"),]$groupes <- "Coléoptères"
dt_spipoll[which(dt_spipoll$insecte_ordre == "Lepidoptera"),]$groupes <- "Lépidoptères"
dt_spipoll[which(dt_spipoll$insecte_ordre == "Hemiptera"),]$groupes <- "Hemiptères"
dt_spipoll[which(dt_spipoll$insecte_ordre == "Araneae"),]$groupes <- "Arachnides"
dt_spipoll[which(is.na(dt_spipoll$insecte_ordre)),]$groupes <- "Non attribué"

# les NAs du champ "insecte_long_name" sont attribuées à "Insecte inconnu"
dt_spipoll$insecte_long_name[which(is.na(dt_spipoll$insecte_long_name))] <- "Insecte inconnu"

#remplacer les NAs par des zéros dans les champs 'protocole_long' et 'nb_validations'
dt_spipoll$protocole_long[is.na(dt_spipoll$protocole_long)] <- 0
dt_spipoll$nb_validation[is.na(dt_spipoll$nb_validation)] <- 0

#créer une colonne 'période" pour grouper les obs sur des périodes de 4 ans (à modifier dans l'argument breaks)
dt_spipoll <- dt_spipoll %>%
  mutate(periode = factor(cut(annee,
                              #Add 1 to the maximum value in dim to make sure it is included in the categorization.
                              breaks = c((seq(min(annee), max(annee), 4)), Inf),
                              #Set this to TRUE to include the lowest value
                              include.lowest = TRUE,
                              labels = FALSE,
                              #intervals are open on the right
                              right = FALSE)))


## Altitude des collections
#transformer les coordonnees des collections
coord_spipoll <- dt_spipoll %>% 
  select(collection_id, longitude, latitude) %>% 
  distinct() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
#ficher .tif avec altitudes
altitude_fr <- raster::raster(here("maps", "altitude", "Altitude.tif"))
#extraire les altitudes
coord_spipoll$altitude <- raster::extract(x = altitude_fr,
                                          y = coord_spipoll %>%
                                            st_transform(2154))
#ajouter l'altitude dans le jeu de donnees
dt_spipoll <- dt_spipoll %>%
  left_join(coord_spipoll %>%
              select(collection_id, altitude),
            by = "collection_id") %>%
  mutate(altitude = round(altitude, 2))

#
# importer les social events
query = read_sql_query(here::here("sql", "spipoll_social_events.sql"))
social_spipoll <- import_from_mosaic(query, database_name = "spgp", force_UTF8 = TRUE)
social_spipoll$typeId[which(social_spipoll$typeId == 1)] <- "Identification validée"
social_spipoll$typeId[which(social_spipoll$typeId == 2)] <- "Identification suggérée"
social_spipoll$typeId[which(social_spipoll$typeId == 3)] <- "Nouvelle identification de l'auteur"
social_spipoll$typeId[which(social_spipoll$typeId == 4)] <- "Commentaire"
social_spipoll$typeId[which(social_spipoll$typeId == 8)] <- "Notification"
social_spipoll$typeId[which(social_spipoll$typeId == 9)] <- "Ajout dénomination plus précise"

# SYNTHESE COLLECTIONS ----------------------------------------------------

# la diversite est calculee a partir du nombre de photo soumises
synthese_collections <- dt_spipoll %>%
  group_by(collection_id) %>%
  summarise(diversite = ifelse(sum(is.na(insecte_photo_1)) == 1, 0, n_distinct(insecte_photo_1))) %>%
  left_join(.,
            dt_spipoll %>% 
              select(collection_id,
                     collection_date,
                     collection_nom,
                     protocole_long,
                     user_pseudo,
                     altitude) %>%
              distinct(),
            by = "collection_id") %>%
  ungroup() %>%
  mutate(collection_date = lubridate::as_date(collection_date),
         annee = year(collection_date),
         mois = lubridate::month(collection_date, label = TRUE, abbr = TRUE))
