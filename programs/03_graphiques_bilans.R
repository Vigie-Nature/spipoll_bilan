# PARTICIPATION --------------------------------------------------------------

participation_mensuelle <- dt_spipoll %>%
  filter(annee %in% c((annee_bilan-annees_recul):annee_bilan)) %>%
  group_by(annee, mois) %>%
  summarise(nb_participations = n_distinct(collection_id)) %>%
  #ajouter un champ pour differencier l'annee focale des autres pour appliquer differents types de lignes
  mutate(annee_focale = if_else(annee == annee_bilan, "1", "2")) %>%
  #recreer un champ date a partir de l'annee et du numero du mois
  #correspondant au premier jour de chaque mois (format "%Y-%U-%u") 
  mutate(date_participation = as.Date(paste(annee_bilan, mois, "1", sep = "-"),
                                      format = "%Y-%m-%d"))

#plot participation mensuelle
plot_participation_mensuelle <- participation_mensuelle %>%
  ggplot(aes(x = date_participation,
             col = factor(annee),
             fill = factor(annee))) +
  geom_col(aes(y = nb_participations),
           position = position_dodge(),
           width = 20) +
  labs(title = "Participation mensuelle",
       x = "",
       y = "Nombre de collections",
       fill = "Année")+
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     expand = expansion(mult = c(0, .1))) +
  #modifier les couleurs pour intégrer la palette OPJ
  scale_fill_manual(values = rev(palette_opie[1:(annees_recul+1)])) +
  scale_color_manual(values = rev(palette_opie[1:(annees_recul+1)])) +
  #modifier l'axe des x pour affichage par mois
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        title = element_text(size = 16),
        legend.position = c(0.85,0.80),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12)) +
  guides(colour = "none")
#This actually save the plot in a image
ggsave(file = "participation_mensuelle.png", 
       plot = plot_participation_mensuelle,
       width = 10, 
       height = 6,
       path = here("reporting", "bilans_annuels", paste0(annee_bilan)))




# DIVERSITE ----------------------------------------------------------------


# Variations mensuelles de la diversite par collection
plot_diversite_mensuelle <- synthese_collections %>% 
  filter(annee == annee_bilan) %>%
  ggplot(aes(x = mois,
             y = diversite)) +
  geom_boxplot() +
  labs(title = "Diversité mensuelle",
       x = "",
       y = "Nombre de taxons observés") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        title = element_text(size = 16),
        legend.position = c(0.85,0.80),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))

#This actually save the plot in a image
ggsave(file = "diversite_mensuelle.png", 
       plot = plot_diversite_mensuelle,
       width = 10, 
       height = 6,
       path = here("reporting", "bilans_annuels", paste0(annee_bilan)))


# GRANDS GROUPES ----------------------------------------------------------------

# Variations mensuelles de la prévalence de chaque grand groupe taxonomique

# palette groupes taxo 2
palette_taxo_2 <- c("Hyménoptères" = "#EE6352",
                    "Diptères" = "#3FA7D6",
                    "Autres" = "#59CD90")

# calcul du pourcentage d'obs pour chaque grand groupe par annee
taxo_mensuel <- dt_spipoll %>% 
  #retirer les interactions non attribuées (groupes == "Non attribué")
  filter(groupes != "Non attribué") %>%
  #ne conserver que les donnees de l'annee focale
  filter(annee == annee_bilan) %>%
  mutate(mois = lubridate::month(collection_date, label = TRUE, abbr = TRUE),
         groupes_taxo = factor(ifelse(groupes %in% c("Hyménoptères", "Diptères"),
                                      groupes,
                                      "Autres"),
                               levels = c("Diptères", "Hyménoptères", "Autres"))) %>%
  group_by(mois, groupes_taxo) %>%
  summarise(nb_observations = n()) %>% 
  group_by(mois)

plot_taxo_mensuel <- taxo_mensuel %>%
  ggplot(aes(fill = groupes_taxo, y = nb_observations, x = mois)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = palette_taxo_2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Grands groupes taxonomiques",
       x = "Mois",
       y = "Pourcentage du total d'observations",
       fill = "Groupes") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), 
        title = element_text(size = 16),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title=element_text(size=15),
        legend.text=element_text(size=12))
#This actually save the plot in a image
ggsave(file = "taxo_mensuel.png", 
       plot = plot_taxo_mensuel,
       width = 10, 
       height = 6,
       path = here("reporting", "bilans_annuels", paste0(annee_bilan)))





# CARTO -------------------------------------------------------------------


# CARTO DES JARDINS AYANT OBSERVE L'ESPECE

#shapefile de la France pour carto (simplifiee)
France <- sf::st_simplify(sf::read_sf(here::here("maps", "metropole-version-simplifiee.geojson")),
                          dTolerance = 4000)
#coordonnees collections simplifiees
coordonnees_simplifiees <- dt_spipoll %>% 
  filter(annee == 2022) %>%
  select(longitude, latitude) %>%
  mutate(longitude = round(longitude, 2),
         latitude = round(latitude, 2)) %>%
  distinct() %>%
  sf::st_as_sf(coords = c("longitude", 
                          "latitude"),
               crs = 4326)
#carte collections
carte_collections <- tm_shape(France) +
  tm_borders(lwd = 3, col = "#1d1d1b") +
  tm_shape(sf::st_filter(coordonnees_simplifiees,France)) + 
  tm_symbols(shape = 20, size = 0.8, col = "#b23e4c") +
  tm_layout(frame = FALSE, 
            title.position = c('left', 'bottom'),
            title.size = 2.5)
tmap_save(carte_collections,
          filename = here::here("reporting", "bilans_annuels", paste0(annee_bilan),
                                paste0(annee_bilan, "_carte_collections.png")))

