
# PARTICIPATION ----------------------------------------------------------------

stats_participation <- dt_spipoll %>%
  group_by(annee) %>%
  summarise(nb_participants = n_distinct(user_id),
            nb_collections = n_distinct(collection_id))
# ajouter le nombre de nouveaux participants
nouveaux_participants <- dt_spipoll %>%
  group_by(user_id) %>%
  summarise(first_participation = min(unique(collection_date))) %>%
  ungroup() %>%
  mutate(annee = year(first_participation)) %>%
  group_by(annee) %>% 
  summarise(nouveaux_participants = n())
# jointure
stats_participation <- left_join(stats_participation, 
                                 nouveaux_participants, 
                                 by = "annee")
rm(nouveaux_participants)
# ajouter infos sur nb de collections moyen/participant, ainsi que nb de participations uniques 
# et "grosse participants" (gros participant = >40 collections dans l'année)
moyenne_participation <- dt_spipoll %>%
  group_by(annee, user_id) %>%
  summarise(nb_collections = n_distinct(collection_id)) %>%
  group_by(annee) %>%
  summarise(nb_collections_moyen = median(nb_collections),
            participations_uniques = sum(nb_collections == 1),
            gros_participants = sum(nb_collections > 40))
stats_participation <- left_join(stats_participation,
                                 moyenne_participation,
                                 by = "annee")
rm(moyenne_participation)


# COMMUNAUTE ----------------------------------------------------------------

stats_communaute <- dt_spipoll %>%
  group_by(annee) %>%
  summarise(nb_id_validees = sum(nb_validation == 3),
            nb_id_non_validees = sum(nb_validation != 3))

# ajouter les validations, suggestions & commentaires
communaute <- social_spipoll %>%
  group_by(annee) %>%
  summarise(nb_validations = sum(typeId == "Identification validée"),
            nb_suggestions = sum(typeId == "Identification suggérée"),
            nb_commentaires = sum(!is.na(comment)),
            nb_valideurs = n_distinct(user_id[typeId == "Identification validée"]))
stats_communaute <- left_join(stats_communaute,
                              communaute,
                              by = "annee") %>%
  arrange(desc(annee))
rm(social_spipoll)
rm(communaute)



# OBSERVATIONS ----------------------------------------------------------------

# STATS GENERALES : 
stats_observation <- dt_spipoll %>%
  group_by(annee) %>%
  summarise(nb_interactions = n(),
            nb_plantes = n_distinct(plante_long_name),
            nb_photos = n()+sum(!is.na(insecte_photo_2)))
# ajouter nombre de photos pour des taxons terminaux
photos_taxons_terminaux <- dt_spipoll %>%
  filter(insecte_rang == "ES") %>%
  group_by(annee) %>%
  summarise(nb_photo_taxons_terminaux = n()+sum(!is.na(insecte_photo_2)))
stats_observation <- left_join(stats_observation,
                               photos_taxons_terminaux,
                               by = "annee")
rm(photos_taxons_terminaux)
# ajouter le nombre d'interactions moyen par collection
moyenne_observation <- dt_spipoll %>%
  group_by(annee, collection_id) %>%
  summarise(diversite = n()) %>%
  group_by(annee) %>%
  summarise(diversite_moyenne = mean(diversite))
stats_observation <- left_join(stats_observation,
                               moyenne_observation,
                               by = "annee")
rm(moyenne_observation)


# TOP ESPECES
# calcul du nombre d'obs par taxon par annee
obs_taxons_annees <- dt_spipoll %>% 
  group_by(annee, insecte_long_name) %>%
  summarise(nb_observations = n()) %>%
  data.table::as.data.table() %>%
  data.table::dcast(insecte_long_name ~ annee, value.var = 'nb_observations')
obs_taxons_annees[is.na(obs_taxons_annees)] <- 0
# calcul des rangs : classement des espece par ordre decroissant du nombre d'observations par annee
rangs_taxons_annees <- dt_spipoll %>% 
  group_by(annee, insecte_long_name) %>%
  summarise(nb_observations = n())
rangs_taxons_annees[is.na(rangs_taxons_annees)] <- 0
rangs_taxons_annees <- rangs_taxons_annees %>%
  group_by(annee) %>%
  mutate(rang = rank(-nb_observations, ties.method = "min")) %>%
  data.table::as.data.table() %>%
  data.table::dcast(insecte_long_name ~ annee, value.var = 'rang')


# TOP GRANDS GROUPES TAXONOMIQUES
# calcul du nombre d'obs par grand groupe par annee
obs_groupes_annees <- dt_spipoll %>% 
  #retirer les interactions non attribuées (groupes == "Non attribué")
  filter(groupes != "Non attribué") %>%
  group_by(annee, groupes) %>%
  summarise(nb_observations = n()) %>%
  data.table::as.data.table() %>%
  data.table::dcast(groupes ~ annee, value.var = 'nb_observations')
obs_groupes_annees[is.na(obs_groupes_annees)] <- 0
# calcul du pourcentage d'obs pour chaque grand groupe par annee
pourcents_groupes_annees <- dt_spipoll %>% 
  #retirer les interactions non attribuées (groupes == "Non attribué")
  filter(groupes != "Non attribué") %>%
  group_by(annee, groupes) %>%
  summarise(nb_observations = n()) %>%
  mutate(pourcentage_obs = round((nb_observations/sum(nb_observations))*100, 2)) %>%
  select(annee, groupes, pourcentage_obs) %>%
  data.table::as.data.table() %>%
  data.table::dcast(groupes ~ annee, value.var = 'pourcentage_obs')
# calcul des rangs : classement des grands groupes par ordre decroissant du nombre d'observations par annee
rangs_groupes_annees <- dt_spipoll %>% 
  #retirer les interactions non attribuées (groupes == "Non attribué") 
  filter(groupes != "Non attribué") %>%
  group_by(annee, groupes) %>%
  summarise(nb_observations = n())
rangs_groupes_annees[is.na(rangs_groupes_annees)] <- 0
rangs_groupes_annees <- rangs_groupes_annees %>%
  mutate(rang = rank(-nb_observations, ties.method = "min")) %>%
  data.table::as.data.table() %>%
  data.table::dcast(groupes ~ annee, value.var = 'rang')




# MENTIONS SPECIALES ------------------------------------------------------

# tableau avec les collections "speciales" (la plus haute, la plus basse...)
mentions_speciales <- synthese_collections %>% 
  select(collection_nom, collection_date, user_pseudo, altitude, diversite) %>%
  filter(year(collection_date) == annee_bilan) %>%
  filter(diversite == max(diversite) | 
           altitude == max(na.omit(altitude)) | 
           altitude == min(na.omit(altitude)))
