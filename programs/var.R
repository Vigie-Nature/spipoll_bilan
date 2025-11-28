## Global Variables ----

# définir l'année focale du bilan (souvent année en cours - 1)
annee_bilan <- year(Sys.Date())-1

# Ddéfinir recul temporel pour la dataviz
annees_recul <- 1


# palette de couleur du Spipoll, manuellement assignée aux 7 groupes
palette_spipoll <- c("Diptères" = "#FF3300", #orange
                     "Hyménoptères" = "#B5F500", #vert
                     "Coléoptères" = "#D5002A", #rouge
                     "Lépidoptères" = "#FFE800", #jaune
                     "Hemiptères" = "#006C80", #bleu
                     "Arachnides" = "#4D0059", #violet
                     "Autres" = "#333333") #gris
palette_opie <- c("#232323", #noir logo
                  "#129877", #vert logo
                  "#b9b1a6", #gris clair
                  "#1976b7", #bleu
                  "#be385a") #rouge
