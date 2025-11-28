# HEADER --------------------------------------------
#
# Author:     Charles Thévenin
# Copyright     Copyright 2024 - Charles Thévenin
# Email:      charles.thevenin@mnhn.fr
#
# Date:     2025-10-31
#
# Script Name:    fonctions/create_df_spipoll.R
#
# Script Description:   Création du data frame qubs avec les données du Spipoll
#   Import depuis les exports standardisés sur le serveur FTP
#
#
# ------------------------------------

library(dplyr)
library(here)
require(httr)
library(readr)

if (Sys.getenv("CI") != "true") {
  readRenviron(".env")
}

source("functions/var.R")

# Requête sur la base de données ftp
# - Données Spipoll export à plat
# print("Requête des données récentes")
if (!exists("req_spipoll")) {
  req_spipoll <- GET(
    paste0(Sys.getenv('SITE_NAME'), "export_spipoll.csv"),
    authenticate(Sys.getenv('FTP_USER'), Sys.getenv('FTP_PASSWORD'), type = "basic")
  )
}

dt_vers <- readr::read_csv2(content(req_spipoll, "raw"))


# - Données Spipoll comments
# print("Requête des données ensemble de la période")
if (!exists("req_spipoll_comments")) {
  req_escargots <- GET(
    paste0(Sys.getenv('SITE_NAME'), "export_spipoll_comments.csv"),
    authenticate(Sys.getenv('FTP_USER'), Sys.getenv('FTP_PASSWORD'), type = "basic")
  )
}

dt_spipoll_comments <- readr::read_csv2(content(req_spipoll_comments, "raw"))