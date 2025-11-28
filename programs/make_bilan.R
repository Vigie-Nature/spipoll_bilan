#' Visualisation spipoll
#' 
#' @description 
#' Créer des outils pour synthétiser la participation à l'observatoire du Spipoll
#' 
#' @author Charles Thévenin \email{thevenin.charles@gmail.com}
#' 
#' @date 2022/11/04




#packages
library(sf)
library(tmap)
library(RMySQL)
library(here)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(rmarkdown)
library(knitr)

## Load Project Addins (R Functions and Packages) ----


#Add new .env variables (access to database)
readRenviron(here::here(".env"))

#functions



## Run Project ----

# Créer un dossier spécifique pour l'année dans l'arboresence 
dir.create(here::here("reporting", "bilans_annuels", paste0(annee_bilan)), showWarnings = FALSE)

# List all R scripts in a sequential order and using the following form:
source(here::here("R", "bilans_annuels", "01_data_bilans.R"))
source(here::here("R", "bilans_annuels", "02_tableaux_bilans.R"))
source(here::here("R", "bilans_annuels", "03_graphiques_bilans.R"))
source(here::here("R", "bilans_annuels", "04_render_bilans.R"))
