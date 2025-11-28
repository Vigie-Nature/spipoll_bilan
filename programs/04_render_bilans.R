# RENDER ------------------------------------------------------------------

rmarkdown::render(here::here("R", "bilans_annuels", "template_bilans_spipoll.Rmd"),
                  output_file=here::here("reporting", "bilans_annuels",
                                         paste0(annee_bilan),
                                         paste0("bilan_",annee_bilan,".html")),
                  params=list(new_title=paste("SPIPOLL\nBilan -", annee_bilan)))
