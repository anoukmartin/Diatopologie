


# Gamme #######################################################################
modeaccords_gammemajeure = c("Majeur", "Mineur", "Mineur", "Majeur",  "Majeur", "Mineur", "Diminué")
# Ex
tonique = "G"
mode = "major"
buildScale()
# 
find_scale <- function(tonique, mode, clav){
  scale <- NULL
  scale$tonique <- tonique
  scale$mode <- mode
  scale$notes <- buildScale(tonique, mode) %>%
    str_remove_all("[:digit:]") %>%
    format_notes()
  scale$rem_tonique <- scale$notes[6]
  scale$rem_notes <- buildScale(scale$rem_tonique, "minor") %>%
    str_remove_all("[:digit:]") %>%
    format_notes()
  
  # Des tests de disponibilité
  scale$tonique <- format_notes(tonique)
  # En général
  scale$main_droite$oui <- all(scale$notes %in% clav$plan$main_droite$note)
  scale$main_droite$missing <- scale$notes[!(scale$notes %in% clav$plan$main_droite$note)]
  scale$main_droite$pousse_oui <- all(scale$notes %in% clav$plan$main_droite$note[clav$plan$main_droite$soufflet == "P"])
  scale$main_droite$pousse_missing <- scale$notes[!(scale$notes %in% clav$plan$main_droite$note[clav$plan$main_droite$soufflet == "P"])]
  scale$main_droite$tire_oui <- all(scale$notes %in% clav$plan$main_droite$note[clav$plan$main_droite$soufflet == "T"])
  scale$main_droite$tire_missing <- scale$notes[!(scale$notes %in% clav$plan$main_droite$note[clav$plan$main_droite$soufflet == "T"])]
  scale$main_gauche_tonique$oui <- scale$tonique %in% clav$plan$main_gauche$note
  scale$main_gauche$oui <- all(scale$notes %in% clav$plan$main_gauche$note)
  scale$main_gauche$missing <- scale$notes[!(scale$notes %in% clav$plan$main_gauche$note)]
  #print(scale)
  return(scale)
  
}

scale <- find_scale(tonique, mode, clav)

write_about_scale <- function(scale){
  scale$name <- paste0(convert_to_french_notes(scale$tonique), " ", str_to_lower(convert_to_french_chords(scale$mode)), 
                       " / ", convert_to_french_notes(scale$rem_tonique), " mineur naturel")
  scale$description <- paste0("Cette tonalitée, ", convert_to_french_notes(scale$tonique), " ", str_to_lower(convert_to_french_chords(scale$mode)),
                              ", est composé des notes ", combine_wordsFr(convert_to_french_notes(scale$notes)), ".\n\nSa relative, ",
                              convert_to_french_notes(scale$tonique), " mineur, est composé des notes ", 
                              combine_wordsFr(convert_to_french_notes(scale$rem_notes)), ".")
  scale$tableau <- data.frame(degrés = c("I", "II", "III", "IV", "V", "VI", "VII"), 
                              noms = c("Tonique", "Sus-tonique", "Médiante", "Sous-dominante", "Dominante", "Sus-dominante", "Sensible"), 
                              notes = convert_to_french_notes(scale$notes[-8]), 
                              accord = modeaccords_gammemajeure,
                              composition = c(
                                paste0(convert_to_french_notes(scale$notes[c(1, 3, 5)]), collapse = "-"), 
                                paste0(convert_to_french_notes(scale$notes[c(2, 4, 6)]), collapse = "-"), 
                                paste0(convert_to_french_notes(scale$notes[c(3, 5, 7)]), collapse = "-"), 
                                paste0(convert_to_french_notes(scale$notes[c(4, 6, 1)]), collapse = "-"), 
                                paste0(convert_to_french_notes(scale$notes[c(5, 7, 2)]), collapse = "-"), 
                                paste0(convert_to_french_notes(scale$notes[c(6, 1, 3)]), collapse = "-"), 
                                paste0(convert_to_french_notes(scale$notes[c(7, 2, 4)]), collapse = "-")))
  
  scale$tableau_mineur <- scale$tableau[c(6, 7, 1, 2, 3, 4, 5), ]
  scale$tableau_mineur$degrés <- c("I", "II", "III", "IV", "V", "VI", "VII")
                              
                          
  #kable(t(scale$tableau))

  # if(scale$tire$oui & scale$pousse$oui){scale$dispo <- "Il est disponible en poussé et en tiré."}
  # if(scale$tire$oui & !scale$pousse$oui){scale$dispo <- "Il est disponible seulement en tiré."}
  # if(!scale$tire$oui & scale$pousse$oui){scale$dispo <- "Il est disponible seulement en poussé."}
  # if(!scale$tire$oui & !scale$pousse$oui){scale$dispo <- "Il n'est pas disponible."}
  return(scale)
  
}
scale$notes
#scale <- write_about_scale(scale)




