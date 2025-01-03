


# Gamme #######################################################################

# Ex
tonique = "G"
mode = "major"

# 
find_scale <- function(tonique, mode, clav){
  scale <- NULL
  scale$tonique <- tonique
  scale$scale <- mode
  scale$notes <- buildScale(tonique, mode) %>%
    str_remove_all("[:digit:]") %>%
    format_notes()
  
  # Des tests de disponibilité
  scale$tonique <- format_notes(tonique)
  scale$main_droite$oui <- all(scale$notes %in% clav$plan$main_droite$note)
  scale$main_gauche$oui <- scale$tonique %in% clav$plan$main_gauche$note
  print(scale)
  return(scale)
}

scale <- find_scale(tonique, mode, clav)

write_about_scale <- function(scale){
  scale$name <- paste0(convert_to_french_notes(scale$tonique), " ", str_to_lower(convert_to_french_chords(scale$mode)))
  scale$description <- paste0("Cet scale est composé des notes ", combine_wordsFr(convert_to_french_notes(scale$notes)), ".")
  if(scale$tire$oui & scale$pousse$oui){scale$dispo <- "Il est disponible en poussé et en tiré."}
  if(scale$tire$oui & !scale$pousse$oui){scale$dispo <- "Il est disponible seulement en tiré."}
  if(!scale$tire$oui & scale$pousse$oui){scale$dispo <- "Il est disponible seulement en poussé."}
  if(!scale$tire$oui & !scale$pousse$oui){scale$dispo <- "Il n'est pas disponible."}
  return(scale)
  
}
scale$notes
scale <- write_about_scale(scale)




