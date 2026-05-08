



# Accord #######################################################################

# Ex
root = "C"
chord = "major"

# 
find_chord <- function(root, chord, clav){
  accord <- NULL
  accord$root <- root
  accord$chord <- chord
  accord$notes <- buildChord(root, chord) %>%
    str_remove_all("[:digit:]") %>%
    format_notes()
  
  # Des tests de disponibilité
  accord$root <- format_notes(root)
  accord$pousse$main_droite$oui <- all(accord$notes %in% clav$plan$main_droite$note[clav$plan$main_droite$soufflet == "P"])
  accord$tire$main_droite$oui <- all(accord$notes %in% clav$plan$main_droite$note[clav$plan$main_droite$soufflet == "T"])
  accord$pousse$main_gauche$oui <- accord$root %in% clav$plan$main_gauche$note[clav$plan$main_gauche$soufflet == "P"]
  accord$tire$main_gauche$oui <- accord$root %in% clav$plan$main_gauche$note[clav$plan$main_gauche$soufflet == "T"]
  accord$pousse$oui <- accord$pousse$main_droite$oui & accord$pousse$main_gauche$oui
  accord$tire$oui <- accord$tire$main_droite$oui & accord$tire$main_gauche$oui
  #print(accord)
  return(accord)
}

accord <- find_chord(root, chord, clav)

write_about_chord <- function(accord){
  accord$name <- paste0(convert_to_french_notes(accord$root), " ", str_to_lower(convert_to_french_chords(accord$chord)))
  accord$description <- paste0("Cet accord est composé des notes ", combine_wordsFr(convert_to_french_notes(accord$notes)), ".")
  if(accord$tire$oui & accord$pousse$oui){accord$dispo <- "Il est disponible en poussé et en tiré."}
  if(accord$tire$oui & !accord$pousse$oui){accord$dispo <- "Il est disponible seulement en tiré."}
  if(!accord$tire$oui & accord$pousse$oui){accord$dispo <- "Il est disponible seulement en poussé."}
  if(!accord$tire$oui & !accord$pousse$oui){accord$dispo <- "Il n'est pas disponible."}
  return(accord)

}

accord <- write_about_chord(accord)
notes <- accord$notes

generer_renversements <- function(notes) {
  
  n <- length(notes)
  
  l <- lapply(seq_len(n), function(i) {
    c(
      notes[i:n],
      head(notes, i - 1)
    )
  })
  l <- as.data.frame(l)
  names(l) <- c("Fond.", paste0(c("1er", "2ème", "3ème", "4ème")[1:(n-1)], " renv."))
  return(l)
}
renversements <- generer_renversements(accord$notes)

ordre_notes <- accord$notes
trouver_positions <- function(acc_pos, ordre_notes) {
  
  positions <- list()
  
  premiere <- acc_pos %>%
    filter(note == ordre_notes[1]) %>%
    slice_min(numero, n = 1)
  
  positions[[1]] <- premiere
  
  for(k in 2:length(ordre_notes)) {
    
    derniere_pos <- positions[[k - 1]]
    
    candidate <- acc_pos %>%
      filter(note == ordre_notes[k]) %>%
      mutate(
        coord_dist =
          abs(coordx - unique(derniere_pos$coordx)) +
          abs(coordy - unique(derniere_pos$coordy))
      ) %>%
      slice_min(coord_dist, n = 1) %>%
      select(-coord_dist)
    
    positions[[k]] <- candidate
  }
  
  bind_rows(positions)
}
