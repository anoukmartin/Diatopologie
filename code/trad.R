# Fonction pour convertir des notes du système américain au système français
convert_to_french_notes <- function(american_notes) {
  # Dictionnaire de correspondance entre les systèmes
  note_mapping <- list(
    "A" = "La",
    "A#" = "La#",
    "Bb" = "Sib",
    "B" = "Si",
    "C" = "Do",
    "C#" = "Do#",
    "Db" = "Réb",
    "D" = "Ré",
    "D#" = "Ré#",
    "Eb" = "Mib",
    "E" = "Mi",
    "F" = "Fa",
    "F#" = "Fa#",
    "Gb" = "Solb",
    "G" = "Sol",
    "G#" = "Sol#"
  )
  
  # Fonction de conversion pour un élément unique
  convert_note <- function(note) {
    if (note %in% names(note_mapping)) {
      return(note_mapping[[note]])
    } else {
      warning(paste("Note inconnue:", note))
      return(NA) # Retourne NA pour les notes non reconnues
    }
  }
  
  # Appliquer la conversion à chaque élément de la liste
  french_notes <- sapply(american_notes, convert_note)
  return(french_notes)
}

# Exemple d'utilisation
american_notes <- c("A", "Bb", "C#", "D", "F", "G#", "H")
convert_to_french_notes(american_notes)



# Fonction pour convertir des accords du système anglais au système français
convert_to_french_chords <- function(english_chords) {
  # Dictionnaire de correspondance entre les systèmes
  chord_mapping <- list(
    "major" = "Majeur",
    "minor" = "Mineur",
    "7th" = "7ème",
    "major7th" = "Majeur 7ème",
    "minor7th" = "Mineur 7ème",
    "diminished" = "Diminué",
    "augmented" = "Augmenté",
    "diminished7th" = "Diminué 7ème",
    "halfDiminished7th" = "Demi-diminué 7ème",
    "7th#5" = "Augmenté 7ème",
    "sus2" = "Suspendu 2nd",
    "sus4" = "Suspendu quarte",
    "6th" = "6ème",
    "minor6th" = "Mineur 6ème",
    "9th" = "9ème",
    "major9th" = "Majeur 9ème",
    "minor9th" = "Mineur 9ème",
    "7th#9" = "7ème 9ème augmentée",
    "added9th" = "Etendu 9ème",
    "minorAdd9th" = "Mineur étendu 9ème",
    "5th" = "Quinte (sans tierce)",
    "flat5th" = "Flat five"
  )
  
  # Fonction de conversion pour un élément unique
  convert_chord <- function(chord) {
    if (chord %in% names(chord_mapping)) {
      return(chord_mapping[[chord]])
    } else {
      warning(paste("Accord inconnu:", chord))
      return(NA) # Retourne NA pour les accords non reconnus
    }
  }
  
  # Appliquer la conversion à chaque élément de la liste
  french_chords <- sapply(english_chords, convert_chord)
  return(french_chords)
}

# Exemple d'utilisation
english_chords <- c("major", "minor", "7th", "augmented", "unknown")
convert_to_french_chords(english_chords)


format_notes <- function(list){
  list %>%
  str_replace_all("Db", "C#") %>%
  str_replace_all("D#", "Eb") %>%
  str_replace_all("Gb", "F#") %>%
  str_replace_all("Ab", "G#") %>%
  str_replace_all("A#", "Bb")
}


combine_wordsFr <- function(words) {
  words <- combine_words(words, and = " et ", oxford_comma = F)
  return(words)
}
