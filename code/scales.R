# Liste des notes chromatiques
chromatic_scale <- c("C", "C#", "D", "Eb", "E", "F", "F#", "G", "G#", "A", "Bb", "B")

# Fonction pour générer l'échelle chromatique avec octaves
generate_scale_with_octaves <- function(octave_range = c(1, 7)) {
  scale_with_octaves <- c()
  
  # Générer les notes avec toutes les octaves dans la plage spécifiée
  for (octave in octave_range[1]:octave_range[2]) {
    for (note in chromatic_scale) {
      scale_with_octaves <- c(scale_with_octaves, paste0(note, octave))
    }
  }
  
  return(scale_with_octaves)
}

# Fonction pour trouver toutes les notes entre deux notes données, en prenant en compte les octaves
get_notes_between_with_octaves <- function(note1, note2, octave_range = c(1, 7)) {
  # Générer l'échelle chromatique avec octaves dans la plage spécifiée
  scale_with_octaves <- generate_scale_with_octaves(octave_range)
  
  # Vérifier si les notes sont présentes dans l'échelle
  if (!(note1 %in% scale_with_octaves) | !(note2 %in% scale_with_octaves)) {
    stop("Une ou les deux notes ne sont pas valides.")
  }
  
  # Trouver les indices des deux notes dans l'échelle
  index1 <- match(note1, scale_with_octaves)
  index2 <- match(note2, scale_with_octaves)
  
  # Si la deuxième note est avant la première, on fait une boucle circulaire
  if (index2 < index1) {
    index2 <- index2 + length(scale_with_octaves)  # Ajoute la longueur pour faire une boucle circulaire
  }
  
  # Extraire les notes entre les deux indices
  notes_between <- scale_with_octaves[index1:index2]
  
  return(notes_between)
}

# Exemple d'utilisation pour lister toutes les notes entre A2 et Bb5
get_notes_between_with_octaves("A2", "Bb5")
