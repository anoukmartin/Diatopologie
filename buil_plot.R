library(ggplot2)
library(dplyr)

# Définition de la fonction
create_clavier_plot <- function(clav, accord, dispNotes = TRUE, dispNumeros = TRUE, dispAccord = TRUE) {
  # Configuration initiale du graphique
  pclavier <- ggplot() +
    theme_void() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.7, 0.2),
      legend.title = element_blank()
    ) +
    coord_fixed(ratio = 1, expand = TRUE, clip = "on")
  
  # Ajout des demi-cercles représentant les claviers
  pclavier <- pclavier + 
    geom_polygon(
      data = clav$halfcircles$main_droite,
      aes(x = x, y = y, group = i),
      fill = "white",
      color = "grey",
      alpha = 0.7
    ) + 
    geom_polygon(
      data = clav$halfcircles$main_gauche,
      aes(x = x, y = y, group = i),
      fill = "white",
      color = "grey",
      alpha = 0.7
    )
  
  # Taille des textes pour les notes
  size_notes <- 2.5
  
  # Gestion des notes en tiré
  if(dispAccord){
  if (accord$tire$oui) {
    pclavier <- pclavier +
      geom_polygon(
        data = clav$halfcircles$main_droite %>%
          filter(soufflet == "T", note %in% accord$notes),
        aes(x = x, y = y, group = i, fill = soufflet),
        color = "grey",
        alpha = 0.7
      ) +
      geom_polygon(
        data = clav$halfcircles$main_gauche %>%
          filter(soufflet == "T", note == accord$root),
        aes(x = x, y = y, group = i, fill = soufflet),
        color = "grey",
        alpha = 0.7
      )
    
    if (!dispNotes) {
      pclavier <- pclavier +
        geom_text(
          data = clav$plan$main_droite %>%
            filter(soufflet == "T", note %in% accord$notes),
          aes(x = coordx, y = coordy - 0.2, label = convert_to_french_notes(note)),
          size = size_notes
        ) +
        geom_text(
          data = clav$plan$main_gauche %>%
            filter(soufflet == "T", note == accord$root),
          aes(x = coordx, y = coordy - 0.2, label = convert_to_french_notes(note)),
          size = size_notes
        )
    }
  }
  
  # Gestion des notes en poussé
  if (accord$pousse$oui) {
    pclavier <- pclavier +
      geom_polygon(
        data = clav$halfcircles$main_droite %>%
          filter(soufflet == "P", note %in% accord$notes),
        aes(x = x, y = y, group = i, fill = soufflet),
        color = "grey",
        alpha = 0.7
      ) +
      geom_polygon(
        data = clav$halfcircles$main_gauche %>%
          filter(soufflet == "P", note == accord$root),
        aes(x = x, y = y, group = i, fill = soufflet),
        color = "grey",
        alpha = 0.7
      )
    
    if (!dispNotes) {
      pclavier <- pclavier +
        geom_text(
          data = clav$plan$main_droite %>%
            filter(soufflet == "P", note %in% accord$notes),
          aes(x = coordx, y = coordy + 0.2, label = convert_to_french_notes(note)),
          size = size_notes
        ) +
        geom_text(
          data = clav$plan$main_gauche %>%
            filter(soufflet == "P", note == accord$root),
          aes(x = coordx, y = coordy + 0.2, label = convert_to_french_notes(note)),
          size = size_notes
        )
    }
  }
  }
  # Ajout de la légende explicite
  pclavier <- pclavier +
    scale_fill_manual(
      values = c("T" = "#99d594", "P" = "#fc8d59"),
      labels = c("T" = "Tiré", "P" = "Poussé")
    )
  
  # Ajout des étiquettes des notes et des numéros
  if (dispNotes) {
    pclavier <- pclavier +
      geom_text(
        data = clav$plan$main_droite %>%
          filter(soufflet == "P"),
        aes(label = convert_to_french_notes(note), x = coordx, y = coordy + 0.2),
        size = size_notes
      ) +
      geom_text(
        data = clav$plan$main_droite %>%
          filter(soufflet == "T"),
        aes(label = convert_to_french_notes(note), x = coordx, y = coordy - 0.2),
        size = size_notes
      ) +
      geom_text(
        data = clav$plan$main_gauche %>%
          filter(soufflet == "P"),
        aes(label = convert_to_french_notes(note), x = coordx, y = coordy + 0.2),
        size = size_notes
      ) +
      geom_text(
        data = clav$plan$main_gauche %>%
          filter(soufflet == "T"),
        aes(label = convert_to_french_notes(note), x = coordx, y = coordy - 0.2),
        size = size_notes
      )
  }
  
  if (dispNumeros) {
    pclavier <- pclavier +
      geom_text(
        data = clav$clavier$main_droite,
        aes(label = name, x = coordx - 0.7, y = coordy),
        size = 2, color = "grey40"
      )
  }
  
  # Annotations supplémentaires
  pclavier <- pclavier +
    annotate("text", label = "Haut (graves)", x = min(clav$clavier$main_droite$coordx), y = max(clav$clavier$main_droite$coordy) + 1.5, size = 2.5) +
    annotate("text", label = "Bas (aigus)", x = min(clav$clavier$main_droite$coordx), y = min(clav$clavier$main_droite$coordy) - 1.5, size = 2.5)
  
  # Retourner le graphique
  return(pclavier)
}


create_clavier_plot(clav, accord)
