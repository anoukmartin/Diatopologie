library(ggplot2)
library(dplyr)




  

# Définition de la fonction
create_clavier_plot <- function(clav, accord = NULL, scale = NULL, dispNotes = TRUE, dispNumeros = TRUE, dispAccord = TRUE, dispGamme = TRUE) {
  # Configuration initiale du graphique
  pclavier <- ggplot() +
    theme_void() +
    theme(
      legend.position = "none",
      legend.position.inside = c(0.7, 0.2),
      legend.title = element_blank()
    ) +
    coord_fixed(ratio = 1, expand = TRUE, clip = "on")
  
  # Ajout des demi-cercles représentant les claviers
  pclavier <- pclavier + 
    geom_polygon(
      data = clav$halfcircles$main_droite,
      aes(x = x, y = y, group = i),
      fill = "grey20",
      color = "white",
      alpha = 1
    ) + 
    geom_polygon(
      data = clav$halfcircles$main_gauche,
      aes(x = x, y = y, group = i),
      fill = "grey20",
      color = "white",
      alpha = 1
    )
  
  # Taille des textes pour les notes
  size_notes <- 2.5
  
  # On montre la game
  if(dispGamme){
    pclavier <- pclavier +
      geom_polygon(
        data = clav$halfcircles$main_droite %>%
            filter(!(note %in% scale$notes)),
          aes(x = x, y = y, group = i),
          fill = "grey90",
          color = "white",
        ) +
        geom_polygon(
          data = clav$halfcircles$main_gauche %>%
            filter(!(note %in% scale$notes)),
          aes(x = x, y = y, group = i), 
          fill = "grey90",
          color = "white",
        )
      
      if (!dispNotes) {
        pclavier <- pclavier +
          geom_text(
            data = clav$plan$main_droite[clav$plan$main_droite$soufflet == "T", ] %>%
              filter(note %in% scale$notes),
            aes(x = coordx, y = coordy - 0.2, label = convert_to_french_notes(note)),
            size = size_notes, 
            color = "white"
          ) +
          geom_text(
            data = clav$plan$main_droite[clav$plan$main_droite$soufflet == "P", ] %>%
              filter(note %in% scale$notes),
            aes(x = coordx, y = coordy + 0.2, label = convert_to_french_notes(note)),
            size = size_notes, 
            color = "white"
          ) +
          geom_text(
            data = clav$plan$main_gauche[clav$plan$main_gauche$soufflet == "T", ] %>%
              filter(note %in% scale$notes),
            aes(x = coordx, y = coordy - 0.2, label = convert_to_french_notes(note)),
            size = size_notes, 
            color = "white"
          ) +
          geom_text(
            data = clav$plan$main_gauche[clav$plan$main_gauche$soufflet == "P", ] %>%
              filter(note %in% scale$notes),
            aes(x = coordx, y = coordy + 0.2, label = convert_to_french_notes(note)),
            size = size_notes, 
            color = "white"
          ) 
      }
    }


  # Gestion des notes en tiré
  if(dispAccord){
  if (accord$tire$oui) {
    pclavier <- pclavier +
      geom_polygon(
        data = clav$halfcircles$main_droite %>%
          filter(soufflet == "T", note %in% accord$notes),
        aes(x = x, y = y, group = i, fill = soufflet),
        color = "white",
        alpha = 0.7
      ) +
      geom_polygon(
        data = clav$halfcircles$main_gauche %>%
          filter(soufflet == "T", note == accord$root),
        aes(x = x, y = y, group = i, fill = soufflet),
        color = "white",
        alpha = 0.7
      )
    
    if (!dispNotes) {
      pclavier <- pclavier +
        geom_text(
          data = clav$plan$main_droite %>%
            filter(soufflet == "T", note %in% accord$notes),
          aes(x = coordx, y = coordy - 0.2, label = convert_to_french_notes(note)),
          size = size_notes, 
          color = "white"
        ) +
        geom_text(
          data = clav$plan$main_gauche %>%
            filter(soufflet == "T", note == accord$root),
          aes(x = coordx, y = coordy - 0.2, label = convert_to_french_notes(note)),
          size = size_notes, 
          color = "white"
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
        color = "white",
        alpha = 0.7
      ) +
      geom_polygon(
        data = clav$halfcircles$main_gauche %>%
          filter(soufflet == "P", note == accord$root),
        aes(x = x, y = y, group = i, fill = soufflet),
        color = "white",
        alpha = 0.7
      )
    
    if (!dispNotes) {
      pclavier <- pclavier +
        geom_text(
          data = clav$plan$main_droite %>%
            filter(soufflet == "P", note %in% accord$notes),
          aes(x = coordx, y = coordy + 0.2, label = convert_to_french_notes(note)),
          size = size_notes, 
          color = "white",
        ) +
        geom_text(
          data = clav$plan$main_gauche %>%
            filter(soufflet == "P", note == accord$root),
          aes(x = coordx, y = coordy + 0.2, label = convert_to_french_notes(note)),
          size = size_notes, 
         color = "white",
        )
    }
  }
  }
  
  ## Légende du plot ####
  legendedata <- bind_rows(generate_half_circle(min(clav$clavier$main_gauche$coordx), 
                                                min(clav$clavier$main_gauche$coordy) -3, 0.5, "top"), 
                           generate_half_circle(min(clav$clavier$main_gauche$coordx), 
                                                min(clav$clavier$main_gauche$coordy) - 3, 0.5, "bottom"))
  legendedata$i <- c(rep(1, 100), rep(2, 100))
  
  # Ajout de la légende explicite
  pclavier <- pclavier +
    scale_fill_manual(
      values = c("T" = "#99d594", "P" = "#fc8d59"),
      labels = c("T" = "Tiré", "P" = "Poussé")
    ) + 
   
    #Légende custom 
    geom_polygon(
      data = legendedata,
      aes(x = x, y = y, group = i),
      fill = "grey20",
      color = "white",
    ) +
    geom_polygon(
      data = legendedata,
      aes(x = x + 2.5, y = y , group = i),
      fill = "grey20",
      color = "white",
    ) +
    geom_polygon(
      data = legendedata[legendedata$i == "2", ],
      aes(x = x, y = y, group = i),
      fill = "grey90",
      color = "white",
    ) +
    geom_polygon(
      data = legendedata[legendedata$i == "1", ],
      aes(x = x + 2.5, y = y, group = i),
      fill = "#99d594",
      color = "white",
      alpha = 0.6
    ) +
    geom_polygon(
      data = legendedata[legendedata$i == "2", ],
      aes(x = x + 2.5, y = y, group = i),
      fill = "#fc8d59",
      color = "white",
      alpha = 0.6
    ) + 
    geom_segment(aes(x = min(clav$clavier$main_gauche$coordx) - 1, xend = 10, 
                     y =  min(clav$clavier$main_gauche$coordy) - 3, 
                     yend = min(clav$clavier$main_gauche$coordy) - 3), 
                 size = 1) +
    annotate(geom = "text", y = 1.2, x = 2, 
             label = "Poussé") + 
    annotate(geom = "text", y = 0.8, x = 2, 
             label = "Tiré")
  
  # Ajout des étiquettes des notes et des numéros
  if (dispNotes) {
    pclavier <- pclavier +
      geom_text(
        data = clav$plan$main_droite %>%
          filter(soufflet == "P"),
        aes(label = convert_to_french_notes(note), x = coordx, y = coordy + 0.2),
        size = size_notes, 
        color = "white"
      ) +
      geom_text(
        data = clav$plan$main_droite %>%
          filter(soufflet == "T"),
        aes(label = convert_to_french_notes(note), x = coordx, y = coordy - 0.2),
        size = size_notes, 
        color = "white"
      ) +
      geom_text(
        data = clav$plan$main_gauche %>%
          filter(soufflet == "P"),
        aes(label = convert_to_french_notes(note), x = coordx, y = coordy + 0.2),
        size = size_notes, 
        color = "white"
      ) +
      geom_text(
        data = clav$plan$main_gauche %>%
          filter(soufflet == "T"),
        aes(label = convert_to_french_notes(note), x = coordx, y = coordy - 0.2),
        size = size_notes,
        color = "white"
      )
  }
  
  if (dispNumeros) {
    pclavier <- pclavier +
      geom_text(
        data = clav$clavier$main_droite,
        aes(label = name, x = coordx - 0.7, y = coordy),
        size = 2, color = "grey20"
      )
  }
  
  # Annotations supplémentaires
  pclavier <- pclavier +
    annotate("text", label = "Haut (graves)", x = min(clav$clavier$main_droite$coordx), y = max(clav$clavier$main_droite$coordy) + 1.5, size = 2.5) +
    annotate("text", label = "Bas (aigus)", x = min(clav$clavier$main_droite$coordx), y = min(clav$clavier$main_droite$coordy) - 1.5, size = 2.5)
  
  # Retourner le graphique
  return(pclavier)
}

accord <- find_chord("F", "major", clav)
create_clavier_plot(clav, accord, scale, dispNotes = T)
