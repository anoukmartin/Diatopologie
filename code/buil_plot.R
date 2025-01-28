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
  legendedata <- bind_rows(
    legendedata, 
    legendedata %>%
      mutate(x = x + 2.5)
  )
  legendedata$i <- c(rep(1, 100), 
                     rep(2, 100), 
                     rep(3, 100),
                     rep(4, 100))
  legendedata <- legendedata %>%
    mutate(soufflet = case_when(
      i == 1 ~ "G",
      i == 2 ~ "HG",
      i == 3 ~ "P",
      i == 4 ~ "T"
    )) %>%
    mutate(alpha = case_when(
      i == 1 ~ 0,
      i == 2 ~ 1,
      i == 3 ~ 0.6,
      i == 4 ~ 0.6)) %>%
    mutate(label = case_when(
      i == 1 ~ NA,
      i == 2 ~ "Hors gamme",
      i == 3 ~ "Accord en poussé",
      i == 4 ~ "Accord en tiré"))
  
  if(!dispAccord){
    legendedata <- legendedata %>%
      filter(i %in% c("1", "2"))
  }
  if(!dispGamme){
    legendedata <- legendedata %>%
      mutate(soufflet = if_else(
        soufflet == "HG", "G", soufflet),
        label = if_else(
          label == "Hors gamme", NA, label))
  }
  
  # Ajout de la légende explicite
  pclavier <- pclavier +
    scale_fill_manual(
      values = c("T" = "#99d594", "P" = "#fc8d59", 
                 "G" = "grey20", "HG" = "grey90")
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
      aes(x = x, y = y, group = i, 
          fill = soufflet, alpha = alpha), 
      color = "white"
    ) +
    geom_text_repel(data = legendedata %>%
                       group_by(i) %>%
                       summarise(x = mean(x), 
                                 y = mean(y), 
                                 label = unique(label)), 
                     aes(x = x, y = y, 
                         label = label), 
                    hjust = 0,
                    size = 2.5, 
                    nudge_x = c(-2, -2, 2, 2),
                    nudge_y = c(1, -1, 1, -1), 
                    color = "grey20") +
      geom_segment(aes(x = min(clav$clavier$main_gauche$coordx) - 1,
                       xend = 10,
                       y =  min(clav$clavier$main_gauche$coordy) - 3,
                       yend = min(clav$clavier$main_gauche$coordy) - 3),
                   size = 0.5, color = "grey20") +
    annotate(geom = "text",
             x = min(clav$clavier$main_gauche$coordx) + 1.2,
             y = min(clav$clavier$main_gauche$coordy) - 2.7,
             size = 2.5,
             label = "Poussé", 
             color = "grey20") +
    annotate(geom = "text",
             x = min(clav$clavier$main_gauche$coordx) + 1.2,
             y = min(clav$clavier$main_gauche$coordy) - 3.3,
             size = 2.5,
             label = "Tiré", 
             color = "grey20") 
  
  
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
create_clavier_plot(clav, accord, scale, dispNotes = T, dispAccord = F)

