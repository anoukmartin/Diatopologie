# Final plot 

# PLOT #########################################################################

# Création du graphique du clavier
pclavier <- ggplot() +
  theme_void() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.7, 0.2), 
        legend.title = element_blank()) +
  coord_fixed(ratio = 1, expand = TRUE, clip = "on")

# Ajout des demi-cercles
pclavier <- pclavier + 
  geom_polygon(
    data = clav$halfcircles$main_droite,
    aes(x = x, y = y, group = i),
    fill = "white",
    color = "grey", # Contour gris
    alpha = 0.7      # Transparence
  ) + 
  geom_polygon(
    data = clav$halfcircles$main_gauche,
    aes(x = x, y = y, group = i),
    fill = "white",
    color = "grey", # Contour gris      
    alpha = 0.7# Transparence
  )

size_notes <- 2.5
pclavier <- pclavier + 
  # Si l'accord est dispo en tiré : 
  ## On surligne les touches à la main droite
  {if(accord$tire$oui)geom_polygon(
    data = clav$halfcircles$main_droite %>%
      filter(soufflet == "T") %>%
      filter(note %in% accord$notes),
    aes(x = x, y = y, group = i, fill = soufflet),
    color = "grey", # Contour gris      
    alpha = 0.7# Transparence
  )} + 
  ## On ajoute le nom des  à la main droite
  {if(accord$tire$oui & !dispNotes)geom_text(
    data = clav$plan$main_droite %>%
      filter(soufflet == "T") %>%
      filter(note %in% accord$notes),
    aes(x = coordx, y = coordy-0.2, label = convert_to_french_notes(note)),
    size = size_notes
  )} +
  ## On surligne les notes à la main gauche 
  {if(accord$tire$oui)geom_polygon(
    data = clav$halfcircles$main_gauche %>%
      filter(soufflet == "T") %>%
      filter(note == accord$root),
    aes(x = x, y = y, group = i, fill = soufflet),
    color = "grey", # Contour gris      
    alpha = 0.7# Transparence
  )} + 
  ## On ajoute le nom des notes
  {if(accord$tire$oui & !dispNotes)geom_text(
    data = clav$plan$main_gauche %>%
      filter(soufflet == "T") %>%
      filter(note == accord$root),
    aes(x = coordx, y = coordy-0.2, label = convert_to_french_notes(note)),
    size = size_notes
  )} +
  # Si l'accord est dispo en poussé : 
  ## On surligne les touches à la main droite
  {if(accord$pousse$oui)geom_polygon(
    data = clav$halfcircles$main_droite %>%
      filter(soufflet == "P") %>%
      filter(note %in% accord$notes),
    aes(x = x, y = y, group = i, fill = soufflet),
    color = "grey", # Contour gris      
    alpha = 0.7# Transparence
  )} + 
  ## On ajoute le nom des  à la main droite
  {if(accord$pousse$oui & !dispNotes)geom_text(
    data = clav$plan$main_droite %>%
      filter(soufflet == "P") %>%
      filter(note %in% accord$notes),
    aes(x = coordx, y = coordy+0.2, label = convert_to_french_notes(note)),
    size = size_notes
  )} +
  ## On surligne les notes à la main gauche 
  {if(accord$pousse$oui )geom_polygon(
    data = clav$halfcircles$main_gauche %>%
      filter(soufflet == "P") %>%
      filter(note == accord$root),
    aes(x = x, y = y, group = i, fill = soufflet),
    color = "grey", # Contour gris      
    alpha = 0.7# Transparence
  )} + 
  ## On ajoute le nom des notes
  {if(accord$pousse$oui & !dispNotes)geom_text(
    data = clav$plan$main_gauche %>%
      filter(soufflet == "P") %>%
      filter(note == accord$root),
    aes(x = coordx, y = coordy+0.2, label = convert_to_french_notes(note)),
    size = size_notes
  )} 


pclavier

# Ajout des étiquettes pour les notes
dispNotes <- TRUE
dispNumeros <- TRUE # Option pour afficher les numéros des notes

pclavier <- pclavier +
  # Notes à la main droite
  {if (dispNotes) geom_text(
    data = clav$plan$main_droite[clav$plan$main_droite$soufflet == "P", ],
    aes(label = convert_to_french_notes(note), x = coordx, y = coordy + 0.2),
    size = size_notes
  )} +
  {if (dispNotes) geom_text(
    data = clav$plan$main_droite[clav$plan$main_droite$soufflet == "T", ],
    aes(label = convert_to_french_notes(note), x = coordx, y = coordy - 0.2),
    size = size_notes
  )} +
  {if (dispNumeros) geom_text(
    data = clav$clavier$main_droite,
    aes(label = name, x = coordx - 0.7, y = coordy),
    size = 2, color = "grey40"
  )} +
  {if (dispNotes) geom_text(
    data = clav$plan$main_gauche[clav$plan$main_gauche$soufflet == "P", ],
    aes(label = convert_to_french_notes(note), x = coordx, y = coordy + 0.2),
    size = size_notes
  )}+
  {if (dispNotes) geom_text(
    data = clav$plan$main_gauche[clav$plan$main_gauche$soufflet == "T", ],
    aes(label = convert_to_french_notes(note), x = coordx, y = coordy - 0.2),
    size = size_notes
  )} +
  annotate("text", label = "Haut (graves)", x = min(clav$clavier$main_droite$coordx), y = max(clav$clavier$main_droite$coordy) + 1.5, size = 2.5) +
  annotate("text", label = "Bas (aigus)", x = min(clav$clavier$main_droite$coordx), y = min(clav$clavier$main_droite$coordy) - 1.5, size = 2.5)


# Affichage du graphique
print(pclavier)


