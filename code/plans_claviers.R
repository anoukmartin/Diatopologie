# === Bibliothèques nécessaires ===
library(data.table)
library(readxl)
library(dplyr)
library(stringr)

# === Section 1 : Définition du type de clavier ===

#' Définir le type de clavier (MD et MG)
#'
#' @param rangsMD Type de clavier pour la main droite. Options disponibles : "2 rangs", "2 rangs +2", "2 rangs et demi", "3 rangs".
#' @param basses Type de clavier pour la main gauche. Options disponibles : "8 basses", "12 basses", "18 basses".
#' @return Une liste contenant deux tables `main_droite` et `main_gauche`, qui décrivent les boutons du clavier.
#' @examples
#' typeClavier(rangsMD = "3 rangs", basses = "18 basses")
typeClavier <- function(rangsMD = c("2 rangs", "2 rangs +2", "2 rangs et demi", "3 rangs"), 
                        basses = c("8 basses", "12 basses", "18 basses")) {
  # Données des boutons pour différents claviers main droite (MD)
  # - 3 rangs MD
  MD3rangs <- data.table(
    name = c(paste0(1:12), paste0(1:11, "'"), paste0(1:10, "''")),
    numero = c(1:12, 1:11, 1:10), 
    rang = c(rep(1, 12), rep(2, 11), rep(3, 10))
  )
  
  # - 2 rangs MD
  MD2rangs <- data.table(
    name = c(paste0(1:11), paste0(1:10, "'")),
    numero = c(1:11, 1:10), 
    rang = c(rep(2, 11), rep(3, 10))
  )
  
  # Données des boutons pour différents claviers main gauche (MG)
  # - 18 basses MG
  MG18 <- data.table(
    name = c(rep(NA, 18)),
    numero = c(rep(1:6, 3)), 
    rang = c(rep(1, 6), rep(2, 6), rep(3, 6))
  )
  
  # - 12 basses MG
  MG12 <- data.table(
    name = c(rep(NA, 12)),
    numero = c(rep(1:6, 2)), 
    rang = c(rep(1, 6), rep(2, 6))
  )
  
  # - 8 basses MG
  MG8 <- data.table(
    name = c(rep(NA, 8)),
    numero = c(rep(1:4, 2)), 
    rang = c(rep(1, 4), rep(2, 4))
  )
  
  # Sélection des données en fonction des paramètres fournis
  if (rangsMD == "3 rangs") {MD <- MD3rangs}
  if (rangsMD == "2 rangs") {MD <- MD2rangs}
  if (basses == "18 basses") {MG <- MG18}
  if (basses == "12 basses") {MG <- MG12}
  if (basses == "8 basses") {MG <- MG8}
  
  # Retourne la configuration main droite et main gauche
  return(list(main_droite = MD, main_gauche = MG))
}

# Exemple d'utilisation
clavier <- typeClavier(rangsMD = "3 rangs", basses = "18 basses")

# === Section 2 : Chargement des données des claviers ===
claviersMD <- read_excel("clavier_notes.xlsx", sheet = "claviersMD")
claviersMG <- read_excel("clavier_notes.xlsx", sheet = "claviersMG")

# === Section 3 : Définition du plan des notes ===

#' Générer le plan des notes sur le clavier
#'
#' @param MD Type de disposition pour la main droite. Exemples : "Sol/Do", "Sol/Do/Heim", "Sol/Do/JPL".
#' @param MG Type de disposition pour la main gauche. Exemples : "Classique", "chelou".
#' @param clavier Un objet généré par `typeClavier`, décrivant la configuration main droite et gauche.
#' @return Une liste contenant deux tables : `main_droite` et `main_gauche`, avec les dispositions des notes.
#' @examples
#' planClavier(MD = "Sol/Do/JPL", MG = "Classique", clavier = typeClavier("3 rangs", "18 basses"))
#' 
planClavier <- function(MD = c("Sol/Do", "Sol/Do/Heim", "Sol/Do/JPL"),
                        MG = c("Classique", "chelou"), 
                        clavier = typeClavier(rangsMD = "3 rangs", basses = "18 basses")) {
  # Filtrer les données pour la main droite (MD)
  clavMD <- claviersMD %>% 
    filter(plan == MD) %>%
    semi_join(clavier$main_droite, by = c("numero", "rang"))
  
  # Filtrer les données pour la main gauche (MG)
  clavMG <- claviersMG %>% 
    filter(str_detect(Disposition, MG)) %>%
    semi_join(clavier$main_gauche, by = c("numero", "rang"))
  
  # Retourne les dispositions pour main droite et gauche
  return(list(main_droite = clavMD, main_gauche = clavMG))
}

# Exemple d'utilisation
plan <- planClavier(MD = "Sol/Do/JPL", MG = "Classique")
# plan$main_gauche <- plan$main_gauche %>%
#   mutate(accords_avectierces = case_when(
#     type == "a" ~ paste0(buildChord(note, tierce), collapse = ";")
#   ))

# === Section 4 : Calcul des coordonnées des boutons ===

#' Calculer les coordonnées des boutons du clavier
#'
#' @param clavier Un objet généré par `typeClavier`, décrivant la configuration main droite et gauche.
#' @param plan Un objet généré par `planClavier`, décrivant les dispositions des notes.
#' @return Une liste contenant les coordonnées des boutons pour `main_droite` et `main_gauche`.
#' @examples
#' calc_coord(clavier = typeClavier("3 rangs", "18 basses"), plan = planClavier("Sol/Do/JPL", "Classique"))
calc_coord <- function(clavier, plan) {
  # Ajouter les coordonnées pour la main droite
  clavier$main_droite <- clavier$main_droite %>%
    mutate(
      coordx = rang, 
      coordy = case_when(
        rang == 1 ~ -(numero * 1.5), 
        rang == 2 ~ -(numero * 1.5 + 0.75), 
        rang == 3 ~ -(numero * 1.5 + 1.5)
      )
    )
  m <- min(clavier$main_droite$coordy)
  M <- max(clavier$main_droite$coordy)
  # Ajouter les coordonnées pour la main gauche
  clavier$main_gauche <- clavier$main_gauche %>%
    mutate(coordx = -rang*1.2 + 10) %>%
    mutate(coordy = -numero*1.2 + (m  + (M - m)) - max(numero +1)*1.2/2)
  
  # Mettre à jour les coordonnées dans le plan pour la main droite
  plan$main_droite <- left_join(
    plan$main_droite,
    clavier$main_droite,
    by = c("numero", "rang")
  ) 
  #%>%    mutate(coordy = if_else(soufflet == "P", coordy + 0.5, coordy - 0.5))
  
  # Mettre à jour les coordonnées dans le plan pour la main gauche
  plan$main_gauche <- left_join(
    plan$main_gauche,
    clavier$main_gauche,
    by = c("numero", "rang")
  ) 
  #%>% mutate(coordy = if_else(soufflet == "P", coordy + 0.5, coordy - 0.5))
  
  # Retourner les coordonnées mises à jour
  return(list(clavier = clavier, plan = plan))
}



## Section 5 : construction des demi-cercles ###################################

# Initialisation des paramètres du clavier
clavier <- typeClavier(rangsMD = "3 rangs", basses = "18 basses")
plan <- planClavier(MD = "Sol/Do/Heim", MG = "Classique", clavier = clavier)
clav <- calc_coord(clavier, plan)


### Fonction outil ####
generate_half_circle <- function(x, y, r, direction = "top", n_points = N) {
  theta <- seq(0, pi, length.out = n_points) # Demi-cercle supérieur (par défaut)
  if (direction == "bottom") {
    theta <- seq(pi, 2 * pi, length.out = n_points) # Demi-cercle inférieur
  }
  
  data.frame(
    x = x + r * cos(theta),  # Coordonnées X
    y = y + r * sin(theta)   # Coordonnées Y
  )
}

# Test visuel de la fonction pour générer un demi-cercle
plot(generate_half_circle(1, 1, 0.5))
##

build_halfcircles <- function(dataTemp = clav$plan$main_droite){
  # Nombre de points pour dessiner les demi-cercles
  N <- 100
  # Initialisation pour chaque main
  # Création des demi-cercles pour les notes de la main droite
  dataTemp <-  dataTemp %>%
    mutate(type2 = if_else(soufflet == "P", "top", "bottom")) # Type de demi-cercle (Poussé/Tiré)
  dataTemp$i <- 1:nrow(dataTemp)
  temp <- lapply(1:nrow(dataTemp), function(i) {
    demi <- generate_half_circle(
      x = dataTemp$coordx[i], 
      y = dataTemp$coordy[i], 
      r = 0.5,              # Rayon du demi-cercle
      direction = dataTemp$type2[i]
    )
    demi$i <- i
    demi <- left_join(demi, dataTemp, by = "i")
  })
  temp <- bind_rows(temp)
  return(temp)
}

clav$halfcircles$main_droite <- build_halfcircles(clav$plan$main_droite)
clav$halfcircles$main_gauche <- build_halfcircles(clav$plan$main_gauche)
