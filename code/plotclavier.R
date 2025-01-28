

dispNotes <- F
clavier$main_droite
dispNumeros <- T

plotClavier <- function(clavier, plan, dispNotes = T, dispNumeros = T){
  ggplot() +
    xlim(-5, 15) +
    geom_point(aes(clavier$main_droite$coordx, clavier$main_droite$coordy), 
               size = 10, shape = 1, color = "grey") + 
    geom_point(aes(clavier$main_gauche$coordx, clavier$main_gauche$coordy), 
               size = 10, shape = 1, color = "grey") +
    {if(dispNotes) geom_text(aes(label = plan$main_droite$note, x = plan$main_droite$coordx, y = plan$main_droite$coordy),
              size = 2.5)} +
    {if(dispNumeros) geom_text(aes(label = clavier$main_droite$name, 
                                   x = clavier$main_droite$coordx - 0.55, y = clavier$main_droite$coordy + 0.55), 
                               size = 2, color = "grey")} +
    geom_text(aes(label = plan$main_gauche$note, x = plan$main_gauche$coordx, y = plan$main_gauche$coordy),
              size = 2.5) +
    annotate("text", label = "Haut (graves)", x = min(clavier$main_droite$coordx), y = max(clavier$main_droite$coordy) + 1.5, size = 2.5) +
    annotate("text", label = "Bas (aigus)", x = min(clavier$main_droite$coordx), y = min(clavier$main_droite$coordy) -1.5, size = 2.5) +
    theme_void()
}



library(dplyr)
library(ggplot2)

clavier <- typeClavier(rangsMD = "3 rangs", basses = "18 basses")
plan <- planClavier(MD = "Sol/Do/JPL", MG = "Classique", 
                    clavier = clavier)
clav <- calc_coord(clavier, plan)
plotClavier(clavier = clav$clavier, plan = clav$plan, 
            dispNotes = F, dispNumeros = T)


