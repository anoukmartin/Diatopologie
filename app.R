##############################################
# Shiny App: DiaTopologie
# Description: Application pour visualiser les plans de clavier et accords
# Auteur: [Votre nom]
# Date: [Date]
##############################################

# Chargement des bibliothèques nécessaires
library(shiny)
library(bslib)
library(bsicons)
library(readxl)
library(music)

##############################################
# Chargement des données
##############################################
claviersMD <- read_excel("clavier_notes.xlsx", sheet = "claviersMD")
claviersMG <- read_excel("clavier_notes.xlsx", sheet = "claviersMG")

##############################################
# Interface Utilisateur (UI)
##############################################
# Thème personnalisé
my_theme <- bs_theme(
  bootswatch = "flatly",   # Style moderne
  primary = "grey",       # Couleur principale
  secondary = "#6C757D",  # Couleur secondaire
  font_scale = 0.7,        # Taille de la police
  base_font = font_google("Roboto")  # Police moderne et lisible
)

# Définition de l'interface utilisateur
ui <- page_sidebar(
  theme = my_theme,  # Application du thème
  title = div(icon("keyboard"), "DiaTopologie"),  # Titre avec icône
  
  # Barre latérale
  sidebar = sidebar(
    open = "always",
    accordion(
      # Panneau : Plan du clavier
      accordion_panel(
        title = "Plan du clavier",
        icon = bsicons::bs_icon("sliders"),
        selectizeInput(
          "rangsMD", 
          label = "Clavier main droite",
          choices = c("2 rangs", "2 rangs +2", "2 rangs et demi", "3 rangs"),
          selected = "3 rangs"
        ),
        selectizeInput(
          "planMD", 
          label = NULL, 
          choices = c("Sol/Do/Heim", "Sol/Do/JPL"),
          selected = "Sol/Do/Heim"
        ),
        selectizeInput(
          "basses", 
          label = "Clavier main gauche", 
          choices = c("8 basses", "12 basses", "18 basses"),
          selected = "18 basses"
        ),
        selectizeInput(
          "planMG", 
          label = NULL, 
          choices = c("Classique", "chelou"), 
          selected = "Classique"
        ),
        input_switch("tierces", 
                     label = "Tierces occultées", 
                     value = TRUE)
      ),
      
      # Panneau : Accord
      accordion_panel(
        title = "Accord",
        icon = bsicons::bs_icon("music-note"),
        input_switch("dispAccord", "Visualiser un accord", value = FALSE),
        uiOutput("choose_chord_ui")
      ),
      
      # Panneau : Gamme
      accordion_panel(
        title = "Gamme",
        icon = bsicons::bs_icon("music-note"),
        input_switch("dispGamme", "Visualiser une gamme", value = FALSE),
        uiOutput("choose_scale_ui")
      )
    )
  ),
  
  # Disposition principale
  layout_columns(
    # Graphique du clavier
    card(
      card_header("Visualisation du clavier"),
      layout_sidebar(
        plotOutput("clavier_plot"),
        sidebar = sidebar(
          icon = bsicons::bs_icon("music-note"),
          position = "right", 
          open = "closed", 
          input_switch(
            id = "dispNumeros", 
            label = "Afficher les numéros des touches", 
            value = TRUE
          ),
          input_switch(
            id = "dispNotes", 
            label = "Afficher toutes les notes", 
            value = TRUE
          ),
          downloadButton("downloadPlot", "Télécharger l'image")
        )
      )
    ),
    
    # Boîtes de valeur
    card(
      value_box(
        title = "Accord sélectionné",
        value = textOutput("accord"),
        showcase = bs_icon("music-note-beamed"),
        p(textOutput("accord_description"))
      ),
      value_box(
        title = "Gamme sélectionnée",
        value = "À venir",
        showcase = bs_icon("music-note-beamed"),
        p("À venir")
      )
    ),
    
    col_widths = c(8, 4)
  )
)

##############################################
# Logique Serveur
##############################################
server <- function(input, output) {
  
  # Génération dynamique de l'interface pour les accords
  output$choose_chord_ui <- renderUI({ 
    if (input$dispAccord) { 
      card(
        fill = TRUE,
        selectizeInput(
          "root", 
          label = "Fondamentale", 
          choices = c(
            "Do [C]" = "C", "Do# [C#]" = "C#", "Réb [Db]" = "C#",
            "Ré [D]" = "D", "Ré# [D#]" = "Eb", "Mib [Eb]" = "Eb",
            "Mi [E]" = "E", "F [F]" = "F", "F# [F#]" = "F#",
            "Solb [Gb]" = "F#", "Sol [G]" = "G", "Sol# [G#]" = "G#",
            "Lab [Ab]" = "G#", "La [A]" = "A", "La# [A#]" = "Bb",
            "Sib [Bb]" = "Bb", "Si [B]" = "B"
          ), 
          selected = "C"
        ),
        selectizeInput(
          "chord", 
          label = "Type d'accord", 
          choices = c(
            "Majeur" = "major", "Mineur" = "minor", "7ème" = "7th",
            "Majeur 7ème" = "major7th", "Mineur 7ème" = "minor7th", "Diminué" = "diminished",
            "Augmenté" = "augmented", "Diminué 7ème" = "diminished7th", 
            "Demi-diminué 7ème" = "halfDiminished7th", "Augmenté 7ème" = "7th#5",
            "Suspendu 2nd" = "sus2", "Suspendu quarte" = "sus4", 
            "6ème" = "6th", "Mineur 6ème" = "minor6th", "9ème" = "9th", 
            "Majeur 9ème" = "major9th", "Mineur 9ème" = "minor9th",
            "7ème 9ème augmentée" = "7th#9", "Étendu 9ème" = "added9th",
            "Mineur étendu 9ème" = "minorAdd9th", "Quinte (sans tierce)" = "5th",
            "Flat five" = "flat5th"
          ),
          selected = "minor"
        )
      )
    } 
  })
  
  # Génération dynamique de l'interface pour les gammes
  output$choose_scale_ui <- renderUI({ 
    if (input$dispGamme) { 
      card(
        fill = TRUE,
        selectizeInput(
          "tonique", 
          label = "Tonique", 
          choices = c(
            "Do [C]" = "C", "Do# [C#]" = "C#", "Réb [Db]" = "C#",
            "Ré [D]" = "D", "Ré# [D#]" = "Eb", "Mib [Eb]" = "Eb",
            "Mi [E]" = "E", "F [F]" = "F", "F# [F#]" = "F#",
            "Solb [Gb]" = "F#", "Sol [G]" = "G", "Sol# [G#]" = "G#",
            "Lab [Ab]" = "G#", "La [A]" = "A", "La# [A#]" = "Bb",
            "Sib [Bb]" = "Bb", "Si [B]" = "B"
          ), 
          selected = "C"
        ),
        selectizeInput(
          "scale", 
          label = "Mode", 
          choices = c(
            "Majeur" = "major", 
            "Mineur" = "minor", 
            "Dorien" = "dorian", 
            "Phrygien" = "phrygian", 
            "Lydien" = "lydian", 
            "Lydien augmenté" = "lydianAugmented", 
            "Acoustique" = "acoustic", 
            "Mixolydien" = "mixolydian", 
            "Locrien" = "locrian", 
            "Locrien majeur" = "majorLocrian", 
            "Majeur harmonique" = "harmonicMajor", 
            "Mineur harmonique" = "harmonicMinor", 
            "Demi-diminué" = "halfDiminished", 
            "Pentatonique mineur" = "minorPentatonic", 
            "Pentatonique majeur" = "majorPentatonic", 
            "Blues" = "blues", 
            "Altéré" = "altered", 
            "Hirajoshi" = "hirajoshi", 
            "Insen" = "insen", 
            "Algérien" = "algerian", 
            "Hongrois" = "hungarian", 
            "In" = "in"
          ),
          selected = "major"
        )
      )
    } 
  })
  
  # Rendu du graphique du clavier
  output$clavier_plot <- renderPlot({
    # Configuration des données du clavier
    clavier <- typeClavier(rangsMD = input$rangsMD, basses = input$basses)
    plan <- planClavier(MD = input$planMD, MG = input$planMG)
    
    clav <- calc_coord(clavier, plan)
    clav$halfcircles$main_droite <- build_halfcircles(clav$plan$main_droite)
    clav$halfcircles$main_gauche <- build_halfcircles(clav$plan$main_gauche)
    
    accord <- if (input$dispAccord) {
      find_chord(input$root, input$chord, clav)
    } else {
      find_chord("C", "minor", clav)
    }
    
    gamme <- if (input$dispGamme) {
      find_scale(input$tonique, input$scale, clav)
    } else {
      find_scale("C", "minor", clav)
    }
    
    create_clavier_plot(
      clav, accord, gamme, 
      dispNotes = input$dispNotes, 
      dispNumeros = input$dispNumeros, 
      dispAccord = input$dispAccord, 
      dispGamme = input$dispGamme
    )
  })
  
  # Texte des accords sélectionnés
  output$accord <- renderText({
    if (input$dispAccord) {
      paste(find_chord(input$root, input$chord, clav)$name)
    } else {
      "Aucun accord sélectionné"
    }
  })
  
  output$accord_description <- renderText({
    if (input$dispAccord) {
      paste(find_chord(input$root, input$chord, clav)$description)
    } else {
      ""
    }
  })
}

##############################################
# Exécution de l'application
##############################################
shinyApp(ui = ui, server = server)
