
library(shiny)
library(plotly)
library(shinyjs)

# Fonction pour mettre à jour le damier selon les règles spécifiées
update_damier <- function(damier) {
  nb_lignes <- nrow(damier)
  nb_colonnes <- ncol(damier)
  
  # Créer une copie du damier pour stocker les nouvelles valeurs
  nouveau_damier <- matrix(0, nrow = nb_lignes, ncol = nb_colonnes)
  
  # Parcourir chaque case du damier
  for (i in 1:nb_lignes) {
    for (j in 1:nb_colonnes) {
      # Compter le nombre de cases noires voisines
      nb_noires_voisines <- sum(damier[max(1, i - 1):min(nb_lignes, i + 1),
                                       max(1, j - 1):min(nb_colonnes, j + 1)]) - damier[i, j]
      
      # Appliquer les règles du jeu de la vie de Conway
      if (damier[i, j] == 1) {
        if (nb_noires_voisines == 2 | nb_noires_voisines == 3) {
          nouveau_damier[i, j] <- 1  # Reste noir
        } else {
          nouveau_damier[i, j] <- 0  # Devient blanc
        }
      } else {
        if (nb_noires_voisines == 3) {
          nouveau_damier[i, j] <- 1  # Devient noir
        } else {
          nouveau_damier[i, j] <- 0  # Reste blanc
        }
      }
    }
  }
  
  return(nouveau_damier)
}

# Interface Shiny
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML('
           #damierPlot {
           min-width: 1000px;
           min-height: 1000px;
           max-width: 1000px;
           max-height: 1000px;
           overflow-x: auto;
           }
           '
      )
    )
  ),
  
  actionButton("startButton", "Start"),
  actionButton("stopButton", "Stop"),
  actionButton("restartButton", "Restart"),
  
  plotlyOutput("damierPlot")
)

server <- function(input, output, session) {
  # Définir les dimensions du damier
  nb_lignes <- 200
  nb_colonnes <- 200
  
  # Créer un damier aléatoire de départ
  damier <- reactiveVal(matrix(sample(0:1, nb_lignes * nb_colonnes, replace = TRUE), nrow = nb_lignes, ncol = nb_colonnes))
  
  # Mettre à jour le damier toutes les secondes
  autoInvalidate <- reactiveTimer(100, session)
  observeEvent(autoInvalidate(), {
    damier(update_damier(damier()))
    output$damierPlot <- renderPlotly({
      data <- data.frame(
        x = rep(1:nb_colonnes, each = nb_lignes),
        y = rep(nb_lignes:1, nb_colonnes),
        z = as.vector(damier())
      )
      plot_ly(data, x = ~x, y = ~y, type = "heatmap", z = ~z,
              colorscale = list(list(0, "white"), list(1, "black")),
              showscale = FALSE) %>%
        layout(
          xaxis = list(showticklabels = FALSE, showline = FALSE, title = ""),
          yaxis = list(showticklabels = FALSE, showline = FALSE, title = "")
        )
    })
  })
  
}

#shinyApp(ui = ui, server = server)
