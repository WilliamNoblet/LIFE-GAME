library(shiny)
library(plotly)

ui <- (fluidPage(
  
  tags$head(
    tags$style(
      HTML('
      
      body {
                                background-color: #333333; /* To change the background color. */
                                color: #ffffff; /* To change the font color. */
                              }
      
           #plot {
           min-width: 500px;
           min-height: 500px;
           max-width: 500px;
           max-height: 500px;
           overflow-x: auto;
           }
           '
      )
    )
  ),
  
  titlePanel("LIFE GAME"),
  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start"),
      actionButton("stop", "Stop"),
      actionButton("reset", "Reset"),
      actionButton("randomize", "Randomize"),
      helpText("Cliquez sur les cellules pour les activer/désactiver."),
      
    ),
    
    sidebarLayout(
      
      sidebarPanel(
        sliderInput("taille",
                    "Taille de la matrice:",
                    min = 10,
                    max = 100,
                    value = 50),
        
        sliderInput("sec",
                    "Secondes:",
                    min = 200,
                    max = 5000,
                    value = 1000)
      ),
      
      mainPanel(
        plotlyOutput("plot")
      )
    )
  )
))


library(shiny)
library(plotly)

# Fonction pour générer une grille vide
generate_grid <- function(n) {
  matrix(0, n, n)
}

# Fonction pour générer une grille aléatoire
random_grid <- function(n) {
  matrix(sample(0:1, n * n, replace = TRUE), n, n)
}

# Fonction pour mettre à jour la grille selon les règles du jeu de la vie
update_grid <- function(grid) {
  n <- nrow(grid)
  new_grid <- grid
  
  for (i in 1:n) {
    for (j in 1:n) {
      # Comptage des voisins vivants
      neighbors <- sum(grid[max(1, i-1):min(n, i+1), max(1, j-1):min(n, j+1)]) - grid[i, j]
      
      if (grid[i, j] == 1 && (neighbors < 2 || neighbors > 3)) {
        new_grid[i, j] <- 0
      } else if (grid[i, j] == 0 && neighbors == 3) {
        new_grid[i, j] <- 1
      }
    }
  }
  
  new_grid
}

# Taille de la grille
#grid_size <- 20

server <- (function(input, output, session) {
  
  
  
  #grid_size <- input$taille
  
  grid <- reactiveVal(generate_grid(n = 50))
  
  running <- reactiveVal(FALSE)
  
  # Observer le bouton Start
  observeEvent(input$start, {
    running(TRUE)
  })
  
  # Observer le bouton Stop
  observeEvent(input$stop, {
    running(FALSE)
  })
  
  observeEvent(input$reset, {
    running(FALSE)
    grid(generate_grid(input$taille))
  })
  
  observeEvent(input$randomize, {
    running(FALSE)
    grid(random_grid(input$taille))
  })
  
  
  
  
  observe({
    if (running()) {
      invalidateLater(input$sec, session)
      isolate({
        #n <- input$taille
        
        #grid <- reactiveVal(generate_grid(n))
        
        grid(update_grid(grid()))
      })
    }
  })
  
  
  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    if (!is.null(click)) {
      j <- floor(click$y) + 1
      i <- input$taille - floor(click$x)
      
      new_grid <- grid()
      new_grid[i, j] <- 1 - new_grid[i, j]
      grid(new_grid)
    }
  })
  
  output$plot <- renderPlotly({
    
    #n <- input$taille
    
    #grid <- reactiveVal(generate_grid(n))
    
    grid_data <- grid()
    plot_ly(
      z = t(grid_data[nrow(grid_data):1, ]),
      type = "heatmap",
      colors = c("white", "black"),
      showscale = FALSE
    ) %>% layout(
      #yaxis = list(
      #  scaleanchor = "x",
      #  scaleratio = 1
      #),
      xaxis = list(showticklabels = FALSE, showline = FALSE, title = ""),
      yaxis = list(showticklabels = FALSE, showline = FALSE, title = "")
    )
  })
})


shinyApp(ui = ui, server = server)


