matrix(c(0, 1, 1, 1, 1, 1, 
         1, 0, 0, 0, 0, 1, 
         0, 0, 0, 0, 0, 1, 
         1, 0, 0, 0, 1, 0, 
         0, 0, 1, 0, 0, 0), nrow = 5, byrow = TRUE)

glider <- matrix(c(0, 1, 0, 
                   0, 0, 1, 
                   1, 1, 1), nrow = 3, byrow = TRUE)

blinker_2 <- matrix(c(1,
                      1,
                      1), nrow = 3, byrow = T)

patterns <- list(glider, blinker_2)

name <- c('Glider', 'Blinker (2)')

patterns_data <- data.frame(name = name, pattern = I(patterns))

patterns_data$pattern

mwss <- matrix(c(0, 1, 1, 1, 1, 1, 
                 1, 0, 0, 0, 0, 1, 
                 0, 0, 0, 0, 0, 1, 
                 1, 0, 0, 0, 1, 0, 
                 0, 0, 1, 0, 0, 0), nrow = 5, byrow = TRUE)

name <- 'Middle weight spaceship'

new_pattern <- data.frame(name = name, pattern = I(list(mwss)))

patterns_data <- rbind(patterns_data, new_pattern)


# 3 different types patterns , still lifes, oscillators, spaceships


generate_grid <- function(n) {
  matrix(0, n, n)
}

grid <- generate_grid(50)

grid_data <- grid

add_pattern <- function(pattern, grid, x, y) {
  grid[x:(x+2), y:(y+2)] <- t(pattern)
  grid
}

grid_data <- add_pattern(blinker_2, grid_data, 25, 25)

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



#Créer une fonction qui permet de savoir l'évolution d'un pattern

size = 2

num_combinations <- 2^(size^2)

# Générer toutes les combinaisons possibles de 0 et 1
combinations <- expand.grid(rep(list(0:1), size^2))

# Convertir chaque combinaison en une matrice
matrices <- apply(combinations, 1, function(x) {
  matrix(x, nrow = size, byrow = TRUE)
})

matrices <- t(matrices)

i = 2

matrix(matrices[i,], nrow = 4, byrow = T)



size = 4

num_combinations <- 2^(size^2)

# Générer toutes les combinaisons possibles de 0 et 1
combinations <- expand.grid(rep(list(0:1), size^2))

# Convertir chaque combinaison en une matrice
matrices <- apply(combinations, 1, function(x) {
  matrix(x, nrow = size, byrow = TRUE)
})

matrices <- t(matrices)







library(gtools)


# Générer toutes les combinaisons de lignes possibles
line_combinations <- expand.grid(rep(list(0:1), size))

# Filtrer les combinaisons de lignes pour ne garder que les combinaisons uniques en termes de nombre de 1
line_combinations <- unique(t(apply(line_combinations, 1, function(x) sort(x, decreasing = TRUE))))

# Générer les combinaisons de matrices en combinant les lignes uniques
unique_matrices <- list()

generate_unique_matrices <- function(size) {
  # Générer toutes les combinaisons de lignes possibles
  line_combinations <- expand.grid(rep(list(0:1), size))
  
  # Filtrer les combinaisons de lignes pour ne garder que les combinaisons uniques en termes de nombre de 1
  line_combinations <- unique(t(apply(line_combinations, 1, function(x) sort(x, decreasing = TRUE))))
  
  # Générer les combinaisons de matrices en combinant les lignes uniques
  unique_matrices <- list()
  
  # Utiliser des combinaisons de lignes pour créer des matrices uniques
  generate_matrices <- function(current_matrix, remaining_lines) {
    if (nrow(current_matrix) == size) {
      # Ajouter la matrice unique si elle n'est pas déjà présente
      is_unique <- TRUE
      for (existing in unique_matrices) {
        if (identical(existing, current_matrix)) {
          is_unique <- FALSE
          break
        }
      }
      
      if (is_unique) {
        unique_matrices <<- append(unique_matrices, list(current_matrix))
      }
      return()
    }
    
    for (i in 1:nrow(remaining_lines)) {
      new_matrix <- rbind(current_matrix, remaining_lines[i, ])
      generate_matrices(new_matrix, remaining_lines[-i, , drop = FALSE])
    }
  }
  
  generate_matrices(matrix(ncol = size, nrow = 0), line_combinations)
  
  return(unique_matrices)
}

# Utiliser la fonction pour générer toutes les matrices 4x4 uniques
unique_matrices_4x4 <- generate_unique_matrices(4)

# Afficher le nombre total de matrices générées
print(length(unique_matrices_4x4))

# Afficher quelques exemples de matrices générées
if (length(unique_matrices_4x4) > 0) {
  print(unique_matrices_4x4[[1]])
}
if (length(unique_matrices_4x4) > 1) {
  print(unique_matrices_4x4[[2]])
}
if (length(unique_matrices_4x4) > 2) {
  print(unique_matrices_4x4[[3]])
}







