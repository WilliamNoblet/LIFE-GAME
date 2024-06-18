library(dplyr)
library(ggplot2)
library(plotly)

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
  grid[x:(x+ncol(pattern)-1), y:(y+nrow(pattern)-1)] <- t(pattern)
  grid
}

grid_data <- add_pattern(mwss, grid_data, 25, 25)

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




size <- 3

# Calcul du nombre total de combinaisons
num_combinations <- 2^(size^2)

# Génération de toutes les combinaisons possibles de 0 et 1
combinations <- expand.grid(rep(list(0:1), size^2))

# Conversion de chaque combinaison en matrice remplie par lignes
matrices <- apply(combinations, 1, function(x) {
  matrix(x, nrow = size, byrow = TRUE)
})

# Affichage de quelques exemples de matrices générées
for (i in 1:5) {
  print(matrices[[i]])
}


####################################################################
size <- 3

# Calcul du nombre total de combinaisons
num_combinations <- 2^(size^2)

# Génération de toutes les combinaisons possibles de 0 et 1
combinations <- expand.grid(rep(list(0:1), size^2))                 
####################################################################

matrice <- matrix(as.vector(t(combinations[500,])), nrow = size, byrow = TRUE)

matrice <- matrix(c(0, 1, 0,
                    0, 1, 0,
                    0, 1, 0), nrow = 3, byrow = TRUE)



generate_grid <- function(n) {
  matrix(0, n, n)
}

grid <- generate_grid(50)

grid_data <- grid

add_pattern <- function(pattern, grid, x, y) {
  grid[x:(x+ncol(pattern)-1), y:(y+nrow(pattern)-1)] <- t(pattern)
  grid
}

grid_data <- add_pattern(matrice, grid_data, 25, 25)

plot_ly(
  z = t(grid_data[nrow(grid_data):1, ]),
  type = "heatmap",
  colors = c("white", "black"),
  showscale = FALSE
) %>% layout(
  xaxis = list(showticklabels = FALSE, showline = FALSE, title = ""),
  yaxis = list(showticklabels = FALSE, showline = FALSE, title = "")
)






update_grid <- function(grid) {
  n <- nrow(grid)
  new_grid <- grid
  
  for (i in 1:n) {
    for (j in 1:n) {
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



grid_data <- update_grid(grid_data)


matrice <- matrix(c(0, 1, 0,
                    0, 1, 0,
                    0, 1, 0), nrow = 3, byrow = TRUE)

matrice <- matrix(combinations[150,], nrow = 3, byrow = T)

first_grid <- add_pattern(matrice, grid_data, 25, 25)

grid_data <- first_grid






for (i in seq(1:10)) {
  grid_data <- update_grid(grid_data)
  n <- 1
  
  if (!identical(grid_data, first_grid)) {
    grid_data <- update_grid(grid_data)
    n = n + 1
  } else {
    print(n)
  }
}


for (i in seq(1:10)) {
  grid_data <- update_grid(grid_data)
  n <- 1
  
  if (!isTRUE(all.equal(grid_data, first_grid))) {
    grid_data <- update_grid(grid_data)
    n = n + 1
  } else {
    print(n)
  }
}



first_grid <- add_pattern(matrice, grid_data, 25, 25)

grid_data <- first_grid


grid_data <- update_grid(grid_data)

n = 1

#Problème ici !!!!!!!!!!!!!

while (!isTRUE(all.equal(grid_data, first_grid)) && !all(grid_data == 0) && n < 10) {
  grid_data <- update_grid(grid_data)
  n <- n + 1
}

if (all(grid_data == 0)) {
  print("Matrice nulle détectée, arrêt du programme.")
}

# Afficher la valeur de n à la fin
print(n)






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









generate_matrices <- function(n, k) {
  # Generate all combinations of indices where 1s can be placed
  indices <- 1:(n * n)
  combinations <- combn(indices, k)
  
  # Initialize a list to store the matrices
  matrices <- list()
  
  # Loop through each combination to create the matrices
  for (i in 1:ncol(combinations)) {
    matrix <- matrix(0, n, n)
    for (index in combinations[, i]) {
      row <- ceiling(index / n)
      col <- (index - 1) %% n + 1
      matrix[row, col] <- 1
    }
    matrices[[i]] <- matrix
  }
  
  return(matrices)
}

# Example usage for a 4x4 matrix with exactly 2 ones
matrices <- generate_matrices(4, 2)

# Print a few examples
for (i in 1:min(5, length(matrices))) {
  cat("Matrix", i, ":\n")
  print(matrices[[i]])
  cat("\n")
}






"
pour une matrice d'une taille 3x3, 9 points possibles
a partir de ça on peut chercher le nombre de dessins pour des 1 parmis des 0
formule simple pour connaitre le nombre de dessins possible

0 -> 1 -> 1 

1 -> 9 -> 1

2 -> 36 -> 4

3 -> 84 -> 

4 -> 126

5 -> 126

6 -> 84

7 -> 36

8 -> 9

9 -> 1

"



prod(1:5)

n = 9

combinaisons <- function(n, k){
  result <- (prod(1:n))/(prod(1:k)*prod(1:(n-k)))
  result
}
combinaisons(9,1)


arrangement <- function(n, k){
  result <- (prod(1:n))/(prod(1:(n-k)))
  result
}
arrangement(9,3)

