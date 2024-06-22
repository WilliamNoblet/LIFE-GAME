library(dplyr)
library(ggplot2)
library(plotly)

####################################################################

size <- 3

num_combinations <- 2^(size^2)

combinations <- expand.grid(rep(list(0:1), size^2))                 

####################################################################

matrice <- matrix(as.vector(t(combinations[500,])), nrow = size, byrow = TRUE)

####################################################################

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


nrow(combinations)

matrice <- matrix(as.vector(t(combinations[512,])), nrow = size, byrow = TRUE)

grid <- generate_grid(50)

first_grid <- add_pattern(matrice, grid, 25, 25)

plot_ly(
  z = t(first_grid[nrow(first_grid):1, ]),
  type = "heatmap",
  colors = c("white", "black"),
  showscale = FALSE
) %>% layout(
  xaxis = list(showticklabels = FALSE, showline = FALSE, title = ""),
  yaxis = list(showticklabels = FALSE, showline = FALSE, title = "")
)

grid_data <- update_grid(first_grid)

####################################################################


n = 1

while (!isTRUE(all.equal(grid_data, first_grid)) && !all(grid_data == 0) && n < 10) {
  grid_data <- update_grid(grid_data)
  n <- n + 1
}

if (all(grid_data == 0)) {
  print("Matrice nulle détectée, arrêt du programme.")
}

n

plot_ly(
  z = t(grid_data[nrow(grid_data):1, ]),
  type = "heatmap",
  colors = c("white", "black"),
  showscale = FALSE
) %>% layout(
  xaxis = list(showticklabels = FALSE, showline = FALSE, title = ""),
  yaxis = list(showticklabels = FALSE, showline = FALSE, title = "")
)


####################################################################

matrice <- matrix(c(0, 0, 0,
                    0, 1, 0,
                    0, 1, 0), nrow = 3, byrow = TRUE)

first_grid <- add_pattern(matrice, grid, 25, 25)

grid_data <- update_grid(first_grid)

n = 1

while (!isTRUE(all.equal(grid_data, first_grid)) && !all(grid_data == 0) && n < 10) {
  grid_data <- update_grid(grid_data)
  n <- n + 1
}

n

####################################################################

contains_matrix <- function(grid, pattern) {
  grid_rows <- nrow(grid)
  grid_cols <- ncol(grid)
  pattern_rows <- nrow(pattern)
  pattern_cols <- ncol(pattern)
  
  for (i in 1:(grid_rows - pattern_rows + 1)) {
    for (j in 1:(grid_cols - pattern_cols + 1)) {
      sub_grid <- grid[i:(i + pattern_rows - 1), j:(j + pattern_cols - 1)]
      if (all(sub_grid == pattern)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

####################################################################

matrice <- matrix(as.vector(t(combinations[512,])), nrow = 3, byrow = TRUE)

first_grid <- add_pattern(matrice, grid, 25, 25)

grid_data <- update_grid(first_grid)


n = 1
max_iterations = 10

# Stocker les configurations précédentes
previous_grids <- list(grid_data)

# Vérifier si la matrice est dans le grid
contains_pattern <- contains_matrix(grid_data, matrice)

# Boucle principale
while (!isTRUE(all.equal(grid_data, first_grid)) && !all(grid_data == 0) && n < max_iterations) {
  grid_data <- update_grid(grid_data)
  n <- n + 1
  
  # Vérifier si le grid contient la matrice
  contains_pattern <- contains_matrix(grid_data, matrice)
  
  if (contains_pattern) {
    cat("La matrice est trouvée dans le grid après", n, "itérations.\n")
  }
  
  # Vérifier si la configuration du grid a déjà été vue
  if (any(sapply(previous_grids, function(g) isTRUE(all.equal(g, grid_data))))) {
    cat("Une configuration de grid répétée est détectée après", n, "itérations.\n")
    break
  }
  
  # Ajouter la configuration actuelle à la liste des configurations précédentes
  previous_grids <- append(previous_grids, list(grid_data))
}

cat("Nombre total d'itérations:", n, "\n")

####################################################################

matrice <- matrix(c(0, 1, 0, 
                   0, 0, 1, 
                   1, 1, 1), nrow = 3, byrow = TRUE)

first_grid <- add_pattern(matrice, grid, 25, 25)

grid_data <- update_grid(first_grid)


n = 1
max_iterations = 10

# Stocker les configurations précédentes
previous_grids <- list(grid_data)

# Vérifier si la matrice est dans le grid
contains_pattern <- contains_matrix(grid_data, matrice)

# Boucle principale
while (!isTRUE(all.equal(grid_data, first_grid)) && !all(grid_data == 0) && n < max_iterations) {
  grid_data <- update_grid(grid_data)
  n <- n + 1
  
  # Vérifier si le grid contient la matrice
  contains_pattern <- contains_matrix(grid_data, matrice)
  
  if (contains_pattern) {
    cat("La matrice est trouvée dans le grid après", n, "itérations.\n")
  }
  
  # Vérifier si la configuration du grid a déjà été vue
  if (any(sapply(previous_grids, function(g) isTRUE(all.equal(g, grid_data))))) {
    cat("Une configuration de grid répétée est détectée après", n, "itérations.\n")
    break
  }
  
  # Ajouter la configuration actuelle à la liste des configurations précédentes
  previous_grids <- append(previous_grids, list(grid_data))
}

cat("Nombre total d'itérations:", n, "\n")

####################################################################

library(dplyr)

# Fonction pour vérifier si une matrice est contenue dans une autre
contains_matrix <- function(grid, pattern) {
  grid_rows <- nrow(grid)
  grid_cols <- ncol(grid)
  pattern_rows <- nrow(pattern)
  pattern_cols <- ncol(pattern)
  
  for (i in 1:(grid_rows - pattern_rows + 1)) {
    for (j in 1:(grid_cols - pattern_cols + 1)) {
      sub_grid <- grid[i:(i + pattern_rows - 1), j:(j + pattern_cols - 1)]
      if (all(sub_grid == pattern)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# Initialiser le grid et la matrice de recherche
matrice <- matrix(c(0, 0, 0,
                    0, 1, 0,
                    0, 1, 0), nrow = 3, byrow = TRUE)

first_grid <- add_pattern(matrice, grid, 25, 25)

# Générer toutes les combinaisons possibles pour une matrice 3x3
size <- 3
num_combinations <- 2^(size^2)
combinations <- expand.grid(rep(list(0:1), size^2))

# Convertir chaque combinaison en matrice et les stocker dans une colonne du DataFrame
results <- data.frame(matrice = I(apply(combinations, 1, function(x) list(matrix(x, nrow = size, byrow = TRUE)))), 
                      type = character(num_combinations), 
                      n = integer(num_combinations), 
                      stringsAsFactors = FALSE)

# Boucle principale pour chaque matrice dans 'results'
for (i in seq_len(nrow(results))) {
  grid_data <- update_grid(first_grid)
  n <- 1
  max_iterations <- 10
  type <- 'st'
  
  while (n < max_iterations) {
    grid_data <- update_grid(grid_data)
    n <- n + 1
    
    # Condition 1: grid_data == first_grid
    if (isTRUE(all.equal(grid_data, first_grid))) {
      type <- 'osc'
      break
    }
    
    # Condition 2: all(grid_data == 0)
    if (all(grid_data == 0)) {
      type <- 'null'
      break
    }
    
    # Condition 3: contains_matrix(grid_data, matrice)
    if (contains_matrix(grid_data, matrice)) {
      type <- 'spaceships'
      break
    }
  }
  
  # Mettre à jour les résultats
  results$type[i] <- type
  results$n[i] <- n
}

# Afficher les résultats
print(results)


####################################################################

library(dplyr)

# Fonction pour vérifier si une matrice est contenue dans une autre
contains_matrix <- function(grid, pattern) {
  grid_rows <- nrow(grid)
  grid_cols <- ncol(grid)
  pattern_rows <- nrow(pattern)
  pattern_cols <- ncol(pattern)
  
  for (i in 1:(grid_rows - pattern_rows + 1)) {
    for (j in 1:(grid_cols - pattern_cols + 1)) {
      sub_grid <- grid[i:(i + pattern_rows - 1), j:(j + pattern_cols - 1)]
      if (all(sub_grid == pattern)) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# Initialiser le grid et la matrice de recherche
first_grid <- add_pattern(matrix(c(0, 0, 0, 0, 1, 0, 0, 1, 0), nrow = 3, byrow = TRUE), grid, 25, 25)

# Générer toutes les combinaisons possibles pour une matrice 3x3
size <- 3
num_combinations <- 2^(size^2)
combinations <- expand.grid(rep(list(0:1), size^2))

# Convertir chaque combinaison en matrice et les stocker dans une colonne du DataFrame
results <- data.frame(matrice = I(apply(combinations, 1, function(x) list(matrix(x, nrow = size, byrow = TRUE)))), 
                      type = character(num_combinations), 
                      n = integer(num_combinations), 
                      stringsAsFactors = FALSE)

# Fonction de mise à jour de la grille (doit être définie en fonction de votre contexte)
# update_grid <- function(grid) { ... }

# Boucle principale pour chaque matrice dans 'results'
for (i in seq_len(nrow(results))) {
  grid_data <- results$matrice[[i]]
  
  grid_data <- add_pattern(matrix(results$matrice[[i]], nrow = 3, byrow = TRUE), grid, 25, 25)
  n <- 1
  max_iterations <- 10
  type <- 'st'
  
  while (n <= max_iterations) {
    grid_data <- update_grid(grid_data)
    
    # Condition 1: grid_data == first_grid
    if (isTRUE(all.equal(grid_data, first_grid))) {
      type <- 'osc'
      break
    }
    
    # Condition 2: all(grid_data == 0)
    if (all(grid_data == 0)) {
      type <- 'null'
      break
    }
    
    # Condition 3: contains_matrix(grid_data, matrice)
    if (contains_matrix(grid_data, matrix(c(0, 0, 0, 0, 1, 0, 0, 1, 0), nrow = 3, byrow = TRUE))) {
      type <- 'spaceships'
      break
    }
    
    n <- n + 1
  }
  
  # Mettre à jour les résultats
  results$type[i] <- type
  results$n[i] <- n
}

# Afficher les résultats
print(results)














