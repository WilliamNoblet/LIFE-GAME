##FOUND PATTERN

library(dplyr)
library(ggplot2)
library(plotly)

####################################################################

generate_grid <- function(n) {
  matrix(0, n, n)
}

grid <- generate_grid(50)

####################################################################

add_pattern <- function(pattern, grid, x, y) {
  grid[x:(x+ncol(pattern)-1), y:(y+nrow(pattern)-1)] <- t(pattern)
  grid
}

####################################################################

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

####################################################################

#create all combinations

size <- 3

num_combinations <- 2^(size^2)

combinations <- expand.grid(rep(list(0:1), size^2))                 

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

for (i in seq_len(nrow(combinations))) {
  
  grid_data <- add_pattern(matrix(as.vector(t(combinations[i,1:9])), nrow = 3, byrow = TRUE), grid, 25, 25)
  first_grid <- grid_data
  
  n <- 0
  max_iterations <- 10
  type <- 'st'
  
  while (n <= max_iterations) {
    grid_data <- update_grid(grid_data)
    
     # Condition 1: all(grid_data == 0)
    if (all(grid_data == 0)) {
      type <- 'null'
      break
    }
    
    # Condition 2: grid_data == first_grid
    if (isTRUE(all.equal(grid_data, first_grid))) {
      type <- 'osc'
      break
    }
    
    # Condition 3: contains_matrix(grid_data, matrice)
    if (contains_matrix(grid_data, matrix(as.vector(t(combinations[i,1:9])), nrow = 3, byrow = TRUE)) & isFALSE(all.equal(grid_data, first_grid))) {
      type <- 'spaceships'
      break
    }
    
    n <- n + 1
  }
  
  # Mettre à jour les résultats
  combinations$type[i] <- type
  combinations$n[i] <- n
}

# Afficher les résultats
print(combinations)

combinations %>% dplyr::group_by(type) %>% dplyr::summarise(n = n(), .groups = 'drop')




library(openxlsx)

write.xlsx(combinations, 'C:/Users/William/Desktop/LIFE-GAME/patterns.xlsx')


write.xlsx(combinations, 'C:/Users/User/OneDrive/Desktop/LIFE-GAME/patterns.xlsx')












