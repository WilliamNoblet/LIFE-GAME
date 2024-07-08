#Oscillators

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

detection_repet <- function(grid_data, history_grid) {
  for (prev_grid in history_grid) {
    if (identical(grid_data, prev_grid)) {
      return(TRUE)
    }
  }
  return(FALSE)
}


####################################################################

i = 449

matrice <- matrix(as.vector(t(combinations[i,1:9])), nrow = 3, byrow = TRUE)

grid_data <- add_pattern(matrix(as.vector(t(combinations[i,1:9])), nrow = 3, byrow = TRUE), grid, 25, 25)

first_grid <- grid_data

n <- 0

result <- ''

history_grid <- list()

repeat {
  previous_grid <- grid_data
  grid_data  <- update_grid(grid_data)
  n <- n + 1
  
  if (detection_repet(grid_data, history_grid)) {
    result <- 'oscillator'
    break
  }
  
  history_grid <- c(history_grid, list(previous_grid))
  
  
  if (all(grid_data == 0) & n < 50) {
    result <- 'null'
    break
  } else if (identical(grid_data, previous_grid) & n < 50) {
    result <- 'stable'
    break
  }
  
}

result

n

























