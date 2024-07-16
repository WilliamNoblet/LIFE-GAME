#Spaceships

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


detection_repet <- function(grid_data, history_grid) {
  for (prev_grid in history_grid) {
    if (identical(grid_data, prev_grid)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

####################################################################

glider <- matrix(c(0, 1, 0, 
                   0, 0, 1, 
                   1, 1, 1), nrow = 3, byrow = TRUE)

grid_data <- add_pattern(t(glider), grid, 25, 25)

first_grid <- grid_data

n <- 0

result <- ''

history_grid <- list()

repeat {
  previous_grid <- grid_data
  grid_data  <- update_grid(grid_data)
  n <- n + 1
  
  if (contains_matrix(grid_data, glider)) {
    result <- 'spaceship'
    break
  }
  

  
}

result

n

####################################################################


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
) %>%
  layout(
    annotations = list(
      x = 0.5,
      y = -0.15,
      xref = 'paper',
      yref = 'paper',
      text = paste('n :', n),
      showarrow = FALSE,
      xanchor = 'center',
      yanchor = 'bottom',
      font = list(size = 12)
    ),
    margin = list(b = 100) # Ajuste la marge inférieure si nécessaire
  )

####################################################################

for (i in seq_len(nrow(combinations))) {
  #grid_data <- add_pattern(matrix(as.vector(t(combinations[i,1:9])), nrow = 3, byrow = TRUE), grid, 25, 25)
  matrice <- matrix(unlist(combinations[i,1:9]), nrow = 3, byrow = TRUE)
  
  grid_data <- add_pattern(t(matrice), grid, 25, 25)
  
  first_grid <- grid_data
  
  history_grid <- list()
  
  n <- 0
  type <- 'null'
  
  repeat {
    previous_grid <- grid_data
    grid_data  <- update_grid(grid_data)
    n <- n + 1
    
    
    history_grid <- c(history_grid, list(previous_grid))
    
    
    if (all(grid_data == 0)) {
      type <- 'null'
      break
    } else if (identical(grid_data, previous_grid)) {
      type <- 'stable'
      break
    } else if (detection_repet(grid_data, history_grid)) {
      type <- 'oscillator'
      break
    } else if (contains_matrix(grid_data, matrice) & !detection_repet(grid_data, history_grid)) {
      type <- 'spaceship'
      break
    } else if (n > 50) {
      break
    }
    
  }
  
  # Mettre à jour les résultats
  combinations$type[i] <- type
  combinations$n[i] <- n
}


combinations %>% dplyr::group_by(type) %>% dplyr::summarise(n = n(), .groups = 'drop')

####################################################################


library(openxlsx)

write.xlsx(combinations, 'C:/Users/William/Desktop/LIFE-GAME/patterns_new.xlsx')


write.xlsx(combinations, 'C:/Users/User/OneDrive/Desktop/LIFE-GAME/patterns_new.xlsx')









####################################################################
####################################################################
####################################################################
####################################################################
####################################################################
####################################################################


