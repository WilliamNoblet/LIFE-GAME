library(dplyr)
library(ggplot2)
library(plotly)

####################################################################

size <- 3

num_combinations <- 2^(size^2)

combinations <- expand.grid(rep(list(0:1), size^2))                 

####################################################################

matrice <- matrix(as.vector(t(combinations[500,])), nrow = size, byrow = TRUE)







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



while (!isTRUE(all.equal(grid_data, first_grid)) && !all(grid_data == 0) && n < 10) {
  grid_data <- update_grid(grid_data)
  n <- n + 1
}

if (all(grid_data == 0)) {
  print("Matrice nulle détectée, arrêt du programme.")
}
















































