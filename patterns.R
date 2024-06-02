matrix(c(0, 1, 1, 1, 1, 1, 
         1, 0, 0, 0, 0, 1, 
         0, 0, 0, 0, 0, 1, 
         1, 0, 0, 0, 1, 0, 
         0, 0, 1, 0, 0, 0), nrow = 5, byrow = TRUE)






# 3 different types patterns , still lifes, oscillators, spaceships

generate_grid <- function(n) {
  matrix(0, n, n)
}

grid <- generate_grid(50)

grid_data <- grid

add_glider <- function(grid, x, y) {
  pattern <- matrix(c(0, 1, 0, 
                      0, 0, 1, 
                      1, 1, 1), nrow = 3, byrow = TRUE)
  grid[x:(x+2), y:(y+2)] <- t(pattern)
  grid
}


25:(25+2)


grid_data <- add_glider(grid_data, 25, 25)







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


