###
### Chat GPT
###   Simulation de Monte-Carlo
###     Estimation de la surface d'un lac


in_lake <- function(x, y, lake_x, lake_y) {
  # Vérifier si le point (x,y) est à l'intérieur ou à l'extérieur du lac
  num_vertices <- length(lake_x)
  inside <- FALSE
  
  for (i in seq_along(lake_x)) {
    x_i <- lake_x[i]
    y_i <- lake_y[i]
    x_j <- lake_x[(i %% num_vertices) + 1]
    y_j <- lake_y[(i %% num_vertices) + 1]
    
    if (((y_i > y) != (y_j > y)) & 
        (x < (x_j - x_i) * (y - y_i) / (y_j - y_i) + x_i)) {
      inside <- !inside
    }
  }
  
  return(inside)
}

# Définir les coordonnées du lac
lake_x <- c(2, 4, 6, 8, 8, 6, 4, 2)
lake_y <- c(5, 6, 7, 7, 5, 4, 4, 5)

cbind(lake_x, lake_y)

# Tracer le lac
plot(lake_x, lake_y, type = "l", xlim = c(0, 10), ylim = c(0, 10), , main="Calcul de la surface du lac", ylab="Coordonnées Y", xlab="Coordonnées X")


# Générer des points aléatoires dans la zone rectangulaire qui contient le lac
num_points <- 1000
x <- runif(num_points, 0, 10)
y <- runif(num_points, 0, 10)

cbind(x, y)


# Vérifier si chaque point est dans le lac
in_lake_indicator <- sapply(1:num_points, function(i) in_lake(x[i], y[i], lake_x, lake_y))

# Tracer les points dans le lac et en dehors du lac
points(x[in_lake_indicator], y[in_lake_indicator], col = "steelblue3")
points(x[!in_lake_indicator], y[!in_lake_indicator], col = "red3")


# Calculer la proportion de points dans le lac
proportion_in_lake <- mean(in_lake_indicator)

# Calculer la superficie du lac en multipliant la proportion par la superficie de la zone rectangulaire
lake_area <- proportion_in_lake * ((max_x-min_x)*(max_y-min_y))  # 10 * 10 (largeur * longueur de la zone rectangulaire)
lake_area
