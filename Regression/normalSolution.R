normal_equ <- function(X,Y){
  theta = solve(t(X)%*%X)%*%t(X)%*%Y
  return(theta)
}

mean_normalize <- function(X) {
  apply(X, 2, function(col) {
    (col - mean(col))/(max(col) - min(col))
  }) 
}

mean_normalize_y <- function(Y){
  res = sapply(1:length(Y), function(i){
    (Y[i] - mean(Y)) / (max(Y) - min(Y))
  })
}


library("ISLR")
auto_data = Auto

# # for 2 dimension
X = matrix(0, nrow = nrow(auto_data), ncol = 2)
X[,1] = rep(1, nrow(auto_data))
X[,2] = auto_data$horsepower
X = as.matrix(X)
Y = as.matrix(auto_data$mpg)
theta = normal_equ(X,Y)
print(theta)
# plot(X[,2],Y)
# abline(theta[1], theta[2])

# for all dimensions
X = matrix(0, nrow = nrow(auto_data), ncol = 8)
X[, 1] = rep(1, nrow(X))
X[, 2:8] = as.matrix(mean_normalize(auto_data[, 2:8]))
Y = as.matrix(auto_data$mpg)
res = normal_equ(X,Y)
print(res)