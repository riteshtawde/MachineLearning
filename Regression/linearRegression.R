#http://ruder.io/optimizing-gradient-descent/index.html#momentum
#install.packages("plotly")
# Batch Gradient Descent using ADAM
library(plotly)
linear_hypothesis = function(X, Y, theta) {
  #print("came here")
  return (X %*% theta)
}

adam_gd = function(data_X, data_Y, dim) {
  epochs = 1e+05
  mini_batch_size = 50
  start = 1
  nextCycle = FALSE
  
  #parameters using adam
  theta = as.matrix(rep(0, dim))
  m_t = as.matrix(rep(0, dim))
  v_t = as.matrix(rep(0, dim))
  m_t_hat = as.matrix(rep(0, dim))
  v_t_hat = as.matrix(rep(0, dim))
  gradient = as.matrix(rep(0, dim))
  alpha = 0.01
  epsilon = 10e-08
  beta1 = 0.9
  beta2 = 0.999
  t = 1
  
  # J(theta) costs
  # to simulate based on batch size
  total_iter = epochs * ceiling((nrow(X) / mini_batch_size))
  cost = rep(0, total_iter)
  
  index = 1
  old_theta = as.matrix(rep(0, dim))
  for (iter in seq(total_iter)) {
    old_theta = theta
    X = data_X
    Y = data_Y
    
    if (nextCycle) {
      sample_X = sample(nrow(X), replace = FALSE)
      X = X[sample_X, ]
      Y = Y[sample_X, ]
      start = 1
    }
    nextCycle = FALSE
    end = start + mini_batch_size - 1
    if (end > nrow(X)) {
      end = nrow(X)
      nextCycle = TRUE
    }
    
    X_batch = X[start:end, ]
    Y_batch = Y[start:end]
    
    #######################
    #adam params
    m_t = beta1 * m_t + (1 - beta1) * gradient
    v_t = beta2 * v_t + (1 - beta2) * (gradient ^ 2)
    
    m_t_hat = m_t / (1 - (beta1 ^ t))
    v_t_hat = v_t / (1 - (beta2 ^ t))
    
    #######################
    
    #######################
    # gradient step
    hypoth = linear_hypothesis(X_batch, Y_batch, theta)
    cost[iter] = sum((hypoth - Y_batch)^2) / length(Y_batch)
    gradient = t(X_batch) %*% (hypoth - Y_batch)
    theta = theta - alpha * m_t_hat / (sqrt(v_t_hat) + epsilon)
    #######################
    
    #######processing at end
    start = end + 1
    t = t + 1
    
    if (all(abs(theta - old_theta) < 0.000001) && iter > 1)
      break
  }
  
  #cat('theta : ',theta,'\n',', iter :',iter)
  return (list("theta"=theta,"cost"=cost))
}

mean_normalize <- function(X) {
  apply(X, 2, function(col) {
    (col - mean(col)) / (max(col) - min(col))
  })
}

# mean_normalize_y <- function(Y){
#   res = sapply(1:length(Y), function(i){
#     (Y[i] - mean(Y)) / (max(Y) - min(Y))
#   })
# }

test_normalize <- function(col, val) {
  (val - mean(col)) / (max(col) - min(col))
}

library("ISLR")
auto_data = Auto

#Question 1.1
# for 2 dimension
X = matrix(0, nrow = nrow(auto_data), ncol = 2)
X[, 1] = rep(1, nrow(X))
X[, 2] = as.matrix(auto_data$horsepower)
Y = as.matrix(auto_data$mpg)
start_time = Sys.time()
res_uni = adam_gd(X, Y, dim(X)[2])
print('univariate lr')
print(Sys.time() - start_time)
#negative relationship

#Question 1.2
plot(X[, 2],
     Y,
     xlab = "Input variable",
     ylab = "Output variable",
     main = "Linear Regression(Uni)")
abline(res_uni[1], res_uni[2], col = "red")

#Question 1.3
hp = 220
mpg = res_uni$theta[1] + res_uni$theta[2] * 220
print(mpg)

#Question 1.4
cost = res_uni$cost
#Question 1.6
#for all dimensions
X = matrix(0, nrow = nrow(auto_data), ncol = 8)
X[, 1] = rep(1, nrow(X))
X[, 2:8] = as.matrix(mean_normalize(auto_data[, 2:8]))
Y = as.matrix(auto_data$mpg)
start_time = Sys.time()
res_multi = adam_gd(X, Y, dim(X)[2])
print('multivariate lr')
print(Sys.time() - start_time)

theta_multi = res_multi$theta
#Question 1.7
mpg = theta_multi[1] + theta_multi[2] * test_normalize(auto_data$cylinders, 4) + theta_multi[3] *
  test_normalize(auto_data$displacement, 300) + theta_multi[4] * test_normalize(auto_data$horsepower, 200) + theta_multi[5] *
  test_normalize(auto_data$weight, 3500) + theta_multi[6] * test_normalize(auto_data$acceleration, 11) + theta_multi[7] * test_normalize(auto_data$year, 70) + theta_multi[8] * 
  test_normalize(auto_data$origin, 2)
