#http://ruder.io/optimizing-gradient-descent/index.html#momentum
# Batch Gradient Descent using ADAM
logic_hypothesis = function(X, Y, theta) {
  #print("came here")
  return (1 / (1 + exp(-(X %*% theta))))
}

adam_gd = function(data_X,
                   data_Y,
                   dim,
                   epochs = 1e+05,
                   alpha = 0.001) {
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
  epsilon = 10e-08
  beta1 = 0.9
  beta2 = 0.999
  t = 1
  
  # J(theta) costs
  total_iter = epochs * ceiling((nrow(X) / mini_batch_size))
  cost_func_lst = rep(0, total_iter)
  
  old_theta = as.matrix(rep(0, dim))
  for (iter in seq(total_iter)) {
    old_theta = theta
    X = data_X
    Y = data_Y
    
    if (nextCycle) {
      sample_X = sample(nrow(X), replace = FALSE)
      X = X[sample_X,]
      Y = Y[sample_X,]
      start = 1
    }
    nextCycle = FALSE
    end = start + mini_batch_size - 1
    if (end > nrow(X)) {
      end = nrow(X)
      nextCycle = TRUE
    }
    
    X_batch = X[start:end,]
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
    hypoth = logic_hypothesis(X_batch, Y_batch, theta)
    #hypoth = ifelse(hypoth <= 0.5, 0, 1)
    gradient = t(X_batch) %*% (hypoth - Y_batch)
    cost_func_lst[iter] = (-t(Y_batch) %*% log(hypoth) - t(1-Y_batch) %*% log(1-hypoth))
    theta = theta - alpha * m_t_hat / (sqrt(v_t_hat) + epsilon)
    #######################
    
    #######processing at end
    start = end + 1
    t = t + 1
    
    if (all(abs(theta - old_theta) < 0.000001) && iter > 1)
      break
  }
  
  
  #cat('theta : ', theta, 'iter :', iter, '\n')
  return (list("theta" = theta, "cost" = cost_func_lst))
}

mean_normalize <- function(X) {
  apply(X, 2, function(col) {
    (col - mean(col)) / (max(col) - min(col))
  })
}

test_normalize <- function(col, val) {
  (val - mean(col)) / (max(col) - min(col))
}


# #Question 3.2

source("newAuto.R")
ret_data = generate_new_data()
X = ret_data$X
Y = ret_data$Y
auto_data = ret_data$auto_data
res = adam_gd(X, Y, dim(X)[2])
theta = res$theta

#Question 3.3
#error of the model

predict = function(X, theta){
  result = apply(X, 1, function(row){
    res_output = logic_hypothesis(t(row), null, theta)
    ifelse(res_output < 0.5, 0, 1)
  })
  result
}
predict_result = predict(X, theta)
error = sum((predict_result != Y) / nrow(auto_data))

#Question 3.4
test_x = as.matrix(c(1, test_normalize(auto_data$cylinders, 8), test_normalize(auto_data$displacement, 340),
  test_normalize(auto_data$horsepower, 200), test_normalize(auto_data$weight, 3500)))

res_output = logic_hypothesis(t(test_x), null, theta)

print(ifelse(res_output < 0.5, 0, 1))

