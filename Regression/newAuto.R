sigmoid_func = function(){
  count_Values = 100
  set.seed(1234)
  values = seq(-10,10,0.001)
  res = rep(0, length(values))
  
  for(i in 1:length(values)){
    res[i] = 1/(1+exp(-values[i]))
  }
  plot(values, res, xlab = "Input", ylab = "Probability", main = "Sigmoid function")
}

#Question 3.1
sigmoid_func()

generate_new_data = function(){
  #Question 3.2
  library("ISLR")
  auto_data = Auto
  
  # preprocessing
  mpg_median = median(auto_data$mpg)
  X = matrix(0, nrow = nrow(auto_data), ncol = 5)
  X[, 1] = rep(1, nrow(X))
  X[, 2:5] = as.matrix(mean_normalize(auto_data[, 2:5]))
  Y = as.matrix(ifelse(auto_data$mpg >= mpg_median, 1, 0))
  return (list("X"=X, "Y"=Y, "auto_data"=auto_data))
}