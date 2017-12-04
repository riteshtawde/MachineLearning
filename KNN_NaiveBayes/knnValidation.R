#install.packages("class")
library(class)

calc_error_rate = function(true_labels, predicted_labels) {
  return (sum(true_labels != predicted_labels) / length(true_labels))
}

ravel_list = function(true_labels) {
  res = sapply(1:length(true_labels), function(i) {
    true_labels[[i]]
  })
  return (res)
}

knn_pack = function(train, test, train_labels, test_labels) {
  error_vector = rep(0, 5)
  near_neigh = as.integer(sqrt(dim(train)[1]))
  trl = factor(unlist(train_labels))
  for (i in (near_neigh - 2):(near_neigh + 2)) {
    res = knn(train, test, trl, k = i)
    error_vector[i - near_neigh + 3] = calc_error_rate(test_labels, res)
  }
  return (list("error_vector" = error_vector, "near_neigh" = near_neigh))
}

plot_graphs_pack = function(title, filename, xvalues, yvalues) {
  title_var = paste(title)
  png(filename = paste("plots/knn/knn_package/", filename, ".png", sep = ""))
  plot(
    x = xvalues,
    y = yvalues,
    xlab = "Nearest neighbours",
    ylab = "Error rate",
    pch = 20,
    col = "red",
    type = "o"
  )
  title(title_var)
  dev.off()
}



# ionosphere
iono_error_rate = matrix(0, nrow = 5, ncol = 5)
k = 0
for (j in 1:5) {
  train_data = read.csv(paste("cv_data_onehot/iono_", j, "_training.csv", sep = ""))
  test_data =  read.csv(paste("cv_data_onehot/iono_", j, "_testing.csv", sep = ""))
  train_labels = lapply(train_data[, 34], as.character)
  test_labels = lapply(test_data[, 34], as.character)
  test_labels = ravel_list(test_labels)
  train_data = as.matrix(train_data[,-c(34)])
  test_data = as.matrix(test_data[,-c(34)])
  res = knn_pack(train_data, test_data, train_labels, test_labels)
  iono_error_rate[j, ] = res$error_vector
  k = res$near_neigh
}
avg_iono_error_rate = colMeans(iono_error_rate)
xvalues = c((k - 2):(k + 2))

# plot
plot_graphs_pack(
  "Error rate for knn package on ionosphere",
  "iono",
  xvalues,
  avg_iono_error_rate
)

# car
car_error_rate = matrix(nrow = 5, ncol = 5)
k = 0
for (j in 1:5) {
  train_data = read.csv(paste("cv_data_onehot/car_", j, "_training.csv", sep = ""))
  test_data =  read.csv(paste("cv_data_onehot/car_", j, "_testing.csv", sep = ""))
  class_col = dim(train_data)[2]
  train_labels = lapply(train_data[, class_col], as.character)
  test_labels = lapply(test_data[, class_col], as.character)
  test_labels = ravel_list(test_labels)
  train_data = as.matrix(train_data[, -c(class_col)])
  test_data = as.matrix(test_data[, -c(class_col)])
  res = knn_pack(train_data, test_data, train_labels, test_labels)
  car_error_rate[j, ] = res$error_vector
  k = res$near_neigh
}

avg_car_error_rate = colMeans(car_error_rate)
xvalues = c((k - 2):(k + 2))

# plot
plot_graphs_pack(
  "Error rate for knn package on car",
  "car",
  xvalues,
  avg_car_error_rate
)

# credit_approval
k = 0
credit_error_rate = matrix(nrow = 5, ncol = 5)
for (j in 1:5) {
  train_data = read.csv(paste("cv_data_onehot/credit_", j, "_training.csv", sep = ""))
  test_data =  read.csv(paste("cv_data_onehot/credit_", j, "_testing.csv", sep = ""))
  class_col = dim(train_data)[2]
  train_labels = lapply(train_data[, class_col], as.character)
  test_labels = lapply(test_data[, class_col], as.character)
  test_labels = ravel_list(test_labels)
  train_data = as.matrix(train_data[, -c(class_col)])
  test_data = as.matrix(test_data[, -c(class_col)])
  res = knn_pack(train_data, test_data, train_labels, test_labels)
  credit_error_rate[j, ] = res$error_vector
  k = res$near_neigh
}

avg_credit_error_rate = colMeans(credit_error_rate)
xvalues = c((k - 2):(k + 2))

# plot
plot_graphs_pack(
  "Error rate for knn package on credit approval",
  "credit",
  xvalues,
  avg_credit_error_rate
)