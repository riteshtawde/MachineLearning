#http://dataaspirant.com/2017/01/02/k-nearest-neighbor-classifier-implementation-r-scratch/

# euclidean distance
euc_dist = function(train, test) {
  return (sqrt(sum((train - test) ^ 2)))
}

# manhatten distance
l1_norm = function(train, test){
  return (abs(train-test))
}

# hamming distance
ham_dist = function(train, test){
  return (sum(train != test))
}

calc_error_rate = function(true_labels, predicted_labels) {
  return (sum(true_labels != predicted_labels) / length(true_labels))
}

# flatten output column
ravel_list = function(true_labels) {
  res = sapply(1:length(true_labels), function(i) {
    true_labels[[i]]
  })
  return (res)
}

knn_algo = function(dt_set,
                    index,
                    train,
                    test,
                    train_labels,
                    test_labels,
                    distance_func) {
  error_vector = rep(0, 5)
  near_neigh = as.integer(sqrt(dim(train)[1]))
  for (k in (near_neigh - 2):(near_neigh + 2)) {
    dist_list = list()
    predictions = c(0, nrow(test))
    for (i in 1:nrow(test)) {
      dist_vec = rep(0, nrow(train))
      label_vec = rep(0, nrow(train))
      for (j in 1:nrow(train)) {
        dist_vec[j] = distance_func(train[j, ], test[i, ])
        label_vec[j] = train_labels[[j]]
      }
      # create a dataframe for distance and associated label
      df_dist = data.frame(dist_vec, label_vec)
      df_dist = df_dist[order(df_dist$dist_vec), ]
      df_dist = df_dist[1:k, ]
      
      freq = table(df_dist[, 2])
      max_freq_cls = names(freq)[freq == max(freq)]
      predictions[i] = max_freq_cls
    }
    error_vector[k - near_neigh + 3] = calc_error_rate(test_labels, predictions)
  }
  return (list("error_vector" = error_vector, "near_neigh" = near_neigh))
}

plot_graphs = function(title, filename, xvalues, yvalues) {
  title_var = paste(title)
  png(filename = paste("plots/knn/knn_reg/", filename, ".png", sep = ""))
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


cat("############################ KNN USing Max Neighbours ############################\n")
start_time = Sys.time()

# ionosphere
iono_error_rate = matrix(0, nrow = 5, ncol = 5)
iono_l1_error_rate = matrix(0, nrow = 5, ncol = 5)
k = 0
k_l1 = 0
for (j in 1:5) {
  train_data = read.csv(paste("cv_data_onehot/iono_", j, "_training.csv", sep = ""))
  test_data =  read.csv(paste("cv_data_onehot/iono_", j, "_testing.csv", sep = ""))
  train_labels = lapply(train_data[, 34], as.character)
  test_labels = lapply(test_data[, 34], as.character)
  test_labels = ravel_list(test_labels)
  train_data = as.matrix(train_data[,-c(34)])
  test_data = as.matrix(test_data[,-c(34)])
  # euclidean
  res = knn_algo("iono", j, train_data, test_data, train_labels, test_labels, euc_dist)
  iono_error_rate[j, ] = res$error_vector
  k = res$near_neigh
  # l1
  res = knn_algo("iono", j, train_data, test_data, train_labels, test_labels, l1_norm)
  iono_l1_error_rate[j, ] = res$error_vector
  k_l1 = res$near_neigh
}
avg_iono_error_rate = colMeans(iono_error_rate)
xvalues = c((k - 2):(k + 2))

# plot
plot_graphs(
  "Error rate for knn on ionosphere (Euclidean)",
  "iono_euclid",
  xvalues,
  avg_iono_error_rate
)

cat("Average error rate for KNN using euclidean distance over ionosphere data : ",avg_iono_error_rate,"\n")

avg_iono_error_rate = colMeans(iono_l1_error_rate)
xvalues = c((k_l1 - 2):(k_l1 + 2))

# plot
plot_graphs(
  "Error rate for knn on ionosphere (L1 norm)",
  "iono_l1",
  xvalues,
  avg_iono_error_rate
)

cat("Average error rate for KNN using manhattan distance over ionosphere data : ",avg_iono_error_rate,"\n")


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
  res = knn_algo("car", j, train_data, test_data, train_labels, test_labels, euc_dist)
  car_error_rate[j, ] = res$error_vector
  k = res$near_neigh
}

avg_car_error_rate = colMeans(car_error_rate)
xvalues = c((k - 2):(k + 2))

cat("Average error rate for KNN using euclidean distance over car evaluation data : ",avg_car_error_rate,"\n")

# plot
plot_graphs(
  "Error rate for knn on car (Euclidean)",
  "car_euclid",
  xvalues,
  avg_car_error_rate
)

car_error_rate = matrix(nrow = 5, ncol = 5)
k = 0
for (j in 1:5) {
  train_data = read.csv(paste("cv_data/car_", j, "_training.csv", sep = ""))
  test_data =  read.csv(paste("cv_data/car_", j, "_testing.csv", sep = ""))
  class_col = dim(train_data)[2]
  train_labels = lapply(train_data[, class_col], as.character)
  test_labels = lapply(test_data[, class_col], as.character)
  test_labels = ravel_list(test_labels)
  train_data = as.matrix(train_data[, -c(class_col)])
  test_data = as.matrix(test_data[, -c(class_col)])
  res = knn_algo("car", j, train_data, test_data, train_labels, test_labels, ham_dist)
  car_error_rate[j, ] = res$error_vector
  k = res$near_neigh
}

avg_car_error_rate = colMeans(car_error_rate)
xvalues = c((k - 2):(k + 2))

# plot
plot_graphs(
  "Error rate for knn on car (Hamming)",
  "car_hamming",
  xvalues,
  avg_car_error_rate
)

cat("Average error rate for KNN using hamming distance over car evaluation data : ",avg_car_error_rate,"\n")

# credit_approval
k = 0
k_l1 = 0
credit_error_rate = matrix(nrow = 5, ncol = 5)
credit_l1_error_rate = matrix(nrow = 5, ncol = 5)
for (j in 1:5) {
  train_data = read.csv(paste("cv_data_onehot/credit_", j, "_training.csv", sep = ""))
  test_data =  read.csv(paste("cv_data_onehot/credit_", j, "_testing.csv", sep = ""))
  class_col = dim(train_data)[2]
  train_labels = lapply(train_data[, class_col], as.character)
  test_labels = lapply(test_data[, class_col], as.character)
  test_labels = ravel_list(test_labels)
  train_data = as.matrix(train_data[, -c(class_col)])
  test_data = as.matrix(test_data[, -c(class_col)])
  res = knn_algo("credit", j, train_data, test_data, train_labels, test_labels, euc_dist)
  credit_error_rate[j, ] = res$error_vector
  k = res$near_neigh
  res = knn_algo("credit", j, train_data, test_data, train_labels, test_labels, l1_norm)
  credit_l1_error_rate[j, ] = res$error_vector
  k_l1 = res$near_neigh
}

avg_credit_error_rate = colMeans(credit_error_rate)
xvalues = c((k - 2):(k + 2))

# plot
plot_graphs(
  "Error rate for knn on credit approval (Euclidean)",
  "credit_euclid",
  xvalues,
  avg_credit_error_rate
)

cat("Average error rate for KNN using euclidean distance over car evaluation data : ",avg_credit_error_rate,"\n")

avg_credit_error_rate = colMeans(credit_l1_error_rate)
xvalues = c((k_l1-2):(k_l1 + 2))

# plot
plot_graphs(
  "Error rate for knn on credit approval (L1 norm)",
  "credit_l1",
  xvalues,
  avg_credit_error_rate
)

cat("Average error rate for KNN using manhattan distance over car evaluation data : ",avg_credit_error_rate,"\n")

cat("############################ END ############################\n")
cat("Total time taken : ",(Sys.time() - start_time))