# install.packages(e1071)
library(e1071)

calculate_error_rate = function(true_labels, predicted_labels) {
  true_labels = sapply(true_labels, as.character)
  return (sum(true_labels != predicted_labels) / length(true_labels))
}

naive_bayes_pack = function(train, test){
  model = naiveBayes(train[,ncol(train)]~., data = train, laplace = 3)
  res = predict(model, test)
  error_rate = calculate_error_rate(test[,ncol(test)], res)
  return (error_rate)
}

plot_graphs_pack_n = function(title, filename, yvalues) {
  title_var = paste(title)
  png(filename = paste("plots/naive_bayes/naive_package/", filename, ".png", sep = ""))
  plot(
    x = c(1:5),
    y = yvalues,
    xlab = "K folds",
    ylab = "Error rate",
    pch = 20,
    col = "red",
    type = "o"
  )
  title(title_var)
  dev.off()
}


# ionosphere
iono_error_rate = rep(0, 5)
for (j in 1:5) {
  train_data = read.csv(paste("cv_data/iono_", j, "_training.csv", sep = ""),
                        header = FALSE)
  test_data =  read.csv(paste("cv_data/iono_", j, "_testing.csv", sep = ""), header = FALSE)
  iono_error_rate[j] = naive_bayes_pack(train_data, test_data)
}
# plot
plot_graphs_pack_n("Error rate for Naive bayes package on Ionosphere",
            "naive_bayes_iono",
            iono_error_rate)

# car
car_error_rate = rep(0, 5)
for (j in 1:5) {
  train_data = read.csv(paste("cv_data/car_", j, "_training.csv", sep = ""),
                        header = FALSE)
  test_data =  read.csv(paste("cv_data/car_", j, "_testing.csv", sep = ""), header = FALSE)
  car_error_rate[j] = naive_bayes_pack(train_data, test_data)
}
# plot
plot_graphs_pack_n("Error rate for Naive bayes package on car",
            "naive_bayes_car",
            car_error_rate)

# credit
credit_error_rate = rep(0, 5)
for (j in 1:5) {
  train_data = read.csv(paste("cv_data/credit_", j, "_training.csv", sep = ""),
                        header = FALSE)
  test_data =  read.csv(paste("cv_data/credit_", j, "_testing.csv", sep = ""), header = FALSE)
  credit_error_rate[j] = naive_bayes_pack(train_data, test_data)
}
# plot
plot_graphs_pack_n(
  "Error rate for Naive bayes package on Credit approval",
  "naive_bayes_credit",
  credit_error_rate
)