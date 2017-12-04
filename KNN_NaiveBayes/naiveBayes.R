# install.packages("dplyr")
library(dplyr)
handle_cat_data = function(dataset) {
  class_idx = ncol(dataset)
  class_distr = sapply(dataset[-class_idx], table, dataset[, class_idx])
  # transpose
  class_distr = lapply(class_distr, t)
  
  # conditional probabilities
  cond_prob = sapply(class_distr, function(X) {
    apply(X, 1, function(X) {
      # laplace smoothing
      (X + 1) / (sum(X) + 1)
    })
  })
  
  uniqv = unique(dataset[, class_idx], na.rm = TRUE)
  priors =  c()
  for (class in uniqv) {
    priors <-
      rbind(priors, c(class, sum(dataset[, class_idx] == class) / nrow(dataset)))
    colnames(priors) <- c("class", "prior")
  }
  
  cond_prob = lapply(cond_prob, t)
  return(list("cond" = cond_prob, "priors" = priors))
}

handle_cont_data = function(dataset) {
  class_idx = ncol(dataset)
  summary_mean = aggregate(dataset[, 1:(class_idx - 1)], list(dataset[, class_idx]), mean, na.rm = TRUE)
  summary_sd = aggregate(dataset[, 1:(class_idx - 1)], list(dataset[, class_idx]), sd, na.rm = TRUE)
  m = dim(dataset)[1]
  uniqv = unique(dataset[, class_idx], na.rm = TRUE)
  priors =  c()
  for (class in uniqv) {
    priors <-
      rbind(priors, c(class, sum(dataset[, class_idx] == class) / nrow(dataset)))
      colnames(priors) <- c("class", "prior")
  }
  return (list(
    "mean" = summary_mean,
    "sd" = summary_sd,
    "priors" = priors
  ))
}

process_cat_cont = function(dataset) {
  new_df = data.frame(nrow = nrow(dataset), ncol = ncol(dataset))
  numeric_vec = c()
  factor_vec = c()
  for (i in 1:(ncol(dataset) - 1)) {
    if (class(dataset[, i]) %in% c("numeric", "integer")) {
      numeric_vec = c(numeric_vec, i)
    } else if (class(dataset[, i]) %in% c("character", "factor")) {
      factor_vec = c(factor_vec, i)
    }
  }
  
  numeric_df = data.frame()
  # appending class column
  if (length(numeric_vec) > 0) {
    numeric_df = data.frame(dataset[, numeric_vec])
    numeric_df$cls = dataset[, ncol(dataset)]
  }
  
  factor_df = data.frame()
  if (length(factor_vec) > 0) {
    factor_df = data.frame(dataset[, factor_vec])
    factor_df$cls = dataset[, ncol(dataset)]
  }
  
  return(list("numer" = numeric_df, "fact" = factor_df))
}

gaussian_distr <- function(row, row_mean, row_sd) {
  return(dnorm(row, mean = row_mean, sd = row_sd))
}

calculate_error_rate = function(true_labels, predicted_labels) {
  true_labels = sapply(true_labels, as.character)
  return (sum(true_labels != predicted_labels) / length(true_labels))
}

naive_bayes = function(train_data, test_data) {
  num_fac_df = process_cat_cont(train_data)
  numeric_df = num_fac_df$numer
  factor_df = num_fac_df$fact
  numeric_tables = 0
  factor_tables = 0
  priors = 0
  if (nrow(numeric_df) != 0) {
    numeric_tables = handle_cont_data(numeric_df)
    priors = numeric_tables$priors
  }
  if (nrow(factor_df) != 0) {
    factor_tables = handle_cat_data(factor_df)
    priors = factor_tables$priors
  }
  
  uniqv = unique(train_data[, ncol(train_data)], na.rm = TRUE)
  
  test_labels = test_data[, ncol(test_data)]
  test_data = test_data[, -c(ncol(test_data))]
  
  predicted_labels = rep(0, nrow(test_data))
  
  # actual testing
  col_names = colnames(test_data)
  for (i in 1:nrow(test_data)) {
    zero_vec = rep(0, length(uniqv))
    hashmap = new.env(hash = TRUE)
    list2env(setNames(as.list(zero_vec), uniqv), envir = hashmap)
    for (j in 1:ncol(test_data)) {
      if (class(test_data[, j]) %in% c("character", "factor")) {
        for (k in 1:length(uniqv)) {
          val = factor_tables$cond[[col_names[j]]][, test_data[i, j]][as.character(uniqv[k])]
          hashmap[[as.character(uniqv[k])]] = hashmap[[as.character(uniqv[k])]] + log(val)
        }
      } else if (class(test_data[, j]) %in% c("numeric", "integer")) {
        for (k in 1:length(uniqv)) {
          mean_val = numeric_tables$mean[numeric_tables$mean["Group.1"] == as.character(uniqv[k]), col_names[j]]
          sd_val = numeric_tables$sd[numeric_tables$sd["Group.1"] == as.character(uniqv[k]), col_names[j]]
          hashmap[[as.character(uniqv[k])]] = hashmap[[as.character(uniqv[k])]] + log(gaussian_distr(test_data[i, j], mean_val, sd_val))
        }
      }
    }
    max = -999999999999
    for (k in 1:length(uniqv)) {
      val = hashmap[[as.character(uniqv[k])]]
      if (max < val) {
        max = val
        predicted_labels[i] = as.character(uniqv[k])
      }
    }
  }
  error_rate = calculate_error_rate(test_labels, predicted_labels)
  return (error_rate)
}

plot_graphs = function(title, filename, yvalues) {
  title_var = paste(title)
  png(filename = paste("plots/naive_bayes/impl/", filename, ".png", sep = ""))
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

cat("############################ Naive bayes ############################\n")
start_time = Sys.time()

# ionosphere
iono_error_rate = rep(0, 5)
for (j in 1:5) {
  train_data = read.csv(paste("cv_data/iono_", j, "_training.csv", sep = ""),
                        header = FALSE)
  test_data =  read.csv(paste("cv_data/iono_", j, "_testing.csv", sep = ""), header = FALSE)
  iono_error_rate[j] = naive_bayes(train_data, test_data)
}
# plot
plot_graphs("Error rate for Naive bayes on Ionosphere",
            "naive_bayes_iono",
            iono_error_rate)

cat("Average error rate for naive bayes over ionosphere : ", mean(iono_error_rate))

# car
car_error_rate = rep(0, 5)
for (j in 1:5) {
  train_data = read.csv(paste("cv_data/car_", j, "_training.csv", sep = ""),
                        header = FALSE)
  test_data =  read.csv(paste("cv_data/car_", j, "_testing.csv", sep = ""), header = FALSE)
  car_error_rate[j] = naive_bayes(train_data, test_data)
}
# plot
plot_graphs("Error rate for Naive bayes on car",
            "naive_bayes_car",
            car_error_rate)

cat("Average error rate for naive bayes over car evaluation : ", mean(car_error_rate))

# credit
credit_error_rate = rep(0, 5)
for (j in 1:5) {
  train_data = read.csv(paste("cv_data/credit_", j, "_training.csv", sep = ""),
                        header = FALSE)
  test_data =  read.csv(paste("cv_data/credit_", j, "_testing.csv", sep = ""), header = FALSE)
  credit_error_rate[j] = naive_bayes(train_data, test_data)
}
# plot
plot_graphs(
  "Error rate for Naive bayes on Credit approval",
  "naive_bayes_credit",
  credit_error_rate
)

cat("Average error rate for naive bayes over credit evaluation : ", mean(credit_error_rate))


cat("############################ END ############################\n")
cat("Total time taken : ",(Sys.time() - start_time))