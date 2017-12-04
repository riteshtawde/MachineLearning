# https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation
# install.packages("onehot")
library(onehot)
one_hot_encoding = function(t_data) {
  res = onehot(t_data, addNA = FALSE, max_levels = 15)
  return (predict(res, t_data))
}

create_dataset = function(dataset, filename, kfolds, foldername) {
  dataset = dataset[sample(nrow(dataset)),]
  folds = cut(seq(1, nrow(dataset)), breaks = kfolds, labels = FALSE)
  for (i in 1:kfolds) {
    split = which(folds == i, arr.ind = TRUE)
    train = dataset[-split,]
    test = dataset[split,]
    
    write.table(
      train,
      file = paste(foldername, "/", filename, "_", i, "_training.csv", sep = ""),
      sep = ",",
      quote = F,
      row.names = F,
      col.names = F
    )
    write.table(
      test,
      file = paste(foldername, "/", filename, "_", i, "_testing.csv", sep = ""),
      sep = ",",
      quote = F,
      row.names = F,
      col.names = F
    )
  }
}

# https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
getmode = function(data) {
  uniqv <- unique(data)
  return (uniqv[which.max(tabulate(match(data, uniqv)))])
}

impute_data = function(X) {
  for (i in 1:ncol(X)) {
    if(i == 14){
      print("14")
      print("came here")
    }
    if (class(X[, i]) %in% c("numeric","integer")) {
      X[is.na(X[, i]), i] <- mean(X[, i], na.rm = TRUE)
    } else if (class(X[, i]) %in% c("character", "factor")) {
      X[is.na(X[, i]), i] <- getmode(X[, i])
    }
  }
  return(X)
}

kfolds = 5

# one hot encoded

#ionosphere
iono_data = read.csv("ionosphere.csv", header = FALSE)
# scaled_iono = scale(iono_data[,-c(2,35)])
# class_lbl = iono_data[,35]
# cbind(as.matrix(scaled_iono), as.matrix(class_lbl))
create_dataset(iono_data[,-c(2)], "iono", kfolds, "cv_data_onehot")

#car
car_data = read.csv("car_data.csv", header = FALSE)
tmp_class = car_data[, c(7)]
tmp_rest = car_data[,-c(7)]
tmp_rest = one_hot_encoding(tmp_rest)
car_data = data.frame(tmp_rest, tmp_class)
create_dataset(car_data, "car", kfolds, "cv_data_onehot")

#credit approval
credit_data = read.csv("credit_approval.csv", header = FALSE, na.strings = c(NA,"?"))
# first handle missing data
tmp_class = credit_data[, c(16)]
# credit_data[credit_data == "?"] = NA
credit_data = impute_data(credit_data[,-c(16)])
tmp_cat_data = credit_data[,-c(2, 3, 8, 11, 14, 15)]
tmp_cat_data = one_hot_encoding(tmp_cat_data)
credit_data = data.frame(credit_data[, c(2, 3, 8, 11, 14, 15)], tmp_cat_data, tmp_class)
create_dataset(credit_data, "credit", kfolds, "cv_data_onehot")


# without one hot encoding

iono_data = read.csv("ionosphere.csv", header = FALSE)
# scaled_iono = scale(iono_data[,-c(2,35)])
# class_lbl = iono_data[,35]
# cbind(as.matrix(scaled_iono), as.matrix(class_lbl))
create_dataset(iono_data[,-c(2)],  "iono", kfolds, "cv_data")

#car
car_data = read.csv("car_data.csv", header = FALSE)
tmp_class = car_data[, c(7)]
tmp_rest = car_data[,-c(7)]
car_data = data.frame(tmp_rest, tmp_class)
create_dataset(car_data, "car", kfolds, "cv_data")

#credit approval
credit_data = read.csv("credit_approval.csv", header = FALSE, na.strings = c(NA,"?"))
# first handle missing data
tmp_class = credit_data[, c(16)]
# credit_data[credit_data == "?"] = NA
credit_data = impute_data(credit_data[,-c(16)])
credit_data = data.frame(credit_data, tmp_class)
create_dataset(credit_data, "credit", kfolds, "cv_data")