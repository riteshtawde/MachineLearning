#install.packages("mvtnorm")   # package for multivariate normal(Gaussian) distribution
library(mvtnorm)

expect = function(data, prior, mu, sigma, K, cluster) {
  result <- apply(data, 1, function(row) {
    new_prior <- sapply(1:K, function(k) {
      prior[k] * dmvnorm(row, mu[, k], sigma[, , k])
    })
    new_prior / sum(new_prior)
  })
  #print(t(result))
  t(result)
}

maximize = function(prior, data, K, N, dims) {
  prior_sum <- apply(prior, 2, sum)
  new_prior <- prior_sum / N

  new_mu <- t(t(t(data) %*% prior) / prior_sum)
  #cat('here : ',new_mu,'\n')
  
  new_sigma <- array(0, dim = c(dims, dims, K))
  
  for (k in 1:K) {
    sig <- matrix(0, dims, dims)
    
    for (n in 1:N) {
      sig <- sig + prior[n, k] * (data[n,] %*% t(data[n,]))
      
    }
    new_sigma[, , k] <-
      sig / prior_sum[k] - new_mu[, k] %*% t(new_mu[, k])
  }
  list(new_prior, new_mu, new_sigma)
  
}

get_cluster = function(prior){
  cluster <- apply(prior, 1, function(row){
    which.max(row)
  })
}

em = function(data, K, labels, isIono) {
  N = dim(data)[1]
  dims = dim(data)[2]
  
  cls_lbl = ifelse(isIono == TRUE, 'g', '1')
  
  count = 0
  cluster = rep(0, N)
  prior = rep(1 / K, K)
  sigma <- array(0, dim = c(dims, dims, K))
  
  for (k in 1:K) {
    sigma[, , k] = 1 * diag(dims)
  }
  
  sample_dt = sample(N, K, replace = FALSE)
  mu = matrix(t(data[sample_dt, ]), nrow = dims, ncol = K)
  
  errorlist =  rep(0, 50)
  error = 99999
  while (error > 1.8) {
    count = count + 1
    prior = expect(data, prior, mu, sigma, K, cluster)
    cluster = get_cluster(prior)
    result = maximize(prior, data, k, N, dims)
    new_prior = result[[1]]
    new_mu = result[[2]]
    #print(new_mu)
    new_sigma = result[[3]]
    error = sqrt(sum((new_mu - mu) ^ 2))
    #cat('new mu : ',new_mu,'\n old mu : ',mu,'\n')
    prior = new_prior
    mu = new_mu
    sigma = new_sigma
    #cat('count : ',count,', distance : ',error,'\n')
  }
  
  # clustering error calculation
  error = 0
  if (K <= 2) {
    for (k in 1:K) {
      good_count = 0
      bad_count = 0
      for (l in 1:N) {
        if (cluster[l] == k) {
          if (labels[l] == cls_lbl)
            good_count = good_count + 1
          else
            bad_count  = bad_count + 1
        }
      }
      if (sum(cluster == k) > 0) {
        error = error + if (good_count >= bad_count)
          (bad_count / (bad_count + good_count))
        else
          (good_count / (good_count + bad_count))
      }
    }
    print(error)
  } else{
    close_good = 0
    close_bad = 0
    
    good_center = colMeans(data[labels == cls_lbl,])
    bad_center = colMeans(data[labels != cls_lbl,])
    
    for (k in 1:K) {
      good_count = 0
      bad_count = 0
      good_dist = sqrt(sum((good_center - mu[,k]) ^ 2))
      bad_dist = sqrt(sum((bad_center - mu[,k]) ^ 2))
      for (l in 1:N) {
        if (cluster[l] == k) {
          if (labels[l] == cls_lbl)
            good_count = good_count + 1
          else
            bad_count  = bad_count + 1
        }
      }
      
      if (sum(cluster == k) > 0) {
        error = error + if (good_dist <= bad_dist)
          (bad_count / (bad_count + good_count))
        else
          (good_count / (good_count + bad_count))
      }
    }
    print(error)
  }
  print(count)
  return (list("error"=error,"iter"=count))
}


# bound on max iterations
max_iter = 50

#ionosphere
data = read.csv("ionosphere_data.csv", header = FALSE)
labels = data[, 35]
data = as.matrix(scale(data[,-c(2, 35)]))

error_vector = rep(rep(0, 20), 4)
iter_vector = rep(rep(0,20),4)
it = 0
print('Ionosphere...')
for (K in 2:5) {
  cat('cluster # : ',K,'\n')
  for (i in 1:20) {
    result = em(data, K, labels, TRUE)
    error_vector[it+i] = result$error
    iter_vector[it+i] = result$iter
  }
  it = it+20
}