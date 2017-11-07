#install.packages("mvtnorm")   # package for multivariate normal(Gaussian) distribution
library(mvtnorm)

# bound on max iterations
max_iter = 50

#ionosphere
iono_data = read.csv("ionosphere_data.csv", header = FALSE)
iono_labels = iono_data[, 35]
iono_data = as.matrix(scale(iono_data[,-c(2,35)]))

#ringnorm
ring_data = read.csv("ringnorm_data.csv", header = FALSE)
ring_labels = ring_data[, 1]
ring_data = scale(ring_data[,-c(1)])
ring_data = matrix(ring_data, nrow = 7400, ncol = 20)
#ring_data = as.matrix(ring_data)


# Function for expectation maximization
em = function(data, K, labels, isIono) {
  n = dim(data)[1]
  d = dim(data)[2]
  
  # class label
  cls_lbl = if (isIono == TRUE)
    'g'
  else
    1
  
  
  # Randomly initializing mu
  sample_dt = sample(n, K, replace = FALSE)
  mu = data[sample_dt, ]
  # initializing covariances to identity
  sigma = array(0, c(K, d, d))
  for (k in 1:K) {
    sigma[k, , ] = diag(d)
  }
  # intitializing priors with equal probabilities
  prior = rep(1 / K, K)
  # cluster no for each data point
  cluster = rep(0, n)
  # probability of ith data point from the kth cluster
  prob = matrix(0, n, K)
  #error initialization
  distance = 9999999
  epsilon = 0.001
  #sum
  total_sum = rep(0, K)
  loop = 1
  iter = 0
  while (distance >= epsilon) {
    iter = iter + 1
    oldmu = mu
    # Expectation step
    for (i in 1:n) {
      for (k in 1:K) {
        # posterior probability
        # if(det(sigma[k,,]) == 0){
        #   diag(sigma[k,,]) = diag(sigma[k,,])+1e-05
        # }
        prob[i, k] = log(prior[k]) + dmvnorm(data[i, ], mu[k, ], sigma[k, , ], log = TRUE)
      }
      prob[i, ] = exp(prob[i, ] - max(prob[i, ]))
      prob[i, ] = prob[i, ] / sum(prob[i, ])  # denominator
      # assigning cluster
      cluster[i] = which.max(prob[i, ])
    }
    print("E")
    # Maximization step
    for (k in 1:K) {
      # reestimating priors
      if (sum(cluster == k) == 0 || sum(cluster == k) == 1)
        next
      total_sum[k] = sum(prob[, k])
      prior[k] = total_sum[k] / n
      
      # reestimating mu's
      mu[k, ] = 0
      sigma[k, , ] = 0
      for (i in 1:n) {
        mu[k, ] = mu[k, ] + (prob[i, k] / total_sum[k]) * data[i, ]
        
      }
      # reestimating covariances
      for (i in 1:n) {
        sigma[k, , ] = sigma[k, , ] + (prob[i, k] / total_sum[k]) * (data[i, ] - mu[k, ]) %*% t(data[i, ] - mu[k, ])
      }
    }
    print("M")
    distance = sqrt(sum((oldmu[1:K, ] - mu[1:K, ]) ^ 2)) / K
    print(distance)
  }
  
  
  error = 0
  if(K <= 2){
    for (k in 1:K) {
      good_count = 0
      bad_count = 0
      for (l in 1:n) {
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
    
    good_center = colMeans(data[labels==(if(isIono == TRUE) 'g' else 1),])
    bad_center = colMeans(data[labels==(if(isIono == TRUE) 'b' else -1),])
    
    for(k in 1:K){
      good_count = 0
      bad_count = 0
      good_dist = sqrt(sum((good_center - mu[k,])^2))
      bad_dist = sqrt(sum((bad_center - mu[k,])^2))
      for (l in 1:n) {
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
  print(iter)
  
  return (list("error" = error, "iter" = iter))
}

#ionosphere
error_vector = rep(rep(0, 20), 4)
start_time = Sys.time()
print('Running...')
i = 0
for (c in 2:4) {
  cat('k : ',(c+1),'\n')
  for (iter in 1:20) {
     error_vector[i+iter] = em(iono_data,(c+1), iono_labels, TRUE)
  }
  i = i+20
}
end_time = Sys.time()
print(end_time - start_time)
print('Completed!!!')

#ringnorm
error_vector = rep(rep(0, 20), 4)

print('Running...')
i = 0
for (c in 4:4) {
  cat('k : ', (c + 1), '\n')
  for (iter in 1:20) {
    start_time = Sys.time()
    error_vector[i + iter] = em(ring_data, (c + 1), ring_labels, FALSE)
    end_time = Sys.time()
    print(end_time - start_time)
  }
  i = i + 20
}

print('Completed!!!')