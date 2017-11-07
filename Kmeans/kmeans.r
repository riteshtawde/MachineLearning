clustering = function(data, K, mu, labels) {
  result <- apply(data, 1, function(row) {
    new_dist <- sapply(1:K, function(k) {
      sum((row - mu[k,]) ^ 2)
    })
    sqrt(new_dist)
  })
}

min_dist_cluster = function(N, K, dist) {
  cluster <- sapply(1:N, function(n) {
    which.min(dist[, n])
  })
}

est_new_center = function(cluster, data, K, centers) {
  result <- sapply(1:K, function(k) {
    if (sum(cluster == k) > 1) {
      colMeans(data[cluster == k,])
    } else {
      #print(centers[k])
      centers[k, ]
    }
  })
  t(result)
}

new_dist = function(cluster, mu, new_mu, K) {
  result <- sapply(1:K, function(k) {
    if (sum(cluster == k) > 1) {
      sqrt(sum((mu[k,] - new_mu[k,]) ^ 2))
    } else{
      0
    }
  })
  sum(result)
}

lloyds_k_means = function(data, K, labels, isIono) {
  d = dim(data)
  n = d[1]
  p = d[2]
  
  # setting class label to compare
  cls_lbl = ifelse(isIono == TRUE, 'g', '1')
  
  #iterations for convergence
  count = 0
  
  #initialize centroids
  centers = matrix(rnorm(K * p), K, p)# center for kth cluster
  dist = matrix(0, K, n)            #distance from ith point to kth cluster center
  cluster = rep(0, n)              #cluster for point data[i,]
  
  sse = 1000
  threshold = 0.001
  
  while (sse >= threshold) {
    result = clustering(data, K, centers, labels)
    cluster = min_dist_cluster(n, K, result)
    new_centers = est_new_center(cluster, data, K, centers)
    sse = new_dist(cluster, centers, new_centers, K)
    centers = new_centers
    sse = sse / K
    count = count+1
    #print(sse)
  }
  
  #######################################
  #error calculation
  # clustering error calculation
  error = 0
  if (K <= 2) {
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
    
    good_center = colMeans(data[labels == cls_lbl, ])
    bad_center = colMeans(data[labels != cls_lbl, ])
    
    for (k in 1:K) {
      good_count = 0
      bad_count = 0
      good_dist = sqrt(sum((good_center - centers[k, ]) ^ 2))
      bad_dist = sqrt(sum((bad_center - centers[k, ]) ^ 2))
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
  
  
  
  ########################################
  
  return (list("error" = error, "iter" = count))
}

##Ionosphere starts
data = read.csv("ionosphere_data.csv", header = FALSE)
threshold = 0.001
labels = data[, 35]
data = as.matrix(data[,-c(2,35)])

error_vector = rep(rep(0, 20), 4)
iter_vector = rep(rep(0, 20), 4)
start_time = Sys.time()
print('Running...')
i = 0
for (c in 1:4) {
  cat('k : ', (c + 1), '\n')
  for (iter in 1:20) {
    cat('iter : ', iter, '\n')
    result = lloyds_k_means(data, c + 1, labels, TRUE)
    error_vector[i + iter] = result$error
    iter_vector[i + iter] = result$iter
  }
  i = i + 20
}
end_time = Sys.time()
print(end_time - start_time)
print('Completed!!!')

write.table(error_vector, col.names = FALSE, file = "iono_kmeans_error.csv", row.names = FALSE)
write.table(iter_vector, col.names = FALSE, file = "iono_kmeans_iter.csv", row.names = FALSE)
##Ionosphere endds


data = read.csv("ringnorm_data.csv", header = FALSE)
threshold = 0.001
labels = data[, 1]
data = as.matrix(data[,-c(1)])

error_vector = rep(rep(0, 20), 4)
start_time = Sys.time()
print('Running...')
i = 0
for (c in 1:4) {
  cat('k : ', (c + 1), '\n')
  for (iter in 1:20) {
    cat('iter : ', iter, '\n')
    error_vector[i + iter] = lloyds_k_means(data, c + 1, labels, FALSE)
  }
  i = i + 20
}
end_time = Sys.time()
print(end_time - start_time)
print('Completed!!!')

write.table(error_vector, col.names = FALSE, file = "ring_kmeans_error.csv", row.names = FALSE)
write.table(iter_vector, col.names = FALSE, file = "ring_kmeans_iter.csv", row.names = FALSE)

