cancer = read.csv("breast_cancer_wisconsin.csv", header = FALSE)
cancer = cancer[!cancer$V7 == '?', ]
#cancer = na.omit(cancer)
cancer$V7 = as.numeric(cancer$V7)
labels = cancer[, 11]

cancer = as.matrix(cancer[,-c(1,11)])
threshold = 0.001

get_distance = function(point, center){
  #cat('get_distance() : point : ',point,' center : ',center,'\n')
  p = length(center)
  dist = 0
  for(i in 1:p){
    dist = dist + (point[p]-center[p])^2
  }
  dist = sqrt(dist)
  return (dist)
}

get_nearest_cluster = function(point, centers){
  index = 0
  nearest = 99999999999
  for(i in 1:dim(centers)[1]){
    if(all(centers[i,])!=0.0){
      #print(centers[i,])
      distance = get_distance(point,centers[i,])
      if(distance<nearest){
        nearest = distance
        index = i
      }
    }
  }
  return (list("index"=index,"distance"=nearest))
}

initialize_clusters = function(points,k){
  dv = dim(points)[2]
  init_centers = matrix(0,k,dv)
  init_centers[1,] = points[1,]
  points = points[c(-1),]
  counter = 1
  sum = 0.0
  Dx2 = rep(0,dim(points)[1])
  while(counter < k){
    for(i in 1:dim(points)[1]){
      center_list = get_nearest_cluster(points[i,],init_centers)
      nearest_dist = center_list$distance
      #cat('initialize_clusters() : nearest distance : ',nearest_dist)
      sum = sum + (nearest_dist*nearest_dist)
      Dx2[i] = sum
    }
    counter = counter + 1
    random_no = runif(1,0,1) * sum
    for(i in 1:length(Dx2)){
      if(Dx2[i]>=random_no){
        init_centers[counter,] = points[i,]
        points = points[c(-i),]
        break
      }
    }
  }
  return (init_centers)
}

k_means_plus = function(k){
  d = dim(cancer)
  n = d[1]
  p = d[2]
  
  
  #initialize centroids
  centers = initialize_clusters(cancer,k)# center for kth cluster
  dist = matrix(0,k,n)            #distance from ith point to kth cluster center
  cluster = rep(0,n)              #cluster for point data[i,]
  
  sse = 300000
  prev_sse = 0
  while (sse != prev_sse) {
    for (i in 1:n) {
      for (j in 1:k) {
        for (l in 1:p) {
          dist[j, i] = dist[j, i] + (cancer[i, l] - centers[j, l]) ^ 2
        }
        dist[j, i] = (sqrt(dist[j, i]))
      }
      cluster[i] = which.min(dist[, i])  #will assign to one of the cluster with first minimum value encountered
    }
    total_sum = 0
    for (j in 1:k) {
      if (sum(cluster == j) == 0 || sum(cluster == j) == 1)
        next
      new_center = colMeans(cancer[cluster == j, ])
      #estimating distance
      centers[j,] = new_center
      within_cluster_sum = 0
      for (point in 1:n) {
        if (cluster[point] == j) {
          for (dim in 1:p) {
            within_cluster_sum = within_cluster_sum + (cancer[point, dim] - centers[j, dim]) ^ 2
          }
        }
        #within_cluster_sum = (sqrt(within_cluster_sum))
      }
      total_sum = total_sum + within_cluster_sum
    }
    prev_sse = sse
    sse = total_sum
  }
  
  error = 0
  for (j in 1:k) {
    good_count = 0
    bad_count = 0
    for (l in 1:n) {
      if (cluster[l] == j) {
        if (labels[l] == 2)
          good_count = good_count + 1
        else
          bad_count  = bad_count + 1
      }
    }
    if (sum(cluster == j) > 0) {
      error = error + if (good_count >= bad_count)
        (bad_count / (bad_count + good_count))
      else
        (good_count / (good_count + bad_count))
    }
  }
  return (list("error" = error, "sse" = sse))
}


error_vector = rep(rep(0, 20), 4)
sse_vector = rep(rep(0, 20), 4)
start_time = Sys.time()
i = 0
for (c in 1:4) {
  cat('center : ',(c+1),'\n')
  for (iter in 1:20) {
    return_list = k_means_plus(c + 1)
    #print(return_list)
    error = return_list$error
    sse = return_list$sse
    error_vector[i + iter] =  error
    sse_vector[i + iter] =  sse
  }
  i = i + 20
}
end_time = Sys.time()
print(end_time - start_time)