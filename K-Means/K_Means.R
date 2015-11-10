# K-means
# Noah Rubin
# November 9, 2015

library(purrr);

kMeansInitCentroids = function(X, K) {
  # Returns: K centroids randomly chosen from X
  # Precondition: X is N x j, where N = width x height
  # of image, and j is color scale values.  So, for 
  # RGB, HSV, or YUV, j = 3, and for gray-scale, j = 1
  
  # Randomly shuffle X
  randidx = sample(dim(X)[1]);
  # Return first K examples from shuffle
  return(
    X[randidx[1:K],]
  );
}

assignCentroids = function(X, centroids) {
  # Returns: Index of assigned centroid for 
  # each example.
  # Precondition: X is N x j (see kMeansInitCentroids
  # for def of N and j), centroids is K x j, where K
  # is the K chosen for K-means. 
  
  # get centroid index for each example
  idx = X %>%
    apply(., 1, getCentroidIndex, centroids);
  return(idx);
}

getCentroidIndex = function(xi, centroids) {
  ## Helper function for assignCentroids
  # Assign centroid that minimizes euclidean
  # distance from xi, an example in X
  return(
    which.min(apply((xi-centroids)^2, 1, sum))
    );
}

computeCentroids = function(X, idx, K) {
  # Returns: Centroids computed after assignment
  # of examples in X to clusters.
  # For each cluster U_ci, the value of the jth color
  # U_ci_j is the mean of the jth color of all examples
  # assigned to cluster ci (mean(xi_j)).
  # Precondition: length(idx) == dim(X)[1]
  
  # Get number of columns in X (the value of j)
  cols = dim(X)[2];
  # Set up zero matrix (equivalent to zeros(K, cols) in MATLAB)
  centroids = matrix(0, K, cols);
  # for each centroid
  for (i in 1:K) {
    # compute new values U_ci_j for each j
    centroids[i,] = apply(X[idx==i,], 2, mean);
  }
  return(centroids);
}

kMeans = function(X, K, max_iters) {
  # Returns: Centroids computed after max_iters iterations
  # of K-means algorithm.  Each loop get computes the 2 steps
  # of K-means: 1) assign each example to cluster, 2) compute
  # new centroid values.  
  # Precondition: X is N x j, max_iters should be <= 20 for most
  # applications, and certainly affects runtime significantly
  
  # randomly initialize centroids
  centroids = kMeansInitCentroids(X, K);
  # for each iteration
  for (i in 1:max_iters) {
    # Print which iteration
    print(paste("K-means iteration ", i, "/", max_iters, "...", sep=""));
    # assign examples to cluster
    idx = assignCentroids(X, centroids);
    # compute new centroids for each cluster
    centroids = computeCentroids(X, idx, K);
  }
  return(centroids);
}