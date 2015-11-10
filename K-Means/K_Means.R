# K-means
# Noah Rubin
# November 9, 2015

library(purrr);

kMeansInitCentroids = function(X, K) {
  randidx = sample(dim(X)[1]);
  return(
    X[randidx[1:K],]
  );
}

assignCentroids = function(X, centroids) {
  idx = X %>%
    apply(., 1, getCentroidIndex, centroids);
  return(idx);
}

getCentroidIndex = function(xi, centroids) {
  return(
    which.min(apply((xi-centroids)^2, 1, sum))
    );
}

computeCentroids = function(X, idx, K) {
  cols = dim(X)[2];
  centroids = matrix(0, K, cols);
  for (i in 1:K) {
    centroids[i,] = apply(X[idx==i,], 2, mean);
  }
  return(centroids);
}

kMeans = function(X, K, iterations) {
  centroids = kMeansInitCentroids(X, K);
  for (i in 1:iterations) {
    print(paste("K-means iteration ", i, "/", iterations, "...", sep=""));
    idx = assignCentroids(X, centroids);
    centroids = computeCentroids(X, idx, K);
  }
  return(
    centroids
  );
}