# Image Compression K-Means Experiment
# Noah Rubin
# November 9, 2015

########################## Code Attribution ##########################
# All code inspired by Coursera ML course taught by Andrew Ng
# This implementation is simply a translation from MATLAB/Octave to R
######################################################################

# Import image read function, png & jpeg libs
source("~/Documents/7StanfordML/ML_Explorations/K-Means/Imread.R");
# Import K-means algorithm (see K_Means.R for code)
source("~/Documents/7StanfordML/ML_Explorations/K-Means/K_Means.R");
# Set up environment
wd = "/Users/noahrubin1/Pictures/";
imagePath = "EwxWGyt.jpg";
setwd(wd);

################################ Part 1 ##############################
# Read in the image from filepath
A = imread(imagePath);
# Ensure all values are within range 0 - 1
if (mean(A) > 1) {
  A = A/255;
}
# Get the dimensions of the image
img_size = dim(A);
# Reshape the image into Nx3 matrix, 
# where N = number of pixels (width x height)
X = matrix(A, nrow=img_size[1]*img_size[2], ncol=img_size[3]);
######################################################################

################################ Part 2 ##############################
# Set K-means parameters
K = 3;
iterations = 10;
# Run K-means for given parameters
centroids = kMeans(X, K, iterations);
######################################################################

################################ Part 3 ##############################
# Get indexes of closest clusters
idx = assignCentroids(X, centroids);
# Map indexes for each example to centroid values
X_recovered = centroids[idx,];
# Reshape image into proper format (3d matrix)
X_recovered = array(X_recovered, c(img_size[1], img_size[2], img_size[3]));
######################################################################
