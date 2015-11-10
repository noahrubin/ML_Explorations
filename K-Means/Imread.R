# Image Read Helper Function
# Noah Rubin
# November 9, 2015

library(png);
library(jpeg);

imread = function(imagePath, nat=F) {
  # Returns: Bitmap of Image (3d array)
  # Is meant to mirror matlab imread function
  # Precondition: Image is either .jpeg, .jpg, or .png
  ## Note: K-Means works better with images in RGB/HSV/YUV format,
  ## instead of gray-scale
  
  # if image is .jpg or .jpeg
  if (grep(".jpg|.JPG|jpeg|.JPEG", imagePath)) {
    # read with jpeg library
    return(
      readJPEG(imagePath, native=nat)
    );
  # if image is .png
  } else if (grep(".png|.PNG", imagePath)) {
    # read with png library
    return(
      readPNG(imagePath, native=nat)
    );
  # else, raise a warning that the file is unrecognized
  } else {
    warning("Unrecognized file format");
  }
}


