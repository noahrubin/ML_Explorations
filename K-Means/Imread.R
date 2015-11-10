# Image Read Helper Function
# Noah Rubin
# November 9, 2015

library(png);
library(jpeg);

imread = function(imagePath, nat=F) {
  if (grep(".jpg|.JPG|jpeg|.JPEG", imagePath)) {
    return(
      readJPEG(imagePath, native=nat)
    );
  } else if (grep(".png|.PNG", imagePath)) {
    return(
      readPNG(imagePath, native=nat)
    );
  } else {
    warning("Unrecognized file format");
  }
}


