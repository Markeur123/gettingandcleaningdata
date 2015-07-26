NRfeatures <- function(x) {
  # Convert x to lower case
  x <- tolower(x)
  
  if (substr(x, 1, 1)=='t') {
    x <- sub("t", "time", x)
  } else if (substr(x, 1, 1)=='f') {
    x <- sub("f", "freq", x)
  }
  
  x <- gsub("-", "", x)
  x <- sub("\\(", "", x)
  x <- sub("\\)", "", x)
}
