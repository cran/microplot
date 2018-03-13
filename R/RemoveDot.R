RemoveDot <- function(x)
  gsub("\\.", "-", x)
## change "." to "-" in figPrefix
## The 'latex' macro \code{\\includegraphics} requires that there be no
## \code{"."} in the filename basename.
