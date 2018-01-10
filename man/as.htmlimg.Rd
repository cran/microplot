\name{as.htmlimg}
\alias{as.htmlimg}
\title{
Place a filename or filepath in the format used by HTML
}
\description{
Place a filename or filepath in the format used by HTML, by
surrounding it with
\code{"<img src"} and \code{"/>"} and with possible additional arguments between.
}
\usage{
as.htmlimg(object, height = "80", width = NULL, wd = getwd(), align = "middle")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Vector of character strings containing filenames.
}
  \item{height, width}{
Number of pixels as a character string.
}
  \item{wd}{
The directory in which the files reside.  The default is the current
working directory that R is using.
}
\item{align}{
Specifies the alignment of an image according to surrounding elements
(Not supported in HTML5).
One of the strings:    \code{"top"}, \code{"bottom"}, \code{"middle"}, \code{"left"}, \code{"right"}
}
}
\value{
A character vector containing the input strings surrounded by
\code{"<img src"} and \code{"/>"} and with possible additional arguments between.
}
\author{ Nooreen Dabbish <nerd@temple.edu> and Richard M. Heiberger <rmh@temple.edu> }
\seealso{
  \code{\link{microplot}}
  }

\examples{
as.htmlimg("abcd.png")
as.htmlimg("abcd.png", wd=".")
as.htmlimg(c("abcd.png", "efgh.png"))
cat( as.htmlimg("abcd.png")                , "\n")
cat( as.htmlimg("abcd.png", wd=".")        , "\n")
cat( paste(as.htmlimg(c("abcd.png", "efgh.png")), "\n"))

## For an example in context, please see the package example:
##    system.file(package="microplot", "examples/irisRMarkdownHtml.Rmd")
## Copy file irisRMarkdownHtml.Rmd to a directory in which you have write privileges.
## Run the statement
##   rmarkdown::render("irisRMarkdownHtml.Rmd", output_file="irisRMarkdownHtml.html")
## at the R Console.
}
\keyword{ documentation }
\keyword{ utilities}
