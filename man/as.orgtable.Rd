\name{as.orgtable}
\alias{as.orgtable}
\title{
Prepare a matrix or data.frame to be used as an org-mode table
}
\description{
Prepare a matrix or data.frame to be used as an org-mode table.  Column
names are required.  Row names are optional (and default to FALSE
}
\usage{
as.orgtable(x, rownames = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Matrix or data.frame.
}
  \item{rownames}{
Logical.  When \code{FALSE} (the default), the row.names are not
displayed in the value.  When \code{TRUE}, the row.names are displayed
in the value.  See the last example for details on this behavior.
}
}
\value{
  Vector of character strings, one item for each row of the argument \code{x}.
  The strings contain the markup that will make them appear as tables in
  an \code{org-mode} document.
}
\author{ Nooreen Dabbish <nerd@temple.edu> and Richard M. Heiberger <rmh@temple.edu> }
\seealso{
\code{\link{microplot}}
}
\examples{
tmp <- matrix(1:12, 3, 4, dimnames=list(letters[1:3], LETTERS[4:7]))
tmp
as.orgtable(tmp)
as.orgtable(tmp, rownames=TRUE)

tmpdf <- data.frame(tmp)
tmpdf
cat(as.orgtable(tmpdf), sep="\n")
cat(as.orgtable(tmpdf, rownames=TRUE), sep="\n")

## This example shows why row names default to FALSE.

tmp2 <- rbind(tmp, tmp)
tmp2
tmp2df <- data.frame(tmp2)
tmp2df

tmp2df <- cbind(" "=row.names(tmp2), group=rep(c("A","B"), each=3), tmp2df)
tmp2df

cat(as.orgtable(tmp2df), sep="\n")  ## this is what we want

## this has the unwanted initial column of 1:6
cat(as.orgtable(tmp2df, rownames=TRUE), sep="\n")
}

\keyword{ utilities}
