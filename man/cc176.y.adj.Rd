%% update HH ref to HH2 in ?cc176
%% document cc176 in HH
\name{cc176.y.adj}
\alias{cc176.y.adj}
\alias{cc176fivenumsd}
\docType{data}
\title{
Adjusted response values and their five number summaries by treatment
level for one model using the cc176 dataset. This dataset is used in two
demos: "bwplot" and "boxplot-ggplot".
}
\description{
Adjusted response values and their five number summaries by treatment
level for one model using the cc176 dataset.
The five number summary is based on the standard deviation.
This data set is used in two \pkg{microplot} demos:\cr
demo("bwplot", package="microplot")\cr
demo("boxplot-ggplot", package="microplot").\cr
In both demos we reproduce HH2 (Heiberger and Holland (2015)) Table 13.2
(page 431), consisting of a
table and aligned boxplot, twice---using the 'LaTeX' tabular environment accessed
through the \code{latex} and \code{microplot} functions, and using an
'MS Word' table accessed through the \code{msWord} and \code{microplot} functions.  In the book we
used manual alignment in the 'LaTeX' code to construct the table (see
chunk 6 in the file referenced by \code{HH::HHscriptnames(13)} for the code).

}
\usage{data("cc176.y.adj")}

\references{
Heiberger, Richard M. and Holland, Burt (2015).
\emph{Statistical Analysis and Data Display: An Intermediate Course
  with Examples in R}, Second Edition.
 Springer Texts in Statistics. Springer.
 ISBN 978-1-4939-2121-8.
 \url{https://link.springer.com/us/book/9781493921218}
}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\examples{
\dontrun{
  ## This example is based on chunks 1, 2, 4, 6 of HH::HHscriptnames(13)
  ## It defines the data(cc176.y.adj) that is used in
  ##    demo("bwplot-lattice")  ## 5 calls to latex() and 5 calls to msWord()
  ##    demo("boxplot-ggplot")  ## 2 calls to latex() and 3 calls to msWord()

data(cc176, package="HH")
cc176.aov <- aov(wt.d ~ rep + wt.n + n.treats*minutes*current,
                 data=cc176)
cc176.y.adj <- cc176$wt.d  -
  (cc176$wt.n - mean(cc176$wt.n))*coef(cc176.aov)["wt.n"]

tmp <-
sapply(split(cc176.y.adj, cc176$current),
       function(x)
         c(min=min(x),
           "m-sd"=mean(x)-sd(x),
           mean=mean(x),
           "m+sd"=mean(x)+sd(x),
           max=max(x)))
cc176fivenumsd <- t(tmp)

save(cc176.y.adj, cc176fivenumsd, file="cc176.y.adj.rda")
}
}
\keyword{datasets}
