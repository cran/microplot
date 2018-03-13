\name{LegendrePolyMatrices}
\alias{LegendrePolyMatrices}
\docType{data}
\title{
Legendre Orthogonal Polynomials for various values of alpha and beta.
  The dataset is used in the demo("LegendrePolynomials").
}
\description{
  Legendre Orthogonal Polynomials for various values of alpha and beta.
  The dataset is used in the demo("LegendrePolynomials").
}
\usage{data("LegendrePolyMatrices")}
\author{ Richard M. Heiberger <rmh@temple.edu> }

\examples{
\dontrun{
## dontrun is to avoid requiring the user to install the polynom and orthopolynom packages

## These matrices are used in the demo showing both latex and msWord tables
##     demo("LegendrePolynomials"       , package="microplot", ask=TRUE)

## Legendre Polynomials
if (require(orthopolynom)) {
LP.score <- function(alpha, beta, m = 4, B = 100) {
  x <- seq(1/B, 1 - 1/B, length = B)
  u <- stats::pbeta(x, alpha, beta)
  poly <-  slegendre.polynomials(m, normalized=TRUE)
  data.frame(x=x, T=sapply(poly[-1], predict, u))
}

alphas <- c(.25, .5, 1)
betas <- c(.25, .5, 1, 2, 10)


## generate LegendrePolyMatrices
LegendrePolyMatrices <- matrix(list(), nrow=length(alphas), ncol=length(betas),
                               dimnames=list(alphas=alphas, betas=betas))
for (alpha in seq(along=alphas))
   for (beta in seq(along=betas))
     LegendrePolyMatrices[[alpha, beta]] <- LP.score(alphas[alpha], betas[beta])

save(LegendrePolyMatrices, file="LegendrePolyMatrices.rda")

detach("package:orthopolynom", unload=TRUE)
detach("package:polynom", unload=TRUE)
} else data(LegendrePolyMatrices)
}
}
\keyword{datasets}
