% Generated by roxygen2 (4.0.1): do not edit by hand
\name{computeCombPCA}
\alias{computeCombPCA}
\title{Combined PCA for training and test sample}
\usage{
computeCombPCA(x, y, robust)
}
\arguments{
\item{x}{a matrix or a data.frame}

\item{y}{a matrix or a data.frame}

\item{robust}{a boolean indicating if robust PCA should be used or not}
}
\value{
PCA projections for each matrix
}
\description{
\code{computeCombPCA} computes combined PCA projections of
the training and test samples.
}
\examples{
#Multivariate data with outliers
library(mvtnorm)
x <- rbind(rmvnorm(100, rep(0, 6), diag(c(5, rep(1,5)))),
          rmvnorm( 15, c(0, rep(20, 5)), diag(rep(1, 6))))
y <- rbind(rmvnorm(100, rep(0, 6), diag(c(5, rep(1,5)))),
          rmvnorm( 15, c(0, rep(20, 5)), diag(rep(1, 6))))
#Here we calculate the principal components
pc <- computeCombPCA(x, y)
}
\author{
Rafael S. de Souza, Alberto Krone-Martins
}
\keyword{misc}

