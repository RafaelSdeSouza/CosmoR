% Generated by roxygen2 (4.0.1): do not edit by hand
\name{computeCombPCA}
\alias{computeCombPCA}
\title{Combined PCA for training and test sample}
\usage{
computeCombPCA(x, y, npcvar)
}
\arguments{
\item{x}{matrix or data.frame}

\item{y}{matrix or data.frame}
}
\value{
PCA projections for each matrix
}
\description{
Combined PCA for training and test sample
}
\examples{
Multivariate data with outliers
library(mvtnorm)
x <- rbind(rmvnorm(100, rep(0, 6), diag(c(5, rep(1,5)))),
          rmvnorm( 15, c(0, rep(20, 5)), diag(rep(1, 6))))
y <- rbind(rmvnorm(100, rep(0, 6), diag(c(5, rep(1,5)))),
          rmvnorm( 15, c(0, rep(20, 5)), diag(rep(1, 6))))
pc <- PCAgrid(x)
}

