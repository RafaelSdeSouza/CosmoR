#  R package GRAD file R/confsand.R
#  Copyright (C) 2014  Rafael S. de Souza
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License version 3 as published by
#the Free Software Foundation.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

# Robust confidence intervals for GLM object

#' @title Robust confidence levels for GLM object
#' @param x  glm object 
#' @return Confidence invervals  
#' @import sandwich 
#'@examples
#'require(sandwich)
#'  counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' 
#' glm.D <- glm(counts ~ outcome + treatment, family=poisson())
#' confsand(glm.D) 
#'  
#'@export 
#
confsand<- function (x){  
  level = 0.95
  cf <- coef(x)
  pnames <- names(cf)
  # if (missing(parm))
  parm <- pnames
  #  else if (is.numeric(parm))
  #    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  ses <- sqrt(diag(sandwich::vcovHC(x)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  return(ci)
}