#  R package CosmoGLM file R/plotProbmcmc.R
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

# Plot coefficient odds for GLM object using ggplot 

#' @title ggplot of probabilities for MCMC object
#' @param mcmcglm mcmc  object 
#' @return plot 
#' @import ggthemes  ggplot2 scales coda
#' @importFrom MCMCpack MCMClogit
#'@examples
#'require(ggplot2)
#'require(scales)
#'require(ggthemes)
#'require(MCMCpack)
#'  response <- c(0,1,0,0,1,1,1,0,1)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' 
#' glm.D <- MCMClogit(response ~ outcome + treatment, family=poisson())
#' plotProbmcmc(glm.D) 
#'  
#' @export 
#
plotProbmcmc<-function(mcmcglm){
  #eta<-exp(coef(glm))[-1]
  q1<-summary(mcmcglm)$quantiles[-1,1]
  q2<-summary(mcmcglm)$quantiles[-1,3]
  q3<-summary(mcmcglm)$quantiles[-1,5]
  d<-data.frame(parameter=varnames(mcmcglm)[-1],
                y=exp(q2)/(1+exp(q2)),
                ylo=exp(q1)/(1+exp(q1)),
                yhi=exp(q3)/(1+exp(q3)))
  d$parameter<-as.factor(d$parameter)
  # d is a data frame with 4 columns
  # d$x gives variable names
  # d$y gives center point
  # d$ylo gives lower limits
  # d$yhi gives upper limits
  with(d,{ limits<-aes(ymax=yhi,ymin=ylo)
           pp<-ggplot(d, aes(x=parameter, y=y, ymin=ylo, ymax=yhi),group=x)+geom_pointrange(data=d,aes(color=parameter),pch=18,lwd=2)+
             scale_color_stata(guide="none")+
             #  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
             #                 labels = trans_format("log10", math_format(10^.x)))+
             theme_economist_white(gray_bg = F, base_size = 11, base_family = "sans")+
             theme(plot.title = element_text(hjust=0.5),axis.title.y=element_text(vjust=0.75),text = element_text(size=20))+
             ggtitle("GLM")
           return(pp)
  })
}