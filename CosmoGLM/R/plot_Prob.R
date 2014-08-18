#  R package CosmoGLM file R/plot_Prob.R
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

#' @title ggplot of probabilities for glm object
#' @param glm glm  object 
#' @return plot 
#' @import ggthemes  ggplot2 scales 
#'@examples
#'require(ggplot2)
#'require(scales)
#'require(ggthemes)
#'  counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' 
#' glm.D <- glm(counts ~ outcome + treatment, family=poisson())
#' plot_Prob(glm.D) 
#'  
#' @export 
#
plot_Prob<-function(glm){
  #eta<-exp(coef(glm))[-1]
  d<-data.frame(parameter=names(glm$coef)[-1],
                y=exp(coef(glm))[-1]/(1+exp(coef(glm))[-1]),
                ylo=exp(confsand(glm)[-1,1])/(1+exp(confsand(glm)[-1,1])),
                yhi=exp(confsand(glm)[-1,2])/(1+exp(confsand(glm)[-1,2])))
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