#  R package GRAD file R/ggplotglm.R
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

# Regression Diagnostics for  GLM object 

#' @title ggplot of GLM diagnostics
#' @param x A GLM object 
#' @return plot 
#' @import ggthemes grid gridExtra ggplot2 
#'@examples
#'require(ggplot2)
#'  counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' 
#' glm.D <- glm(counts ~ outcome + treatment, family=poisson())
#' ggplot.glm(glm.D) 
#'  
#' @export 
#
# ggplot of a GLM object
ggplot.glm <- function(x){
  df <- fortify(x)
  df <- cbind(df, rows=1:nrow(df))
with(x,{  
  # residuals vs fitted
  g1 <- ggplot(df, aes(.fitted, .resid)) +
    geom_point()  +
    geom_smooth(color="blue3",fill = "gray60", size = 1.2, alpha = 0.4,method="lm",se = F) +
    geom_hline(linetype=2, size=.2) +
    scale_x_continuous("Fitted Values") +
    scale_y_continuous("Residuals") +
    ggtitle("Residuals vs Fitted")+
    theme_economist_white(gray_bg = F, base_size = 11, base_family = "sans")+
    theme(plot.title = element_text(hjust=0.5),axis.text.y=element_text(hjust=1),
          axis.title.y=element_text(vjust=0.75),text = element_text(size=15))
  
  # normal qq
  a <- quantile(df$.stdresid, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  g2 <- ggplot(df, aes(sample=.resid)) +
    stat_qq() +
    geom_abline(size=1.2,color="blue3",slope=slope, intercept=int) +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Std. Residuals") +
    ggtitle("Normal Q-Q")+
    theme_economist_white(gray_bg = F, base_size = 11, base_family = "sans")+
    theme(plot.title = element_text(hjust=0.5),axis.text.y=element_text(hjust=1),
          axis.title.y=element_text(vjust=0.75),text = element_text(size=15))
  
  # scale-location
  g3 <- ggplot(df, aes(.fitted, sqrt(abs(.stdresid)))) +
    geom_point() +
    geom_smooth(color="blue3",fill = "gray60", size = 1.2, alpha = 0.4,method="lm",se = F) +
    scale_x_continuous("Fitted Values") +
    scale_y_continuous(expression(sqrt("Std. Residuals"))) +
    ggtitle("Scale-Location")+
    theme_economist_white(gray_bg = F, base_size = 11, base_family = "sans")+
    theme(plot.title = element_text(hjust=0.5),axis.text.y=element_text(hjust=1),
          axis.title.y=element_text(vjust=0.75),text = element_text(size=15))
  
  # cook's distance
  g4 <-  ggplot(df, aes(rows, .cooksd, ymin=0, ymax=.cooksd)) +
    geom_point() + geom_linerange(size=1.2,color="blue3") +
    scale_x_continuous("Observation") +
    scale_y_continuous("Cook's distance") +
    ggtitle("Cook's Distance")+
    theme_economist_white(gray_bg = F, base_size = 11, base_family = "sans")+
    theme(plot.title = element_text(hjust=0.5),axis.text.y=element_text(hjust=1),
          axis.title.y=element_text(vjust=0.75),text = element_text(size=15))
  
  # residuals vs leverage
  g5 <- ggplot(df, aes(.hat, .stdresid)) +
    geom_point() +
    geom_smooth(color="blue3",fill = "gray60", size = 1.2, alpha = 0.4,method="lm",se = F) +
    geom_hline(linetype=2, size=.2) +
    scale_x_continuous("Leverage") +
    scale_y_continuous("Std. Residuals") +
    ggtitle("Residuals vs Leverage")+
    theme_economist_white(gray_bg = F, base_size = 11, base_family = "sans")+
    theme(plot.title = element_text(hjust=0.5),axis.text.y=element_text(hjust=1),
          axis.title.y=element_text(vjust=0.75),text = element_text(size=15))
  
  # cooksd vs leverage
  g6 <- ggplot(df, aes(.hat, .cooksd)) +
    geom_point() +
    geom_smooth(color="blue3",fill = "gray60", size = 1.2, alpha = 0.4,method="lm",se = F) +
    scale_x_continuous("Leverage") +
    scale_y_continuous("Cook's distance") +
    ggtitle("Cook's dist vs Leverage")+
    theme_economist_white(gray_bg = F, base_size = 11, base_family = "sans")+
    theme(plot.title = element_text(hjust=0.5),axis.text.y=element_text(hjust=1),
          axis.title.y=element_text(vjust=0.75),text = element_text(size=15))

  return(grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2, main = ""))
})
}
