---
title: "Generalized Linear Models for  Astronomy"
author: "Rafael S. de Souza."
date: "August 14th, 2014"
output: pdf_document
---

Installation
------------

CosmoGLM is available from github. The current development version can be installed using devtools.

```r
library(devtools)
install_github("CosmoR", username="RafaelSdeSouza", subdir="CosmoGLM")

```


This is an R Markdown document to explain in simple terms the use of Generalized Linear Models into Astronomy.


# Required libraries
```{r,results='hide',message=FALSE, cache=FALSE}



require(CosmoGLM)

```
Reading the data 

Set working directory to the data folder (replace by your own directory)
```{r}
data_path<-"/Users/rafael/Dropbox/artigos/Meusartigos/IAA-WGC/GLMs/Simulation/data/"

Biffi_data<-read.table(file=paste(data_path,"Biffi2014.csv",sep=""),
                       header=TRUE)

```

```{r}
Biffi_original<-Biffi_data
```

 Problem 1: xmol, Z, SFR. SFR is the response variable
```{r}
Biffi_data<-Biffi_data[,c("SFR","Xmol","Z")]
```


Transforming  variable into numeric (required by GLM packages) 

```{r}
Biffi_data$SFR[which(Biffi_data$SFR==0)]<-0
Biffi_data$SFR[which(Biffi_data$SFR>0)]<-1
Biffi_data$SFR<-as.numeric(Biffi_data$SFR)
Biffi_data$Z<-scale(Biffi_data$Z)#Scaling and centering
Biffi_data$Xmol<-scale(Biffi_data$Xmol)#Scaling and centering
```  


Frequentist
```{r}
Fglm <-glm(SFR~Z+Xmol,family=binomial(link="logit"),
                 data = Biffi_data)
```


```{r,fig.width=8, fig.height=9}

plotProb(Fglm)+ylab("Predicted Probabilities for SF")+xlab("")+
ggtitle("Frequentist Logistic Regression")+coord_cartesian(ylim = c(0,1.05))
```

