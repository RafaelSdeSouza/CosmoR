library(shiny)
library(CosmoPhotoz)
library(ggplot2)
library(ggthemes)
options(shiny.maxRequestSize=100*1024^2) # This is to change the maximum size of the upload to 30 MB

shinyServer(function(input, output) {

  # Now, we need to create the reactive container
  shinyCompPhotoZ <- reactive({
    # First we need to have some data, of course!
      if(input$dataSourceFlag == FALSE) {
        # First the file used for training
        inFile1 <- input$file1
        if (is.null(inFile1)) {
          return(NULL)
        }
        # Then the file used for estimating
        inFile2 <- input$file2
        if (is.null(inFile2)) {
          return(NULL)
          #return("\n GLM PhotoZ Estimator :: No file was uploaded for redshift estimation! ")
        }
        # Now read the files
        PHAT0train <- read.table(file=inFile1$datapath, header=TRUE)
        PHAT0test <- read.table(file=inFile2$datapath, header=TRUE)
      } else {
        # In this case, just lazy load the data from inside the package
        data(PHAT0train)
        data(PHAT0test)
      }

      # Combine the training and test data and calculate the principal components
      PC_comb <- computeCombPCA(subset(PHAT0train, select=c(-redshift)),
                                subset(PHAT0test,  select=c(-redshift)))    
      Trainpc <- cbind(PC_comb$x, redshift=PHAT0train$redshift)
      Testpc <- PC_comb$y

      # Dynamic generation of the formula based on the user selected number of PCs
      formM <- paste(names(PC_comb$x[1:6]), collapse="*") ## THE NUMBER OF PCS USED ENTER HERE
      formM <- paste("redshift~",formM, sep="")
      
      # Fitting
      Fit <- glmTrainPhotoZ(Trainpc, formula=eval(parse(text=formM)), method=input$method, family=input$family)
#      Fit<-glmTrainPhotoZ(Trainpc, formula=redshift~Comp.1*Comp.2*Comp.3*Comp.4*Comp.5*Comp.6, method=input$method, family=input$family)
      
      # Photo-z estimation
      photoz <- predict(Fit$glmfit, newdata=Testpc, type="response")
      specz <- PHAT0test$redshift

    # Time to return the data!
    return(data.frame(photoz, specz))
  })

  # Create the output text
  output$diagnostics <- renderPrint({
    tempObj <- shinyCompPhotoZ()
    if(!is.null(tempObj)) {
      computeDiagPhotoZ(tempObj$photoz, tempObj$specz)
    }
  })

  # Create basic comparison plots
  output$errorDistPlot <- renderPlot({
    tempObj <- shinyCompPhotoZ()
    if(!is.null(tempObj)) {
      plotDiagPhotoZ(tempObj$photoz, tempObj$specz, type = "errordist")
    }
  })
  output$predictObs <- renderPlot({
    tempObj <- shinyCompPhotoZ()
    if(!is.null(tempObj)) {
      plotDiagPhotoZ(tempObj$photoz, tempObj$specz, type = "predobs")
    }
  })
  output$violins <- renderPlot({
    tempObj <- shinyCompPhotoZ()
    if(!is.null(tempObj)) {
      plotDiagPhotoZ(tempObj$photoz, tempObj$specz, type = "errorviolins")
    }
  })
  output$box <- renderPlot({
    tempObj <- shinyCompPhotoZ()
    if(!is.null(tempObj)) {
      plotDiagPhotoZ(tempObj$photoz, tempObj$specz, type = "box")
    }
  })

  # Download the data
  output$downloadData <- downloadHandler(
    filename = function() { "glmPhotoZresults.dat" },
    content = function(file) {
      write.table(shinyCompPhotoZ_estimate(), file, quote=F, sep=" ", col.names = FALSE, row.names = FALSE)
    }
  )

})
