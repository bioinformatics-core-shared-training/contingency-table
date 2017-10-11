library(shiny)
library(ggplot2)
library(knitr)

shinyServer(function(input, output){
  
  data <- reactive({

      table <- input$table
      Table <- do.call(rbind,sapply(strsplit(table, "--")[[1]], function(x) strsplit(x, "|",fixed=TRUE)))

      ncol <- ncol(Table)
      nrow <- nrow(Table)
      .Table <- matrix(as.numeric(Table), nrow,ncol,byrow=FALSE)
  })
  
  
  output$mytable <- renderPrint({
    .Table <- data()
    ncol <- ncol(.Table)
    nrow <- nrow(.Table)
    
    colnames(.Table) <- paste0("Col",1:ncol)
    
    .Table <- data.frame(.Table, Total = rowSums(.Table))
    .Table <- rbind(.Table, colSums(.Table))
    rownames(.Table) <- c(paste0("Row",1:nrow),"Total")
    kable(.Table)
  })
  
  output$summary <- renderPrint({
    .Table <- data()
    ncol <- ncol(.Table)
    nrow <- nrow(.Table)
    
    colnames(.Table) <- paste0("Col",1:ncol)
    
    CrossTable(.Table,chisq = TRUE,fisher=TRUE,prop.c = FALSE,prop.r=TRUE,prop.t = FALSE,prop.chisq = FALSE)
    
  })
  
  output$result <- renderPrint({
    .Table <- data()

    if(input$test =="chi-squared"){
      .Test <- chisq.test(.Table,correct=FALSE)
    } else  .Test <- fisher.test(.Table)
    
    .Test
  })
  
  output$frequencies<- renderPrint({
    .Table <- data()
     
     .Test <- chisq.test(.Table,correct=FALSE)
    .Test$expected
  })
  
  output$distribution <- renderPlot({
    
    .Table <- data()
    
    if(input$test =="chi-squared"){
      .Test <- chisq.test(.Table,correct=FALSE)
    
      degf <- .Test$parameter

      xmax <- max(4,.Test$statistic)
      xs <- seq(0, xmax, length.out = 10000)
      df <- data.frame(X = xs, Y = dchisq(xs,degf))
      
      title <- substitute(paste(chi^2, " with ", degf, " degrees of freedom"),list(degf=degf))
      
      p <- ggplot(df, aes(x=X,y=Y)) + geom_line() + geom_vline(xintercept=.Test$statistic,col="red") + xlim(0,xmax+1) + ggtitle(title)
      p
    }
    
  })
  
}
)
  

