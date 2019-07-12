library(shiny)

function(input, output) {
  
  
  output$normalCurve <- renderPlot({            
      
    distrMean <- input$slider1
    distrSD <- input$slider2
    if (input$select1 == 1) {
        xLow <- -150
        xHigh <- max(input$slider3)
    }
    if (input$select1 == 2) {
        xLow <- min(input$slider3)
        xHigh <- max(input$slider3)
    }
    if (input$select1 == 3) {
        xLow <- min(input$slider3)
        xHigh <- 250
    }
    
    myNormCurve <- function(distrMean, distrSD, xLow, xHigh) {
      range <- seq(xLow,xHigh,length.out = 100)
      cord.x <- c(xLow, range, xHigh) 
      cord.y <- c(0,dnorm(range, distrMean, distrSD),0) 
      curve(dnorm(x,distrMean,distrSD),xlim=c(0, 150), main='Normal Density Curve', bty="n", xaxt="n")
      abline(h=0)
      abline(v=distrMean, lty = "dashed")
      polygon(cord.x,cord.y,col='grey50', border = NA)
      axis( side=1, at= c(xLow,xHigh, 0, distrMean, 150), labels=as.character(c(xLow,xHigh, 0, distrMean, 150)),  tcl=-0.5 )
    }
    myNormCurve(distrMean, distrSD, xLow, xHigh)
  })

  
  output$text2 <- renderText({           
      
    distrMean <- input$slider1
    distrSD <- input$slider2
    if (input$select1 == 1) {
        xLow <- -150
        xHigh <- max(input$slider3)
    }
    if (input$select1 == 2) {
        xLow <- min(input$slider3)
        xHigh <- max(input$slider3)
    }
    if (input$select1 == 3) {
        xLow <- min(input$slider3)
        xHigh <- 250
    }
    
    if (input$select1 == 1){
      p <- pnorm(xHigh, mean = distrMean, sd = distrSD)
    } 
    if (input$select1 == 2) {
      p <- pnorm(xHigh, mean = distrMean, sd = distrSD) - pnorm(xLow, mean = distrMean, sd = distrSD)
    } 
    if (input$select1 == 3) {
      p <- pnorm(xLow, mean = distrMean, sd = distrSD, lower.tail = FALSE)
    }
    
    if (input$select1 == 1) {
        print(c("Proportion of observations below", xHigh, "(probability of randomly drawing an observation below", xHigh, ") = ", round(p, digits = 3)))
    } else if (input$select1 == 2) {
        print(c("Proportion of observation between", xLow, "and", xHigh, "(probability of randomly drawing an observation between", xLow, "and", xHigh, ") = ", round(p, digits = 3)))
    } else if (input$select1 == 3) {
        print(c("Proportion of observation above", xLow, "(probability of randomly drawing an observation above", xLow, ") = ", round(p, digits = 3)))
    }
    
  })
  
  
  
  output$standardNormalCurve <- renderPlot({          
      
    distrMean <- input$slider1
    distrSD <- input$slider2
    
    if (input$select1 == 1) {
        xLow <- -150
        xHigh <- max(input$slider3)
    }
    if (input$select1 == 2) {
        xLow <- min(input$slider3)
        xHigh <- max(input$slider3)
    }
    if (input$select1 == 3) {
        xLow <- min(input$slider3)
        xHigh <- 250
    }
    
    zLow <- (xLow-distrMean)/distrSD
    zHigh <- (xHigh-distrMean)/distrSD
    
    myStandardNormCurve <- function(zLow, zHigh) {
      range <- seq(zLow,zHigh,0.01)
      cord.x <- c(zLow, range, zHigh) 
      cord.y <- c(0,dnorm(range),0)
      curve(dnorm(x,0,1),xlim=c(-4,4),main="Standard Normal Density Curve (Mean = 0, SD = 1, Area = 1)", bty="n", xaxt="n")
      abline(h=0)
      polygon(cord.x,cord.y,col='grey50', border = NA)
      axis( side=1, at= c(zLow,zHigh, -3, -2, -1, 0, 1, 2, 3), labels=as.character(c(zLow,zHigh, -3, -2, -1, 0, 1, 2, 3)),  tcl=-0.5 )
    }
    myStandardNormCurve(zLow, zHigh)
    if (input$select1 == 1){text(c(2, 2.6), 0.2, c("z-score =", round(zHigh, digits = 2)))} 
    if (input$select1 == 2){text(c(2, 2.7, 2.1, 2.8), c(0.3, 0.3, 0.25, 0.25), c("z-score Low =", round(zLow, digits = 2), "z-score High =", round(zHigh, digits = 2)))} 
    if (input$select1 == 3){text(c(2, 2.6), 0.2, c("z-score =", round(zLow, digits = 2)))}
    
  })
  

  output$text3 <- renderText({ 
      if (input$radio1 == 1) {value <- qnorm(input$slider4/100, mean = input$slider12, sd = input$slider22, lower.tail = FALSE)}
      if (input$radio1 == 2) {value <- qnorm(input$slider4/100, mean = input$slider12, sd = input$slider22)}
      if (input$radio1 == 3) {
          tailProps <- (1-(input$slider4/100))/2
          valueHigh <- qnorm(tailProps, mean = input$slider12, sd = input$slider22, lower.tail = FALSE)
          valueLow <- qnorm(tailProps, mean = input$slider12, sd = input$slider22)
          value <- c(valueLow, valueHigh)
      }
      print(c("Percent of cases:", input$slider4, "---", "Tail (1=Top/2=Bottom):", input$radio1, "---",  "Value(s) of interest =", round(value, digits = 2)))
  })
  
  
  output$normalCurve2 <- renderPlot({            # Plots NORMAL Density Curve (Mean = Choose, SD = Choose, Area = 1)
      
      if (input$radio1 == 1) {value <- qnorm(input$slider4/100, mean = input$slider12, sd = input$slider22, lower.tail = FALSE)}
      if (input$radio1 == 2) {value <- qnorm(input$slider4/100, mean = input$slider12, sd = input$slider22)}
      if (input$radio1 == 3) {
          tailProps <- (1-(input$slider4/100))/2
          valueHigh <- qnorm(tailProps, mean = input$slider12, sd = input$slider22, lower.tail = FALSE)
          valueLow <- qnorm(tailProps, mean = input$slider12, sd = input$slider22)
          value <- c(valueLow, valueHigh)
      }
      
      distrMean <- input$slider12
      distrSD <- input$slider22
      
      if (input$radio1 == 1) {
          xLow <- value
          xHigh <- 250
      }
      if (input$radio1 == 2) {
          xLow <- -150
          xHigh <- value
      }
      if (input$radio1 == 3) {
          xLow <- valueLow
          xHigh <- valueHigh
      }
      
      myNormCurve2 <- function(distrMean, distrSD, xLow, xHigh) {
          range <- seq(xLow,xHigh,length.out = 100)
          cord.x <- c(xLow, range, xHigh) 
          cord.y <- c(0,dnorm(range, distrMean, distrSD),0) 
          curve(dnorm(x,distrMean,distrSD),xlim=c(0, 150), main='Normal Density Curve', bty="n", xaxt="n")
          abline(h=0)
          abline(v=distrMean, lty = "dashed")
          polygon(cord.x,cord.y,col='grey50', border = NA)
          axis( side=1, at= c(xLow,xHigh, 0, distrMean, 150), labels=as.character(c(round(xLow, digits = 2), round(xHigh, digits = 2), 0, distrMean, 150)),  tcl=-0.5 )
      }
      myNormCurve2(distrMean, distrSD, xLow, xHigh)
      if (input$radio1 == 1){text(c(distrMean+2*distrSD, distrMean+2*distrSD+15), range(dnorm(distrMean,distrMean,distrSD)*0.75), c("Value =", round(xLow, digits = 2)))} 
      if (input$radio1 == 2){text(c(distrMean+2*distrSD, distrMean+2*distrSD+15), range(dnorm(distrMean,distrMean,distrSD)*0.75), c("Value =", round(xHigh, digits = 2)))} 
      
  })
  
  
  output$standardNormalCurve2 <- renderPlot({       
      
      if (input$radio1 == 1) {value <- qnorm(input$slider4/100, mean = input$slider12, sd = input$slider22, lower.tail = FALSE)}
      if (input$radio1 == 2) {value <- qnorm(input$slider4/100, mean = input$slider12, sd = input$slider22)}
      if (input$radio1 == 3) {
          tailProps <- (1-(input$slider4/100))/2
          valueHigh <- qnorm(tailProps, mean = input$slider12, sd = input$slider22, lower.tail = FALSE)
          valueLow <- qnorm(tailProps, mean = input$slider12, sd = input$slider22)
          value <- c(valueLow, valueHigh)
      }
      
      distrMean <- input$slider12
      distrSD <- input$slider22
      
      if (input$radio1 == 1) {
          xLow <- value
          xHigh <- 250
      }
      if (input$radio1 == 2) {
          xLow <- -150
          xHigh <- value
      }
      if (input$radio1 == 3) {
          xLow <- valueLow
          xHigh <- valueHigh
      }
      
      zLow <- (xLow-distrMean)/distrSD
      zHigh <- (xHigh-distrMean)/distrSD
      
      myStandardNormCurve2 <- function(zLow, zHigh) {
          range <- seq(zLow,zHigh,0.01)
          cord.x <- c(zLow, range, zHigh) 
          cord.y <- c(0,dnorm(range),0)
          curve(dnorm(x,0,1),xlim=c(-4,4),main="Standard Normal Density Curve (Mean = 0, SD = 1, Area = 1)", bty="n", xaxt="n")
          abline(h=0)
          polygon(cord.x,cord.y,col='grey50', border = NA)
          axis( side=1, at= c(zLow,zHigh, -3, -2, -1, 0, 1, 2, 3), labels=as.character(c(round(zLow, digits = 2), round(zHigh, digits = 2), -3, -2, -1, 0, 1, 2, 3)),  tcl=-0.5 )
      }
      myStandardNormCurve2(zLow, zHigh)
      if (input$radio1 == 1){text(c(2, 2.65), 0.2, c("z-score =", round(zLow, digits = 2)))} 
      if (input$radio1 == 2){text(c(2, 2.65), 0.2, c("z-score =", round(zHigh, digits = 2)))}
      if (input$radio1 == 3){text(c(2, 2.7, 2.1, 2.8), c(0.3, 0.3, 0.25, 0.25), c("z-score Low =", round(zLow, digits = 2), "z-score High =", round(zHigh, digits = 2)))} 
  })
  
  
}




