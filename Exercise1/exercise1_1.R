
### Kristin Meier
### R script for MSiA 400 Lab 1 
### Excersise 1 Problem 1
### 10/16/2016

# create line charts for the 3 functions on the same set of axes
  # create the data frame w/ all functions and plot from there
  # use maximum range for y values (-4:4)

ex1_chart <- function(){

 plotloc <- "/Users/kmeier92/Documents/Northwestern/fall2016/Competitive_Analytics/labs/exercises/"

 ##############################
 # 1. Make Data
 ##############################
 
   # sin(x) between -4 and 4
    x <- seq(-4,4,0.2)
    sinx <- sin(x)
   # create data frame
    plotdata <- data.frame(x,sinx)
  
   # create x^2 b/w -2 and 2
    plotdata$x2 <- NULL
    plotdata$x2[plotdata$x >= -2 & plotdata$x <= 2] <- (plotdata$x[plotdata$x >= -2 & plotdata$x <= 2])^2
  
   # create binary step function
    plotdata$step <- NULL
    plotdata$step[plotdata$x >= 0 & plotdata$x <= 3] <- 1
    plotdata$step[plotdata$x >= -3 & plotdata$x < 0] <- 0
  
 ##############################
 # 2. Make Plot
 ##############################

  pdf(paste(plotloc,"ex1chart.pdf",sep=""))
  
    # set up plot info here 
    # easy to change if necessary 
    labels <- c("sin(x)","x^2","step")
    colors <- c("red","green","blue")
    types <- c("l","p","o")
    ymin <- min(plotdata[,-1],na.rm=T)
    ymax <- max(plotdata[,-1],na.rm=T)
    title <- "Exercise 1 Chart"
      
    # first plot sin(x)
    plot(x = plotdata$x,
         y = plotdata$sinx,
         type = types[1],
         col = colors[1],
         ylim = c(ymin,ymax),
         xlab = "x",
         ylab = "y",
         main = title
         )
    # next plot x^2
    lines(x = plotdata$x,
          y = plotdata$x2,
          type = types[2],
          col = colors[2])
    # last plot step function
    lines(x = plotdata$x,
          y = plotdata$step,
          type = types[3],
          col = colors[3])
    # add legend to top middle
    legend("topleft",
           legend = labels,
           cex = 0.9,
           col = colors,
           lty = c(1,NA,1),
           pch = c(NA,1,1),
           bty = "n",
           inset = 0.03)

 dev.off()

}

# run function to create chart
ex1_chart()
