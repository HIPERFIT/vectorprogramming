saveplot <- function(plot, filename, size) {
  pdf(filename, width=1.2*size, height=1*size)
  print(plot)
  notused <- dev.off()
}

create_plot <- function (frame, greyscale, uselineplot, isabsolute, xlabel, timeunit) {
 
    if(isabsolute) {
      .e <- environment()
      p <- ggplot(data = frame, aes(x=Size, y=AbsoluteTime*timeunit, fill=Language, colour=Language, shape=Language), environment = .e)
      limits <- aes(ymax = AbsoluteStddevUB*timeunit, ymin=AbsoluteStddevLB*timeunit)

      if(timeunit == 1) {
        yaxis_label <- "Time (s)"
      } else if (timeunit == 1000) {
        yaxis_label <- "Time (ms)"
      } else if (timeunit == 1000000) {
        yaxis_label <- expression(paste("Time (", mu, "s)"))
      } else {
        stop("error in time unit")
      }

    } else {
      p <- ggplot(data = frame, aes(x=Size, y=RelativeTime, fill=Language, colour=Language, shape=Language))
      limits <- aes(ymax = RelativeStddevUB, ymin=RelativeStddevLB)

      yaxis_label <- "Speed-up"
    }

    if(greyscale) {
      p <- p + scale_colour_grey(start = 0, end = .6)
      p <- p + scale_fill_grey(start = 0, end = .6)
      p <- p + theme_bw()
    }
    
    if(uselineplot) {
      p <- p + geom_line(stat="identity")
      p <- p + geom_point(size=3)
      p <- p + geom_errorbar(limits, width=0.07)
    } else {
      # Bar
      # Dodge is used to position the bars beside each other instead of on
      # top of each other
      dodge <- position_dodge(width=0.9)
      p <- p + geom_bar(stat="identity", position=dodge)
      p <- p + geom_errorbar(limits, width=0.3, position=dodge)
    }

    # Configure scales
    p <- p + scale_x_log10(breaks=basis_data[,1], name=xlabel) #, label=comma_format())
    p <- p + scale_y_log10(breaks=c(0, 0.001,0.01,0.05,0.1,0.5,1,2,4,8,16,32,50,100,200,400,600,1000,2000,4000), name=yaxis_label, label=comma_format())

    #p <- p + ylab(paste("Speed-up compared to ", file_path_sans_ext(basisfile)))

    # Hide unnecessary grid lines from x axis
    p <- p + theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank())

    # Move legend below the plot
    p <- p + theme(legend.position="bottom")

    # No legend title
    p <- p + theme(legend.title = element_blank())

    # Select nice shapes
    # See possibilities here: http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
    p <- p + scale_shape_manual(values=c(15,16,17,18,4))
    
    return(p)
}

## create_time_plot <- function (data, greyscale, uselineplot, xlabel) {
##     p <- ggplot(data = frame, aes(x=Size, y=AbsoluteTime*1000, fill=Language, colour=Language, shape=Language))
##     if(greyscale) {
##       p <- p + scale_colour_grey(start = 0, end = .6)
##       p <- p + scale_fill_grey(start = 0, end = .6)
##       p <- p + theme_bw()
##     }
##     limits <- aes(ymax = AbsoluteStddevLB*1000, ymin=AbsoluteStddevUB*1000)


##     if(uselineplot) {
##       p <- p + geom_line(stat="identity")
##       p <- p + geom_point(size=3)
##       p <- p + geom_errorbar(limits, width=0.04)
##     } else {
##       # Bar
##       # Dodge is used to position the bars beside each other instead of on
##       # top of each other
##       dodge <- position_dodge(width=0.9)
##       p <- p + geom_bar(stat="identity", position=dodge)
##       p <- p + geom_errorbar(limits, width=0.3, position=dodge)
##     }

##     # Configure scales
##     p <- p + scale_x_log10(breaks=basis_data[,1], name=xlabel) #, label=comma_format())
##     p <- p + scale_y_log10(breaks=c(0, 0.001,0.01,0.05,0.1,0.5,1,2,5,10,25,50,100,200,400,1000,2000,4000,8000,16000),
##                            labels=c("0", "0.001","0.01","0.05","0.1","0.5","1","2","5","10","25","50","100","200","400","1000","2000","4000", "8000", "16000"),
##                            name="Time (ms)") #, label=comma_format())

##     #p <- p + ylab(paste("Speed-up compared to ", file_path_sans_ext(basisfile)))

##     # Hide unnecessary grid lines from x axis
##     p <- p + theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank())

##     # Move legend below the plot
##     p <- p + theme(legend.position="bottom")

##     # No legend title
##     p <- p + theme(legend.title = element_blank())

##     # Select nice shapes
##     # See possibilities here: http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
##     p <- p + scale_shape_manual(values=c(15,16,17,18,4))
    
##     return(p)
## }
