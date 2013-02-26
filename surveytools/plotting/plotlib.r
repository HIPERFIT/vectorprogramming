saveplot <- function(plot, filename, size) {
  pdf(filename, width=1.618*size, height=1*size)
  print(plot)
  notused <- dev.off()
}

create_speedup_plot <- function (frame, greyscale, uselineplot) {
    p <- ggplot(data = frame, aes(x=Size, y=RelativeTime, fill=Language, colour=Language, shape=Language)) + theme_bw()
    if(greyscale) {
      p <- p + scale_colour_grey(start = 0, end = .6)
      p <- p + scale_fill_grey(start = 0, end = .6)
    }

    limits <- aes(ymax = RelativeStddevLB, ymin=RelativeStddevUB)

    if(uselineplot) {
      p <- p + geom_line(stat="identity")
      p <- p + geom_point(size=3)
      p <- p + geom_errorbar(limits, width=0.04)
    } else {
      # Bar
      # Dodge is used to position the bars beside each other instead of on
      # top of each other
      dodge <- position_dodge(width=0.9)
      p <- p + geom_bar(stat="identity", position=dodge)
      p <- p + geom_errorbar(limits, width=0.3, position=dodge)
    }

    # Configure scales
    p <- p + scale_x_log10(breaks=basis_data[,1], name="Problem size", label=comma_format())
    p <- p + scale_y_log10(breaks=c(0, 0.001,0.01,0.05,0.1,0.5,1,2,10,25,50,100,200,400,600,1000,2000,4000), name="Speed-up", label=comma_format())

    #p <- p + ylab(paste("Speed-up compared to ", file_path_sans_ext(basisfile)))

    # Hide unnecessary grid lines from x axis
    p <- p + theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank())

    return(p)
}

create_time_plot <- function (data, greyscale, uselineplot) {
    p <- ggplot(data = frame, aes(x=Size, y=AbsoluteTime, fill=Language, colour=Language, shape=Language)) + theme_bw()
    if(greyscale) {
      p <- p + scale_colour_grey(start = 0, end = .6)
      p <- p + scale_fill_grey(start = 0, end = .6)
    }
    limits <- aes(ymax = AbsoluteStddevLB, ymin=AbsoluteStddevUB)


    if(uselineplot) {
      p <- p + geom_line(stat="identity")
      p <- p + geom_point(size=3)
      p <- p + geom_errorbar(limits, width=0.04)
    } else {
      # Bar
      # Dodge is used to position the bars beside each other instead of on
      # top of each other
      dodge <- position_dodge(width=0.9)
      p <- p + geom_bar(stat="identity", position=dodge)
      p <- p + geom_errorbar(limits, width=0.3, position=dodge)
    }

    # Configure scales
    p <- p + scale_x_log10(breaks=basis_data[,1], name="Problem size", label=comma_format())
    p <- p + scale_y_log10(breaks=c(0, 0.001,0.01,0.05,0.1,0.5,1,2,10,25,50,100,200,400,600,1000,2000,4000),
                           labels=c("0", "0.001","0.01","0.05","0.1","0.5","1","2","10","25","50","100","200","400","600","1000","2000","4000"),
                           name="Time in seconds") #, label=comma_format())

    #p <- p + ylab(paste("Speed-up compared to ", file_path_sans_ext(basisfile)))

    # Hide unnecessary grid lines from x axis
    p <- p + theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank())

    return(p)
}
