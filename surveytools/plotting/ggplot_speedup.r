# ggplot2 guide: http://wiki.stdout.org/rcookbook/Graphs/
# look at tikzDevice for tex-output
# pdf_cairo() for exotic fonts

library(ggplot2, quietly=TRUE)
library(tools, quietly=TRUE)
library(scales, quietly=TRUE)

uselineplot = TRUE
greyscale = TRUE

# read arguments following --args (--args included)
args <- commandArgs(trailingOnly = TRUE)

dir <- args[2]   # where the results are located
basisfile <- args[3] # which of graphs should be treated as basis (zero) which the others compares to

outputfile <- paste(dir, "/speedup-graph.pdf", sep="")

# Store the resulting graph in the given directory
size=8
pdf(outputfile, width=1.618*size, height=1*size)

# The graph to measure the other against
basis_data <- read.csv(paste(dir, "/", basisfile, sep=""))
basis <- cbind(basis_data, Language=basisfile)

# Collect remaining results in a data frame
csvfiles = list.files(dir)
frame <- data.frame()
for(i in 1:length(csvfiles)) {
  csv = csvfiles[i]
  if(file_ext(csv) == "csv") {
    data <- read.csv(paste(dir, "/", csv, sep=""))

    # Only include results with some data
    if(any(is.na(data[,2]))) {
      print(paste("=== WARNING: Incomplete data in ", csv, " - will not be included!"))
    } else {
      n <- min(length(data[,1]), length(basis_data[,1]))

      newframe <- data.frame(Size=data[1:n,1]
                             , Time=basis_data[1:n,2]/data[1:n,2]
                             , Language=file_path_sans_ext(csv)
                             , StddevLB=basis_data[1:n,2]/(data[1:n,2]-data[1:n,6])
                             , StddevUB=basis_data[1:n,2]/(data[1:n,2]+data[1:n,7])
                             )
      frame <- rbind(frame, newframe)
    }
  }
}

# Plot
p = ggplot(data = frame, aes(x=Size, y=Time, fill=Language, colour=Language, shape=Language)) + theme_bw()
if(greyscale) {
  p = p + scale_colour_grey(start = 0, end = .6)
  p = p + scale_fill_grey(start = 0, end = .6)
}
limits <- aes(ymax = StddevLB, ymin=StddevUB)


if(uselineplot) {
  p = p + geom_line(stat="identity")
  p = p + geom_point(size=3)
  p = p + geom_errorbar(limits, width=0.04)
} else {
  # Bar
  # Dodge is used to position the bars beside each other instead of on
  # top of each other
  dodge <- position_dodge(width=0.9)
  p = p + geom_bar(stat="identity", position=dodge)
  p = p + geom_errorbar(limits, width=0.3, position=dodge)
}

# Configure scales
p = p + scale_x_log10(breaks=basis_data[,1], name="Problem size (option expiry time in years)", label=comma_format())
p = p + scale_y_log10(breaks=c(0, 0.001,0.01,0.05,0.1,0.5,1,2,10,25,50,100,200,400,600,1000,2000,4000), name="Speed-up", label=comma_format())

#p = p + ylab(paste("Speed-up compared to ", file_path_sans_ext(basisfile)))

# Hide unnecessary grid lines from x axis
p = p + theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank())

# Save the plot
print(p)
#dev.off()
