# ggplot2 guide: http://wiki.stdout.org/rcookbook/Graphs/
# look at tikzDevice for tex-output
# pdf_cairo() for exotic fonts

library(ggplot2)
library(tools)

size=8
pdf("benchmark.pdf", width=1.618*size, height=1*size)


# read arguments following --args (--args included)
args <- commandArgs(trailingOnly = TRUE)

dir <- args[2]   # where are the results located
basisfile <- args[3] # which of graphs should be treated as basis (zero) which the others compares to

# The graph to measure the other against
basis_data <- read.csv(paste(dir, "/", basisfile, sep=""))
basis <- cbind(basis_data, Language=basisfile)

# Collect remaining results in a dataset
csvfiles = list.files(dir)
dataset <- c()
for(i in 1:length(csvfiles)) {
  csv = csvfiles[i]
  if(csv != basisfile) {
    data <- read.csv(paste(dir, "/", csv, sep=""))
    dataset <- rbind(dataset, cbind(data, Language=file_path_sans_ext(csv)))
  }
}

frame <- data.frame(Size=dataset[,1]
                  , Time=basis_data[,2]/dataset[,2] -1
                  , Language=dataset[,8]
                  , Stddev=dataset[,5]
                    )

# Dodge is used to position the bars beside each other instead of on
# top of each other
dodge <- position_dodge(width=0.25)

# Plot
p = ggplot(data = frame, aes(x=Size, y=Time, fill=Language))
p = p + geom_bar(stat="identity", position=dodge)

# Add error bars
limits <- aes(ymax = Time + Stddev, ymin=Time - Stddev)
p = p + geom_errorbar(limits, position=dodge, width=.1)

# Configure scales
p = p + scale_x_log10(breaks=basis_data[,1], name="Problem size (option expiry time in years)")
p = p + ylab(paste("Speed-up compared to ", file_path_sans_ext(basisfile)))

# Hide unnecessary grid lines from x axis
p = p + theme(panel.grid.minor.x=element_blank())


# Show the plot
print(p)

dev.off()
