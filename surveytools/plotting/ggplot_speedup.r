# ggplot2 guide: http://wiki.stdout.org/rcookbook/Graphs/

library(ggplot2)
#library(scales)

#size=8
#pdf("binomial_benchmark.pdf", width=1.618*size, height=1*size)
# look at tikzDevice for tex-output
# pdf_cairo() for exotic fonts

# Format the data for plotting
cuda <- read.csv("results/binomial/Cuda.csv")
repa <- read.csv("results/binomial/Repa.csv")
nikola <- read.csv("results/binomial/Nikola-Precompiled.csv")
rlang <- read.csv("results/binomial/R.csv")
vector <- read.csv("results/binomial/Vector.csv")


basis <- cbind(vector, Language="Vector")

dataset <- rbind(cbind(cuda, Language="CUDA")
               , cbind(repa, Language="Repa")
               , cbind(nikola, Language="Nikola")
               , cbind(rlang, Language="R"))

frame <- data.frame(Size=dataset[,1]
                  , Time=vector[,2]/dataset[,2] -1
                  , Language=dataset[,8]
                  ## , Stddev=dataset[,5]
                    )

# Plot
p = ggplot(data = frame, aes(x=Size, y=Time, fill=Language))
p = p + geom_bar(stat="identity", position=position_dodge())
#p = p + geom_point()

# Error bars
#limits <- aes(ymax = Time + Stddev, ymin=Time - Stddev)
#p = p + geom_errorbar(limits, width=.02)

# Configure scales
## breaks=c(0.1, 1, 5)
## p = p + scale_y_log10(limits = c(0.1,5)
##                     , name="Execution time (seconds)"
##                     , breaks=breaks
##                     , labels=as.character(breaks))
p = p + scale_x_log10(breaks=cuda[,1], name="Problem size (option expiry time in years)")
p = p + ylab("Speed-up compared to Data.Vector")

# Hide unnecessary grid lines
#p = p + theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank())

# Show the plot
print(p)

#dev.off()
