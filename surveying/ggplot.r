# ggplot2 guide: http://wiki.stdout.org/rcookbook/Graphs/

library(ggplot2)
#library(scales)

size=8
pdf("binomial_benchmark.pdf", width=1.618*size, height=1*size)
# look at tikzDevice for tex-output
# pdf_cairo() for exotic fonts

# Format the data for plotting
cuda <- read.csv("results/Cuda.csv")
repa <- read.csv("results/Repa.csv")
nikola <- read.csv("results/Nikola-Precompiled.csv")
rlang <- read.csv("results/R.csv")
vector <- read.csv("results/Vector.csv")

dataset <- rbind(cbind(cuda, Language="CUDA")
               , cbind(repa, Language="Repa")
               , cbind(nikola, Language="Nikola")
               , cbind(rlang, Language="R")
               , cbind(vector, Language="Vector"))

frame <- data.frame(Size=dataset[,1]
                  , Time=dataset[,2]
                  , Language=dataset[,8]
                  , Stddev=dataset[,5])


# Plot
p = ggplot(data = frame, aes(x=Size, y=Time, colour=Language))
p = p + geom_line()
p = p + geom_point()

# Error bars
limits <- aes(ymax = Time + Stddev, ymin=Time - Stddev)
p = p + geom_errorbar(limits, width=.02)

# Configure scales
breaks=c(0.001, 0.01, 0.1, 1, 5, 10, 20, 40, 80, 100)
p = p + scale_y_log10(limits = c(0.001,100)
                    , name="Execution time (seconds)"
                    , breaks=breaks
                    , labels=as.character(breaks))
p = p + scale_x_log10(breaks=cuda[,1], name="Problem size (option expiry time in years)")

# Hide unnecessary grid lines
p = p + theme(panel.grid.minor=element_blank(), panel.grid.major.x=element_blank())

# Show the plot
print(p)

dev.off()
