
#install.packages("randtoolbox")

library(ggplot2, quietly=TRUE)
library(randtoolbox, quietly=TRUE)

## library(tools, quietly=TRUE)
## library(scales, quietly=TRUE)


greyscale <- TRUE
n <- 400

# Generate Sobol sequence
sobol_sequence <- sobol(n, dim = 2)
sobol_frame <- data.frame(x=sobol_sequence[,1],y=sobol_sequence[,2])

# Plot Sobol sequence
pdf("2D-sobol-sequence.pdf", width=5, height=5)
sobol_plot <- ggplot(data = sobol_frame, aes(x,y))
sobol_plot <- sobol_plot + geom_point()
sobol_plot <- sobol_plot + ylab("") + xlab("")
print(sobol_plot)
dev.off()

# Generate MersenneTwister sequence
set.generator("MersenneTwister", initialization="init2002", resolution=53, seed=12345)
mersenne_frame <- data.frame(x=runif(n), y=runif(n))

# Plot 
pdf("2D-mersenne-sequence.pdf", width=5, height=5)
mersenne_plot <- ggplot(data = mersenne_frame, aes(x,y))
mersenne_plot <- mersenne_plot + geom_point()
mersenne_plot <- mersenne_plot + ylab("") + xlab("")
print(mersenne_plot)
dev.off()
