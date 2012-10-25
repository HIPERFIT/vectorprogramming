# Plotting tutorial: http://www.harding.edu/fmccown/r/

cuda <- read.csv("results/Cuda.csv")
repa <- read.csv("results/Repa.csv")
nikola <- read.csv("results/Nikola-Precompiled.csv")
rlang <- read.csv("results/R.csv")
vector <- read.csv("results/Vector.csv")


data <- data.frame(Size=cuda[,1], CUDA=cuda[,2], Repa=repa[,2], Nikola=nikola[,2], R=rlang[,2], Vector=vector[,2])

maxy <- max(data[,2:6])

plotcolors <- c("blue","red","forestgreen", "black", "purple")


plot(data$Size, data$CUDA, type="o", ylim=c(0.001,maxy), axes=FALSE, ann=FALSE, log="xy", xlim=c(1,128),
     col=plot_colors[1])
lines(data$Size, data$Repa, type="o", pch=22, lty=2, col=plotcolors[2])
lines(data$Size, data$Nikola, type="o", pch=23, lty=3, col=plotcolors[3])
lines(data$Size, data$R, type="o", pch=24, lty=4, col=plotcolors[4])
lines(data$Size, data$Vector, type="o", pch=25, lty=5, col=plotcolors[5])

axis(1, at=data$Size)
axis(2, at=c(0:ceiling(maxy)))


legend(1, maxy, names(data[,2:6]), cex=0.8, pch=21:25, lty=1:5, col=plotcolors);

box()

title(xlab= "Years", col.lab=rgb(0,0.5,0))
title(ylab= "Running time (seconds)", col.lab=rgb(0,0.5,0))


X11()

barplot(t(as.matrix(data[,2:6])), log="y", main="", ylab= "Seconds", xlab="Problem size (years)",
   beside=TRUE, col=rainbow(5), names.arg=data$Size)

legend("topleft", names(data[2:6]), cex=1, bty="n", fill=rainbow(5));
