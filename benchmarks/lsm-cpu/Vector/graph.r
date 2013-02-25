library(ggplot2)

paths <- read.csv("paths_vector.csv")

#m <- length(paths[1,])

#print(tpaths)
df <- data.frame(PathID = paths[,1], Timestep = paths[,2], Y=paths[,3])

#p = ggplot(data=df, aes(x=Timestep, y=Y, colour=PathID)) + geom_point() + stat_smooth()
#print(p)


ys = df$Y[df$Timestep == 50]

print(summary(ys))
