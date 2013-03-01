# ggplot2 guide: http://wiki.stdout.org/rcookbook/Graphs/
# look at tikzDevice for tex-output
# pdf_cairo() for exotic fonts

library(ggplot2, quietly=TRUE)
library(tools, quietly=TRUE)
library(scales, quietly=TRUE)

source("plotlib.r")

uselineplot <- TRUE
greyscale <- FALSE

# read arguments following --args (--args included)
args <- commandArgs(trailingOnly = TRUE)
dir <- args[2]   # where the results are located
basisfile <- args[3] # which of graphs should be treated as basis (zero) which the others compares to



# Store the resulting graph in the given directory
size <- 4

# Xlabel
con <- file(paste(dir, "/xlabel", sep=""))
open(con);
xlabel <- readLines(con, n = 1, warn = FALSE)

# The graph to measure the other against
basis_data <- read.csv(paste(dir, "/", basisfile, sep=""))
basis <- cbind(basis_data, Language=basisfile)

formatLanguageName <- function(csv_filename) {
  benchmarkname <- file_path_sans_ext(csv_filename)
  lang <- strsplit(benchmarkname, "-")
  return(lang[[1]][3])
}

# Collect remaining results in a data frame
csvfiles <- list.files(dir)
frame <- data.frame()
for(i in 1:length(csvfiles)) {
  csv <- csvfiles[i]
  if(file_ext(csv) == "csv") {
    data <- read.csv(paste(dir, "/", csv, sep=""))

    # Only include results with some data
    if(any(is.na(data[,2]))) {
      print(paste("=== WARNING: Incomplete data in ", csv, " - will not be included!"))
    } else {
      n <- min(length(data[,1]), length(basis_data[,1]))

      newframe <- data.frame(Size=data[1:n,1]
                             , Language=formatLanguageName(csv)
                             , AbsoluteTime=data[1:n,2]
                             , AbsoluteStddevLB=data[1:n,2]-data[1:n,6]
                             , AbsoluteStddevUB=data[1:n,2]+data[1:n,7]
                             , RelativeTime=basis_data[1:n,2]/data[1:n,2]
                             , RelativeStddevLB=basis_data[1:n,2]/(data[1:n,2]-data[1:n,6])
                             , RelativeStddevUB=basis_data[1:n,2]/(data[1:n,2]+data[1:n,7])
                             )
      frame <- rbind(frame, newframe)
    }
  }
}

# Plot
p <- create_speedup_plot(frame, greyscale, uselineplot, xlabel)
saveplot(p, paste(dir, "/speedup-graph.pdf", sep=""), size)

p <- create_time_plot(frame, greyscale, uselineplot, xlabel)
saveplot(p, paste(dir, "/time-graph.pdf", sep=""), size)
