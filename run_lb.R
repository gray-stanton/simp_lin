args <- commandArgs(trailingOnly = TRUE)
simnum <- as.numeric(args[[1]])
method <- args[[2]]
set.seed(323 * simnum)
library(simplin)
setwd("./output/")
simulate(100, method=method, id=simnum)
