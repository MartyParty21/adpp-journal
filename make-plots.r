source("functions.r")
args<-commandArgs(TRUE)
min.instances.for.summary <- 1
pd <- position_dodge(0)
make.grid.plot(args[1], "plots/", min.instances.for.summary)

