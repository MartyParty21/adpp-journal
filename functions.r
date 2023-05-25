### Load the libs 

library(plyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

#alg.palette <- brewer.pal(length(unique(runs$alg)), "Set1")[1:length(unique(runs$alg))-1]
#alg.palette <- brewer.pal(length(unique(runs$alg)), "Set1")[1:length(unique(runs$alg))-1]

orange <-"#E69F00"
blue <- "#56B4E9"
green <- "#009E73"
yellow <- "#F0E442"

get.color <- function(algs) {
  pal <- c()
  for (alg in algs) {
    if (is.na(alg)) {
      pal <- c(pal, "#888888")
    }
    else if (alg == "ADPP-singleProcess") {
      pal <- c(pal, "gray")
    }
    else if (alg == "ADPP-1Core") {
      pal <- c(pal, "springgreen3")
    } else if (alg == "ADPP-4Core") {
      pal <- c(pal, "brown")
    } else if (alg == "ADPP-7Core") {
      pal <- c(pal, "magenta")
    } else if (alg == "ADPP-8Core") {
      pal <- c(pal, "firebrick3")
    } else if (alg == "ADPP-Distributed") {
      pal <- c(pal, "deepskyblue1")
    }
  }
  return(pal)
}

get.shape <- function(algs) {
  pal <- c()
  for (alg in algs) {
    if (is.na(alg)) {
      pal <- c(pal, 7)
    } else if (alg == "ADPP-singleProcess") {
      pal <- c(pal, 1)
    }
    else if (alg == "ADPP-1Core") {
      pal <- c(pal, 12)
    } else if (alg == "ADPP-4Core") {
      pal <- c(pal, 2)
    } else if (alg == "ADPP-7Core") {
      pal <- c(pal, 5)
    }else if (alg == "ADPP-8Core") {
      pal <- c(pal, 22)
    } else if (alg == "ADPP-Distributed") {
      pal <- c(pal, 17)
    }
  }
  return(pal)
}

get.linetype <- function(algs) {
  pal <- c()
  for (alg in algs) {
    if (is.na(alg)) {
      pal <- c(pal, "twodash")
    } else if (alg == "ADPP-singleProcess" | alg == "ADPP-1Core" | alg == "ADPP-4Core" | alg == "ADPP-7Core" | alg == "ADPP-8Core" | alg == "ADPP-Distributed") {
      pal <- c(pal, "solid")
    } else {
      pal <- c(pal, "dashed")
    }
  }
  return(pal)
}

###

common.runs <- function(runs, algs) {
  solved.by.all <- unique(runs$instance)
  for (alg in algs) {
    solved.by.all <- intersect(solved.by.all, unique(runs[runs$alg==alg & is.finite(runs$cost), "instance"]))                              
  }
  
  common.runs <- runs[is.element(runs$instance, solved.by.all) & is.element(runs$alg,algs), ] 
  return(common.runs)
}


############################################
######### MAIN GRAPHS BEGIN ################
############################################

### runtime ###

runtime.vs.nagents <- function(runs, min.instances, maxagents) {
  time.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(time)),
                    mean = mean(time),
                    med = median(time),
                    sd = sd(time),
                    se = sd / sqrt(N),
                    min = min(time),
                    max = max(time))
  time.sum <- time.sum[time.sum$N >= min.instances, ]

  plot <- ggplot(time.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(time.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(time.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(time.sum$alg)), name="method") +

    scale_y_continuous(name="time to converge [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. time to solution")

  return(plot)
}

## speedup ~ no of agents ##

speedup.vs.nagents <- function(runs, min.instances, maxagents) {
  x <-runs
  for (alg in c("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core", "ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")) {
    x$speedup[x$alg==alg] <- 1/(x[x$alg==alg, "time"]/x[x$alg=="ADPP-singleProcess", "time"])
  }

  # summarize

  speedup.sum <- ddply(x, .(nagents, alg, radius),
                       summarise,
                       N = sum(!is.na(speedup)),
                       mean = mean(speedup),
                       med = median(speedup),
                       sd = sd(speedup),
                       se = sd / sqrt(N),
                       min = min(speedup),
                       max = max(speedup))

  speedup.sum <- speedup.sum[speedup.sum$N >= min.instances, ]

  maxy <- max(speedup.sum$mean+speedup.sum$se, na.rm=TRUE)
  plot <- ggplot(speedup.sum, aes(x=nagents, y=mean, color=alg, shape=alg, linetype=alg))+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0, position=pd, size=2, alpha=0.7) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, fill="white", position=pd)+
    #geom_point(aes(y=med), size=3, shape=18, position=pd)+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +
    scale_y_continuous(limits=c(0,maxy), name="avg. speed-up rel. to 1 Core ADPP [-]") +
    scale_x_continuous(limits=c(0, maxagents+3), name="number of robots [-]") +
    geom_hline(yintercept = 1, linetype = "longdash", colour="black", alpha=0.5) +

    scale_color_manual(values=get.color(unique(speedup.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(speedup.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(speedup.sum$alg)), name="method") +
    theme_bw() +
    ggtitle("Avg. speed-up relative to original implementation")

  return(plot)
}

## planning time total ###

planningTimeTotal.per.agent.vs.nagents <- function(runs, min.instances, maxagents) {
  planningTime.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(planningTimeTotal.per.agent)),
                    mean = mean(planningTimeTotal.per.agent),
                    med = median(planningTimeTotal.per.agent),
                    sd = sd(planningTimeTotal.per.agent),
                    se = sd / sqrt(N),
                    min = min(planningTimeTotal.per.agent),
                    max = max(planningTimeTotal.per.agent))
  planningTime.sum <- planningTime.sum[planningTime.sum$N >= min.instances, ]

  plot <- ggplot(planningTime.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(planningTime.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(planningTime.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(planningTime.sum$alg)), name="method") +

    scale_y_continuous(name="planning time [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. sum of planning times per agent")

  return(plot)
}

planningTime.per.agent.vs.nagents <- function(runs, min.instances, maxagents) {
  planningTime.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(planningTime.per.agent)),
                    mean = mean(planningTime.per.agent),
                    med = median(planningTime.per.agent),
                    sd = sd(planningTime.per.agent),
                    se = sd / sqrt(N),
                    min = min(planningTime.per.agent),
                    max = max(planningTime.per.agent))
  planningTime.sum <- planningTime.sum[planningTime.sum$N >= min.instances, ]

  plot <- ggplot(planningTime.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(planningTime.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(planningTime.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(planningTime.sum$alg)), name="method") +

    scale_y_continuous(name="planning time [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. planning time per agent")

  return(plot)
}

## runtime substracted by average planning time for agent  ###

noPlanTime.per.agent.vs.nagents <- function(runs, min.instances, maxagents) {
  time.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(noPlanTime.per.agent)),
                    mean = mean(noPlanTime.per.agent),
                    med = median(noPlanTime.per.agent),
                    sd = sd(noPlanTime.per.agent),
                    se = sd / sqrt(N),
                    min = min(noPlanTime.per.agent),
                    max = max(noPlanTime.per.agent))
  time.sum <- time.sum[time.sum$N >= min.instances, ]

  plot <- ggplot(time.sum, aes(x=nagents, y=mean/1000, color=alg, linetype=alg, shape=alg))+
    geom_errorbar(aes(ymin=(mean-se)/1000, ymax=(mean+se)/1000), width=2, position=pd, size=0.5, alpha=0.5) +
    #geom_errorbar(aes(ymin=(mean-sd)/1000, ymax=(mean+sd)/1000), width=0.1, position=pd, size=2, alpha=1) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    #geom_text(aes(label=N, y=0, size=2), colour="black") +

    scale_color_manual(values=get.color(unique(time.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(time.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(time.sum$alg)), name="method") +

    scale_y_continuous(name="runtime without planning [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. runtime without planning")

  return(plot)
}


## planning vs. no of agents ##

planning.per.agent.vs.nagents <- function(runs, min.instances, maxagents) {
  exp.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                   N = sum(!is.na(planning.per.agent)),
                   mean = mean(planning.per.agent, na.rm=TRUE),
                   sd = sd(planning.per.agent, na.rm=TRUE),
                   se = sd / sqrt(N),
                   min = min(planning.per.agent, na.rm=TRUE),
                   max = max(planning.per.agent, na.rm=TRUE))

  exp.sum <- exp.sum[exp.sum$N >= min.instances, ]

  plot <- ggplot(exp.sum[], aes(x=nagents, y=mean, color=alg, shape=alg, linetype=alg))+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=2, position=pd, size=0.5, alpha=0.5) +
    geom_line(size=1, position=pd)+
    geom_point(size=4, position=pd, fill="white")+
    geom_hline(yintercept = 2, linetype = "longdash", colour="black", alpha=0.5) +
    scale_x_continuous(limits=c(0,maxagents+3),name="number of robots [-]") +
    scale_y_continuous(name="avg. replannings per robot  [-]") +

    scale_color_manual(values=get.color(unique(exp.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(exp.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(exp.sum$alg)), name="method") +

    theme_bw() +
    ggtitle("Avg. number of trajectory planning per robot")

  return(plot)
}

runtime.vs.nagents.norm <- function(runs, min.instances, maxagents) {
  time.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(time)),
                    mean = mean(time),
                    med = median(time),
                    sd = sd(time),
                    se = sd / sqrt(N),
                    min = min(time),
                    max = max(time))
  time.sum <- time.sum[time.sum$N >= min.instances, ]

  time.sum$mean[time.sum$alg == "ADPP-Distributed"] <- time.sum$mean[time.sum$alg == "ADPP-Distributed"] / 4.04

  plot <- ggplot(time.sum, aes(x = nagents, y = mean / 1000, color = alg, linetype = alg, shape = alg)) +
    geom_errorbar(aes(ymin = (mean - se) / 1000, ymax = (mean + se) / 1000), width = 2, position = pd, size = 0.5, alpha = 0.5) +
    geom_line(size = 1, position = pd) +
    geom_point(size = 4, position = pd, fill = "white") +

    scale_color_manual(values = get.color(unique(time.sum$alg)), name = "method") +
    scale_linetype_manual(values = get.linetype(unique(time.sum$alg)), name = "method") +
    scale_shape_manual(values = get.shape(unique(time.sum$alg)), name = "method") +

    scale_y_continuous(name = "time to converge [s]") +
    scale_x_continuous(limits = c(0, maxagents + 3), name = "number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. time to solution - normalized")

  return(plot)
}

speedup.vs.nagents.norm <- function(runs, min.instances, maxagents) {
  adpp_distributed <- subset(runs, alg == "ADPP-Distributed")
  adpp_distributed$time <- adpp_distributed$time / 4.04
  modified_runs <- rbind(subset(runs, alg != "ADPP-Distributed"), adpp_distributed)
  x <- modified_runs
  for (alg in c("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core", "ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")) {
    x$speedup[x$alg == alg] <- 1 / (x[x$alg == alg, "time"] / x[x$alg == "ADPP-singleProcess", "time"])
  }

  # summarize

  speedup.sum <- ddply(x, .(nagents, alg, radius),
                       summarise,
                       N = sum(!is.na(speedup)),
                       mean = mean(speedup),
                       med = median(speedup),
                       sd = sd(speedup),
                       se = sd / sqrt(N),
                       min = min(speedup),
                       max = max(speedup))

  speedup.sum <- speedup.sum[speedup.sum$N >= min.instances, ]

  plot <- ggplot(speedup.sum, aes(x = nagents, y = mean, color = alg, shape = alg, linetype = alg)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 2, position = pd, size = 0.5, alpha = 0.5) +
    geom_line(size = 1, position = pd) +
    geom_point(size = 4, fill = "white", position = pd) +
    scale_y_continuous(limits = c(0, max(speedup.sum$mean + speedup.sum$se, na.rm = TRUE)), name = "avg. speed-up rel. to 1 Core ADPP [-]") +
    scale_x_continuous(limits = c(0, maxagents + 3), name = "number of robots [-]") +
    geom_hline(yintercept = 1, linetype = "longdash", colour = "black", alpha = 0.5) +

    scale_color_manual(values = get.color(unique(speedup.sum$alg)), name = "method") +
    scale_linetype_manual(values = get.linetype(unique(speedup.sum$alg)), name = "method") +
    scale_shape_manual(values = get.shape(unique(speedup.sum$alg)), name = "method") +
    theme_bw() +
    ggtitle("Avg. speed-up relative to original implementation - normalized.")

  return(plot)
}

planningTimeTotal.per.agent.vs.nagents.norm <- function(runs, min.instances, maxagents) {
  planningTime.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                            N = sum(!is.na(planningTimeTotal.per.agent)),
                            mean = mean(planningTimeTotal.per.agent),
                            med = median(planningTimeTotal.per.agent),
                            sd = sd(planningTimeTotal.per.agent),
                            se = sd / sqrt(N),
                            min = min(planningTimeTotal.per.agent),
                            max = max(planningTimeTotal.per.agent))
  planningTime.sum <- planningTime.sum[planningTime.sum$N >= min.instances, ]

  planningTime.sum$mean[planningTime.sum$alg == "ADPP-Distributed"] <- planningTime.sum$mean[planningTime.sum$alg == "ADPP-Distributed"] / 4.04

  plot <- ggplot(planningTime.sum, aes(x = nagents, y = mean / 1000, color = alg, linetype = alg, shape = alg)) +
    geom_errorbar(aes(ymin = (mean - se) / 1000, ymax = (mean + se) / 1000), width = 2, position = pd, size = 0.5, alpha = 0.5) +
    geom_line(size = 1, position = pd) +
    geom_point(size = 4, position = pd, fill = "white") +

    scale_color_manual(values = get.color(unique(planningTime.sum$alg)), name = "method") +
    scale_linetype_manual(values = get.linetype(unique(planningTime.sum$alg)), name = "method") +
    scale_shape_manual(values = get.shape(unique(planningTime.sum$alg)), name = "method") +

    scale_y_continuous(name = "planning time [s]") +
    scale_x_continuous(limits = c(0, maxagents + 3), name = "number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. sum of planning times per agent - normalized")

  return(plot)
}

planningTime.per.agent.vs.nagents.norm <- function(runs, min.instances, maxagents) {
  planningTime.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                            N = sum(!is.na(planningTime.per.agent)),
                            mean = mean(planningTime.per.agent),
                            med = median(planningTime.per.agent),
                            sd = sd(planningTime.per.agent),
                            se = sd / sqrt(N),
                            min = min(planningTime.per.agent),
                            max = max(planningTime.per.agent))
  planningTime.sum <- planningTime.sum[planningTime.sum$N >= min.instances, ]

  planningTime.sum$mean[planningTime.sum$alg == "ADPP-Distributed"] <- planningTime.sum$mean[planningTime.sum$alg == "ADPP-Distributed"] / 4.04

  plot <- ggplot(planningTime.sum, aes(x = nagents, y = mean / 1000, color = alg, linetype = alg, shape = alg)) +
    geom_errorbar(aes(ymin = (mean - se) / 1000, ymax = (mean + se) / 1000), width = 2, position = pd, size = 0.5, alpha = 0.5) +
    geom_line(size = 1, position = pd) +
    geom_point(size = 4, position = pd, fill = "white") +

    scale_color_manual(values = get.color(unique(planningTime.sum$alg)), name = "method") +
    scale_linetype_manual(values = get.linetype(unique(planningTime.sum$alg)), name = "method") +
    scale_shape_manual(values = get.shape(unique(planningTime.sum$alg)), name = "method") +

    scale_y_continuous(name = "planning time [s]") +
    scale_x_continuous(limits = c(0, maxagents + 3), name = "number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. planning time per agent - normalized")

  return(plot)
}

noPlanTime.per.agent.vs.nagents.norm <- function(runs, min.instances, maxagents) {
  time.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(noPlanTime.per.agent)),
                    mean = mean(noPlanTime.per.agent),
                    med = median(noPlanTime.per.agent),
                    sd = sd(noPlanTime.per.agent),
                    se = sd / sqrt(N),
                    min = min(noPlanTime.per.agent),
                    max = max(noPlanTime.per.agent))
  time.sum <- time.sum[time.sum$N >= min.instances, ]

  time.sum$mean[time.sum$alg == "ADPP-Distributed"] <- time.sum$mean[time.sum$alg == "ADPP-Distributed"] / 4.04

  plot <- ggplot(time.sum, aes(x = nagents, y = mean / 1000, color = alg, linetype = alg, shape = alg)) +
    geom_errorbar(aes(ymin = (mean - se) / 1000, ymax = (mean + se) / 1000), width = 2, position = pd, size = 0.5, alpha = 0.5) +
    geom_line(size = 1, position = pd) +
    geom_point(size = 4, position = pd, fill = "white") +

    scale_color_manual(values = get.color(unique(time.sum$alg)), name = "method") +
    scale_linetype_manual(values = get.linetype(unique(time.sum$alg)), name = "method") +
    scale_shape_manual(values = get.shape(unique(time.sum$alg)), name = "method") +

    scale_y_continuous(name = "runtime without planning [s]") +
    scale_x_continuous(limits = c(0, maxagents + 3), name = "number of agents [-]") +

    theme_bw() +
    ggtitle("Avg. runtime without planning - normalized")

  return(plot)
}


### plot everything ###

make.grid.plot <- function(env, plotsdir, min.instances.for.summary) {

#   pd <- position_dodge(2)

  dir <- paste("instances/",env, sep="")
  imgdir <- paste(dir, "/figs/", sep="")
  runs <- read.csv(file=paste(dir, "/data.out.head", sep=""), head=TRUE, sep=";")
  runs <- runs[order(runs$instance, runs$alg),]
  runs$time[runs$time==0] <- NA
  runs$agents.in.cluster <- runs$nagents/runs$clusters
  runs$agents.in.cluster.ceil <- ceiling(runs$nagents/runs$clusters)
  runs$planning.per.agent <- runs$replans / runs$nagents
  runs$expansions.per.replan <- runs$expansions/runs$replans

  runs$planningTimeTotal.per.agent <- runs$planningTime / runs$nagents;

  runs$planningTime.per.agent <- runs$planningTimeTotal.per.agent / runs$planning.per.agent;
  runs$noPlanTime.per.agent <- (runs$time - runs$planningTimeTotal.per.agent)



  maxagents <- max(runs$nagents)

  runs$alg = factor(runs$alg,levels=c("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core", "ADPP-7Core", "ADPP-Distributed"))

  runtime <-
    runtime.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  speedup <-
    speedup.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  planning <-
    planning.per.agent.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  planningTimeTotal <-
   planningTimeTotal.per.agent.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  planningTime <-
   planningTime.per.agent.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core",  "ADPP-Distributed")), min.instances.for.summary, maxagents)

  noPlanTime <-
   noPlanTime.per.agent.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core",  "ADPP-Distributed")), min.instances.for.summary, maxagents)


  runtimeNorm <-
    runtime.vs.nagents.norm(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core",  "ADPP-Distributed")), min.instances.for.summary, maxagents)

  speedupNorm <-
    speedup.vs.nagents.norm(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  planningTimeTotalNorm <-
   planningTimeTotal.per.agent.vs.nagents.norm(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  planningTimeNorm <-
   planningTime.per.agent.vs.nagents.norm(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  noPlanTimeNorm <-
   noPlanTime.per.agent.vs.nagents.norm(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)



  ggsave(filename=paste(plotsdir, env,"-runtime.pdf", sep=""), plot=runtime + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-speedup.pdf", sep=""), plot=speedup + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-planning.pdf", sep=""), plot=planning + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-planningTimeTotal.pdf", sep=""), plot=planningTimeTotal + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-planningTime.pdf", sep=""), plot=planningTime + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-runtime-noPlan.pdf", sep=""), plot=noPlanTime + theme(legend.position="bottom"), width=8, height=4)

  ggsave(filename=paste(plotsdir, env,"-runtime-norm.pdf", sep=""), plot=runtimeNorm + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-speedup-norm.pdf", sep=""), plot=speedupNorm + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-planning.pdf", sep=""), plot=planning + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-planningTimeTotal-norm.pdf", sep=""), plot=planningTimeTotalNorm + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-planningTime-norm.pdf", sep=""), plot=planningTimeNorm + theme(legend.position="bottom"), width=8, height=4)
  ggsave(filename=paste(plotsdir, env,"-runtime-noPlan-norm.pdf", sep=""), plot=noPlanTimeNorm + theme(legend.position="bottom"), width=8, height=4)

#   ggsave(filename=paste(plotsdir, env,"-prolong.pdf", sep=""), plot=prolong, width=8, height=5)Norm

  # create a table of individual plots...

  g_legend<-function(p){
    tmp <- ggplotGrob(p)
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

  legend <- g_legend(runtime)
  lwidth <- sum(legend$width)
  lheight <- sum(legend$heights)

  grid.plots <- arrangeGrob(
    runtime + theme(legend.position="bottom"),
    speedup + theme(legend.position="bottom"),
    planning + theme(legend.position="bottom"),
    planningTimeTotal + theme(legend.position="bottom"),
    planningTime + theme(legend.position="bottom"),
    noPlanTime + theme(legend.position="bottom"),
    runtimeNorm + theme(legend.position="bottom"),
    speedupNorm + theme(legend.position="bottom"),
    planningTimeTotalNorm + theme(legend.position="bottom"),
    planningTimeNorm + theme(legend.position="bottom"),
    noPlanTimeNorm + theme(legend.position="bottom"),
    ncol=1)
  
  # some versions of ggplot2 and gridExtra are incompatible, causing the following line to fail
  ggsave(filename=paste(plotsdir, env,".pdf", sep=""), plot=grid.plots, width=10, height=60, limitsize = FALSE)
  
  grid.plots
}