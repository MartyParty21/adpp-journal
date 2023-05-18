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

### success rate ####
succ.nagents <- function(runs, timelimit, maxagents) {
  x <- runs[!is.na(runs$time) && runs$time<timelimit, ]
  succ <- ddply(x, .(nagents, alg, radius), summarise,
                solved = sum(is.finite(cost)),
                total = length(unique(instance))
  )

  plot <- ggplot(succ, aes(x=nagents, y=(solved/total)*100, color=alg, linetype=alg))+
    geom_line(size=1, position=pd)+
    geom_point(aes(shape=alg), position=pd, size=4, fill="white") +
    scale_y_continuous(limits=c(0,100), name=paste("instances solved [%]")) +
    scale_x_continuous(limits=c(0,maxagents+3), name="number of robots [-]") +
    scale_color_manual(values=get.color(unique(succ$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(succ$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(succ$alg)), name="method") +
    theme_bw() +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    ggtitle("Coverage") #ggtitle(paste("1: Coverage (", max(succ$total),"instances)"))

  return(plot)
}

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
    scale_x_continuous(limits=c(0,maxagents+3), name="no of robots [-]") +  
    
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
    ggtitle("Avg. speed-up rel. to single-core.")
  
  return(plot)
}

## planning time total ###

planningTimeTotal.per.agent.vs.nagents <- function(runs, min.instances, maxagents) {
  time.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(planningTimeTotal.per.agent)),
                    mean = mean(planningTimeTotal.per.agent),
                    med = median(planningTimeTotal.per.agent),
                    sd = sd(planningTimeTotal.per.agent),
                    se = sd / sqrt(N),
                    min = min(planningTimeTotal.per.agent),
                    max = max(planningTimeTotal.per.agent))
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

    scale_y_continuous(name="planning time [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="no of robots [-]") +

    theme_bw() +
    ggtitle("Avg. sum of planning times per agent")

  return(plot)
}

planningTime.per.agent.vs.nagents <- function(runs, min.instances, maxagents) {
  time.sum <- ddply(runs, .(nagents, alg, radius), summarise,
                    N = sum(!is.na(planningTime.per.agent)),
                    mean = mean(planningTime.per.agent),
                    med = median(planningTime.per.agent),
                    sd = sd(planningTime.per.agent),
                    se = sd / sqrt(N),
                    min = min(planningTime.per.agent),
                    max = max(planningTime.per.agent))
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

    scale_y_continuous(name="planning time [s]") +
    scale_x_continuous(limits=c(0,maxagents+3), name="no of robots [-]") +

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
    scale_x_continuous(limits=c(0,maxagents+3), name="no of robots [-]") +

    theme_bw() +
    ggtitle("Avg. runtime without planning")

  return(plot)
}


## replans vs. no of agents ##

replans.per.agent.vs.nagents <- function(runs, min.instances, maxagents) {  
  exp.sum <- ddply(runs, .(nagents, alg, radius), summarise,  
                   N = sum(!is.na(replans.per.agent)),
                   mean = mean(replans.per.agent, na.rm=TRUE),
                   sd = sd(replans.per.agent, na.rm=TRUE),
                   se = sd / sqrt(N),
                   min = min(replans.per.agent, na.rm=TRUE),
                   max = max(replans.per.agent, na.rm=TRUE))
  
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
    ggtitle("Avg. number of messages broadcast per robot")
  
  return(plot)
}

### quality ###

prolong.vs.nagents <- function(runs, min.instances, maxagents) {
  x <- runs
  
  for (alg in unique(runs$alg)) {
    x$prolong[x$alg==alg] <- 100*((x[x$alg==alg, "cost"]-x[x$alg=="ADPP-1Core", "cost"])/x[x$alg=="ADPP-1Core", "cost"])
  }
  
  # summarize
  
  prolong.sum <- ddply(x[x$alg != "ADPP-1Core"], .(nagents, alg), summarise,
                       N = sum(!is.na(prolong)),
                       mean = mean(prolong, na.rm=TRUE),
                       med = median(prolong, na.rm=TRUE),
                       sd = sd(prolong, na.rm=TRUE),
                       se = sd / sqrt(N),
                       min = min(prolong, na.rm=TRUE),
                       max = max(prolong, na.rm=TRUE))
  
  prolong.sum <- prolong.sum[prolong.sum$N >= min.instances, ]
  
  plot <- ggplot(prolong.sum, aes(x=nagents, y=mean,  color=alg, shape=alg, linetype=alg))+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=2, position=pd, size=0.5, alpha=0.5) +
    geom_line(size=1, position=pd)+ 
    geom_point(size=4, fill="white", position=pd)+ 
    #geom_text(aes(label=N, y=100, size=2), colour="black") + 
    scale_x_continuous(limits=c(0,maxagents+3), name="number of robots [-]") +
    scale_y_continuous(name="prolongation [%]") +
    
    scale_color_manual(values=get.color(unique(prolong.sum$alg)), name="method") +
    scale_linetype_manual(values=get.linetype(unique(prolong.sum$alg)), name="method") +
    scale_shape_manual(values=get.shape(unique(prolong.sum$alg)), name="method") +
    
    theme_bw()  +
    ggtitle("2: Avg. prolongation")
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
  runs$replans.per.agent <- runs$replans / runs$nagents
  runs$expansions.per.replan <- runs$expansions/runs$replans

  runs$planningTimeTotal.per.agent <- runs$planningTime / runs$nagents;

  runs$planningTime.per.agent <- runs$planningTimeTotal.per.agent / runs$replans.per.agent;
  runs$noPlanTime.per.agent <- (runs$time - runs$planningTimeTotal.per.agent)



  maxagents <- max(runs$nagents)
  
  runs$alg = factor(runs$alg,levels=c("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core", "ADPP-7Core", "ADPP-8Core", "ADPP-Distributed"))

  runs$alg.scheme <- NA
  runs$alg.scheme[runs$alg=="ADPP-singleProcess" | runs$alg=="ADPP-1Core" | runs$alg=="ADPP-4Core" | runs$alg=="ADPP-7Core" | runs$alg=="ADPP-8Core" | runs$alg=="ADPP-Distributed"] <- "AD"

  runs$alg.ppvar <- "NA"
  runs$alg.ppvar[runs$alg=="ADPP-singleProcess" | runs$alg=="ADPP-1Core" | runs$alg=="ADPP-4Core" | runs$alg=="ADPP-7Core" | runs$alg=="ADPP-8Core" | runs$alg=="ADPP-Distributed"] <- "PP"

  success <-
    succ.nagents(runs[is.element(runs$alg,.("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")),], Inf, maxagents)

  runtime <-
    runtime.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)
  
  speedup <-
    speedup.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  replans <-
    replans.per.agent.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  planningTimeTotal <-
   planningTimeTotal.per.agent.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  planningTime <-
   planningTime.per.agent.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  noPlanTime <-
   noPlanTime.per.agent.vs.nagents(common.runs(runs, .("ADPP-singleProcess", "ADPP-1Core", "ADPP-4Core","ADPP-7Core", "ADPP-8Core", "ADPP-Distributed")), min.instances.for.summary, maxagents)

  ggsave(filename=paste(plotsdir, env,"-success.pdf", sep=""), plot=success, width=8, height=5)
  ggsave(filename=paste(plotsdir, env,"-runtime.pdf", sep=""), plot=runtime, width=8, height=5)
  ggsave(filename=paste(plotsdir, env,"-speedup.pdf", sep=""), plot=speedup, width=8, height=5)
  ggsave(filename=paste(plotsdir, env,"-replans.pdf", sep=""), plot=replans, width=8, height=5)
  ggsave(filename=paste(plotsdir, env,"-planningTimeTotal.pdf", sep=""), plot=planningTimeTotal, width=8, height=5)
  ggsave(filename=paste(plotsdir, env,"-planningTime.pdf", sep=""), plot=planningTime, width=8, height=5)
  ggsave(filename=paste(plotsdir, env,"-runtime-noPlan.pdf", sep=""), plot=noPlanTime, width=8, height=5)

#   ggsave(filename=paste(plotsdir, env,"-prolong.pdf", sep=""), plot=prolong, width=8, height=5)
  
  # create a table of individual plots...
  
  g_legend<-function(p){
    tmp <- ggplotGrob(p)
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  legend <- g_legend(success)
  lwidth <- sum(legend$width)
  lheight <- sum(legend$heights)
  
  grid.plots <- arrangeGrob(
    success + theme(legend.position="bottom"),
#     prolong + theme(legend.position="none"),
    runtime + theme(legend.position="bottom"),
    speedup + theme(legend.position="bottom"),
    replans + theme(legend.position="bottom"),
    planningTimeTotal + theme(legend.position="bottom"),
    planningTime + theme(legend.position="bottom"),
    noPlanTime + theme(legend.position="bottom"),
    ncol=1)
  
  # some versions of ggplot2 and gridExtra are incompatible, causing the following line to fail
  ggsave(filename=paste(plotsdir, env,".pdf", sep=""), plot=grid.plots, width=10, height=45)
  
  grid.plots
}