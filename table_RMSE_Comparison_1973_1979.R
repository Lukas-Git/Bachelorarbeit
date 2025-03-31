rm(list = ls())

library(data.table)
library(glmnet)
library(pracma)
library(CVXR)
library(foreach)
library(doParallel)
library(stats)
library(methods)
library(Matrix)
library(iterators)
library(datasets)
library(base)
library(Assemblage)
library(ggplot2)
library(here)
library(MetBrewer)
library(RColorBrewer)
library(patchwork)
library(gt)
library(tidyverse)
library(gtsummary)
library(flextable)
library(pagedown)
.d <- `[`
{
  get_6_24_n <- function(){cores <- 8
  
  horizon.gap <- 24
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY24.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_24_o <- function(){cores <- 8
  
  horizon.gap <- 24
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY24.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_12_n <- function(){cores <- 8
  
  horizon.gap <- 12
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_12_o <- function(){cores <- 8
  
  horizon.gap <- 12
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_6_n <- function(){cores <- 8
  
  horizon.gap <- 6
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY6.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_6_o <- function(){cores <- 8
  
  horizon.gap <- 6
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY6.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_3_n <- function(){cores <- 8
  
  horizon.gap <- 3
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY3.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_3_o <- function(){cores <- 8
  
  horizon.gap <- 3
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY3.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_1_n <- function(){cores <- 8
  
  horizon.gap <- 1
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY1.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_6_1_o <- function(){cores <- 8
  
  horizon.gap <- 1
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY1.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                        x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:216],
                        rank.OOS = AssDataRanks[outsample3,1:216],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_24_n <- function(){cores <- 8
  
  horizon.gap <- 24
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY24.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_24_o <- function(){cores <- 8
  
  horizon.gap <- 24
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY24.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_12_n <- function(){cores <- 8
  
  horizon.gap <- 12
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_12_o <- function(){cores <- 8
  
  horizon.gap <- 12
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_6_n <- function(){cores <- 8
  
  horizon.gap <- 6
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY6.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_6_o <- function(){cores <- 8
  
  horizon.gap <- 6
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY6.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_3_n <- function(){cores <- 8
  
  horizon.gap <- 3
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY3.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_3_o <- function(){cores <- 8
  
  horizon.gap <- 3
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY3.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_1_n <- function(){cores <- 8
  
  horizon.gap <- 1
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY1.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_3_1_o <- function(){cores <- 8
  
  horizon.gap <- 1
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY1.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:50], 
                        x.weight = AssDataWeights[insample, 1:50], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:50],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                        rank.OOS = AssDataRanks[outsample3,1:50],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_24_n <- function(){cores <- 8
  
  horizon.gap <- 24
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY24.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_24_o <- function(){cores <- 8
  
  horizon.gap <- 24
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY24.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_12_n <- function(){cores <- 8
  
  horizon.gap <- 12
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_12_o <- function(){cores <- 8
  
  horizon.gap <- 12
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_6_n <- function(){cores <- 8
  
  horizon.gap <- 6
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY6.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_6_o <- function(){cores <- 8
  
  horizon.gap <- 6
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY6.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_3_n <- function(){cores <- 8
  
  horizon.gap <- 3
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY3.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_3_o <- function(){cores <- 8
  
  horizon.gap <- 3
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY3.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_1_n <- function(){cores <- 8
  
  horizon.gap <- 1
  
  window.size <- 120
  
  end.dates <- "1978_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY1.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
  get_2_1_o <- function(){cores <- 8
  
  horizon.gap <- 1
  
  window.size <- 120
  
  end.dates <- "1972_DEC"
  
  step.estim.window <- 100000
  
  model.select <- "comp"
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY1.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(which(AssDataY[,2]==end.dates)-horizon.gap+120)
  outsample3 <- outsample - 3
  insample3 <- insample - 3
  print(outsample3)
  class(AssDataY) <- "numeric"
  
  
  
  
  Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:15], 
                        x.weight = AssDataWeights[insample, 1:15], bench = AssDataBench[insample, 1:2],
                        pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:15],
                        Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:15],
                        rank.OOS = AssDataRanks[outsample3,1:15],
                        
                        train.size=1,
                        cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                        model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
  return(Asslvl2)
  }
}

rankOOS <- function(f){
  return(f[["OOS"]][["RMSE"]][[1]])
}
compOOS <- function(f){
  return(f[["OOS"]][["RMSE"]][[2]])
}


f_6_24_n <- get_6_24_n()
r_6_24_n <- rankOOS(f_6_24_n)
c_6_24_n <- compOOS(f_6_24_n)
f_6_24_o <- get_6_24_o()
r_6_24_o <- rankOOS(f_6_24_o)
c_6_24_o <- compOOS(f_6_24_o)
f_6_12_n <- get_6_12_n()
r_6_12_n <- rankOOS(f_6_12_n)
c_6_12_n <- compOOS(f_6_12_n)
f_6_12_o <- get_6_12_o()
r_6_12_o <- rankOOS(f_6_12_o)
c_6_12_o <- compOOS(f_6_12_o)
f_6_6_n <- get_6_6_n()
r_6_6_n <- rankOOS(f_6_6_n)
c_6_6_n <- compOOS(f_6_6_n)
f_6_6_o <- get_6_6_o()
r_6_6_o <- rankOOS(f_6_6_o)
c_6_6_o <- compOOS(f_6_6_o)
f_6_3_n <- get_6_3_n()
r_6_3_n <- rankOOS(f_6_3_n)
c_6_3_n <- compOOS(f_6_3_n)
f_6_3_o <- get_6_3_o()
r_6_3_o <- rankOOS(f_6_3_o)
c_6_3_o <- compOOS(f_6_3_o)
f_6_1_n <- get_6_1_n()
r_6_1_n <- rankOOS(f_6_1_n)
c_6_1_n <- compOOS(f_6_1_n)
f_6_1_o <- get_6_1_o()
r_6_1_o <- rankOOS(f_6_1_o)
c_6_1_o <- compOOS(f_6_1_o)

f_3_24_n <- get_3_24_n()
r_3_24_n <- rankOOS(f_3_24_n)
c_3_24_n <- compOOS(f_3_24_n)
f_3_24_o <- get_3_24_o()
r_3_24_o <- rankOOS(f_3_24_o)
c_3_24_o <- compOOS(f_3_24_o)
f_3_12_n <- get_3_12_n()
r_3_12_n <- rankOOS(f_3_12_n)
c_3_12_n <- compOOS(f_3_12_n)
f_3_12_o <- get_3_12_o()
r_3_12_o <- rankOOS(f_3_12_o)
c_3_12_o <- compOOS(f_3_12_o)
f_3_6_n <- get_3_6_n()
r_3_6_n <- rankOOS(f_3_6_n)
c_3_6_n <- compOOS(f_3_6_n)
f_3_6_o <- get_3_6_o()
r_3_6_o <- rankOOS(f_3_6_o)
c_3_6_o <- compOOS(f_3_6_o)
f_3_3_n <- get_3_3_n()
r_3_3_n <- rankOOS(f_3_3_n)
c_3_3_n <- compOOS(f_3_3_n)
f_3_3_o <- get_3_3_o()
r_3_3_o <- rankOOS(f_3_3_o)
c_3_3_o <- compOOS(f_3_3_o)
f_3_1_n <- get_3_1_n()
r_3_1_n <- rankOOS(f_3_1_n)
c_3_1_n <- compOOS(f_3_1_n)
f_3_1_o <- get_3_1_o()
r_3_1_o <- rankOOS(f_3_1_o)
c_3_1_o <- compOOS(f_3_1_o)

f_2_24_n <- get_2_24_n()
r_2_24_n <- rankOOS(f_2_24_n)
c_2_24_n <- compOOS(f_2_24_n)
f_2_24_o <- get_2_24_o()
r_2_24_o <- rankOOS(f_2_24_o)
c_2_24_o <- compOOS(f_2_24_o)
f_2_12_n <- get_2_12_n()
r_2_12_n <- rankOOS(f_2_12_n)
c_2_12_n <- compOOS(f_2_12_n)
f_2_12_o <- get_2_12_o()
r_2_12_o <- rankOOS(f_2_12_o)
c_2_12_o <- compOOS(f_2_12_o)
f_2_6_n <- get_2_6_n()
r_2_6_n <- rankOOS(f_2_6_n)
c_2_6_n <- compOOS(f_2_6_n)
f_2_6_o <- get_2_6_o()
r_2_6_o <- rankOOS(f_2_6_o)
c_2_6_o <- compOOS(f_2_6_o)
f_2_3_n <- get_2_3_n()
r_2_3_n <- rankOOS(f_2_3_n)
c_2_3_n <- compOOS(f_2_3_n)
f_2_3_o <- get_2_3_o()
r_2_3_o <- rankOOS(f_2_3_o)
c_2_3_o <- compOOS(f_2_3_o)
f_2_1_n <- get_2_1_n()
r_2_1_n <- rankOOS(f_2_1_n)
c_2_1_n <- compOOS(f_2_1_n)
f_2_1_o <- get_2_1_o()
r_2_1_o <- rankOOS(f_2_1_o)
c_2_1_o <- compOOS(f_2_1_o)

forecastingperformance <- data.frame(titel=c("Albacore_Comps","Albacore_Ranks","Albacore_Comps","Albacore_Ranks","Albacore_Comps","Albacore_Ranks"),
                                     I1 = c(c_2_1_o,r_2_1_o,c_3_1_o,r_3_1_o,c_6_1_o,r_6_1_o),
                                     I2 = c(c_2_3_o,r_2_3_o,c_3_3_o,r_3_3_o,c_6_3_o,r_6_3_o),
                                     I3 = c(c_2_6_o,r_2_6_o,c_3_6_o,r_3_6_o,c_6_6_o,r_6_6_o),
                                     I4 = c(c_2_12_o,r_2_12_o,c_3_12_o,r_3_12_o,c_6_12_o,r_6_12_o),
                                     I5 = c(c_2_24_o,r_2_24_o,c_3_24_o,r_3_24_o,c_6_24_o,r_6_24_o),
                                     O1 = c(c_2_1_n,r_2_1_n,c_3_1_n,r_3_1_n,c_6_1_n,r_6_1_n),
                                     O2 = c(c_2_3_n,r_2_3_n,c_3_3_n,r_3_3_n,c_6_3_n,r_6_3_n),
                                     O3 = c(c_2_6_n,r_2_6_n,c_3_6_n,r_3_6_n,c_6_6_n,r_6_6_n),
                                     O4 = c(c_2_12_n,r_2_12_n,c_3_12_n,r_3_12_n,c_6_12_n,r_6_12_n),
                                     O5 = c(c_2_24_n,r_2_24_n,c_3_24_n,r_3_24_n,c_6_24_n,r_6_24_n))

colnames(forecastingperformance)[2:11] <- paste0("Var", rep(1:10))
colnames(forecastingperformance)[1] <- "h -->"

# Erstelle die GT-Tabelle
tbl <- gt(forecastingperformance) %>%
  # Spaltennamen f??r Anzeige anpassen
  cols_label(
    Var1 = "1", Var2 = "3", Var3 = "6", Var4 = "12", Var5 = "24",
    Var6 = "1", Var7 = "3", Var8 = "6", Var9 = "12", Var10 = "24"
  ) %>%
  # Gruppierung f??r Zeitperioden mit Tab Spanner
  tab_spanner(label = "1979m1-1982m12", columns = c(Var1, Var2, Var3, Var4, Var5)) %>%
  tab_spanner(label = "1973m1-1976m12", columns = c(Var6, Var7, Var8, Var9, Var10)) %>%
  tab_stubhead(label = "h -->") %>%
  # Zeilen-Gruppen erstellen
  tab_row_group(group = "Level 2(K=15)", rows = 1:2) %>%
  tab_row_group(group = "Level 3(K=50)", rows = 3:4) %>%
  tab_row_group(group = "Level 6(K=215)", rows = 5:6) %>%
  # Stil der Gruppenzeilen
  tab_style(
    style = list(
      cell_fill(color = "lightgray")
    ),
    locations = cells_row_groups()
  ) %>%
  # Trennlinien nach der ersten Spalte
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(1)),
    locations = cells_body(columns = c("h -->"))
  ) %>%
  # D??nne Trennlinie zwischen den Tab-Spannern
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_column_spanners()
  ) %>%
  # D??nne Linien zwischen Zeilen-Gruppen
  tab_options(
    row_group.border.top.color = "black",
    row_group.border.top.width = px(1),
    row_group.border.bottom.color = "black",
    row_group.border.bottom.width = px(1),
    table_body.border.top.color = "black",
    table_body.border.top.width = px(1)
  ) %>%
  # Linie zwischen Spalten??berschriften und Daten
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_column_labels()
  ) %>%
  # Tabelle ohne ??berfl??ssigen Randstil
  tab_options(
    table.border.top.width = px(0),
    table.border.bottom.width = px(0)
  )

# Tabelle anzeigen
tbl <- tbl %>%
  fmt_number(
    columns = everything(),   # Alle numerischen Spalten formatieren
    decimals = 2              # Auf 2 Nachkommastellen runden
  )
print(tbl)
gtsave(tbl, "D:/Daten/table_RMSE_Comparison_new2.pdf")