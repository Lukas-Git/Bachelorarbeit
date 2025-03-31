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

get_6_24 <- function(){cores <- 8

horizon.gap <- 24

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY24.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_6_12 <- function(){cores <- 8

horizon.gap <- 12

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY12.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
outsample3 <- outsample - 3
insample3 <- insample - 3
print(outsample3)
class(AssDataY) <- "numeric"



Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:185], 
                      x.weight = AssDataWeights[insample, 1:185], bench = AssDataBench[insample, 1:2],
                      pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:185],
                      Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:185],
                      rank.OOS = AssDataRanks[outsample3,1:185],
                      
                      train.size=1,
                      cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                      model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
return(Asslvl2)
}
get_6_6 <- function(){cores <- 8

horizon.gap <- 6

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY6.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
outsample3 <- outsample - 3
insample3 <- insample - 3
print(outsample3)
class(AssDataY) <- "numeric"



Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:185], 
                      x.weight = AssDataWeights[insample, 1:185], bench = AssDataBench[insample, 1:2],
                      pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:185],
                      Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:185],
                      rank.OOS = AssDataRanks[outsample3,1:185],
                      
                      train.size=1,
                      cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                      model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
return(Asslvl2)
}
get_6_3 <- function(){ cores <- 8

horizon.gap <- 3

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY3.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
outsample3 <- outsample - 3
insample3 <- insample - 3
print(outsample3)
class(AssDataY) <- "numeric"



Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:185], 
                      x.weight = AssDataWeights[insample, 1:185], bench = AssDataBench[insample, 1:2],
                      pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:185],
                      Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:185],
                      rank.OOS = AssDataRanks[outsample3,1:185],
                      
                      train.size=1,
                      cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                      model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
return(Asslvl2)
}
get_6_1 <- function(){cores <- 8

horizon.gap <- 1

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY1.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
outsample3 <- outsample - 3
insample3 <- insample - 3
print(outsample3)
class(AssDataY) <- "numeric"



Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:185], 
                      x.weight = AssDataWeights[insample, 1:185], bench = AssDataBench[insample, 1:2],
                      pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:185],
                      Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:185],
                      rank.OOS = AssDataRanks[outsample3,1:185],
                      
                      train.size=1,
                      cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                      model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
return(Asslvl2)
}
get_3_24 <- function(){cores <- 8

horizon.gap <- 24

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY24.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_3_12 <- function(){cores <- 8

horizon.gap <- 12

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY12.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_3_6 <- function(){cores <- 8

horizon.gap <- 6

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY6.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_3_3 <- function(){cores <- 8

horizon.gap <- 3

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY3.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_3_1 <- function(){cores <- 8

horizon.gap <- 1

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY1.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_2_24 <- function(){cores <- 8

horizon.gap <- 24

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000

model.select <- "comp"

AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY24.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_2_12 <- function(){cores <- 8

horizon.gap <- 12

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000

model.select <- "comp"

AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY12.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_2_6 <- function(){cores <- 8

horizon.gap <- 6

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000

model.select <- "comp"

AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY6.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_2_3 <- function(){cores <- 8

horizon.gap <- 3

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000

model.select <- "comp"

AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY3.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
get_2_1 <- function(){cores <- 8

horizon.gap <- 1

window.size <- 120

end.dates <- "2019_DEC"

step.estim.window <- 100000

model.select <- "comp"

AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataWeights.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY1.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl2.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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

rankIS <- function(f){
  return(f[["All.in.sample.info"]][["RMSE.in.sample.norm"]][[1]])
}
compIS <- function(f){
  return(f[["All.in.sample.info"]][["RMSE.in.sample.norm"]][[2]])
}
rankOOS <- function(f){
  return(f[["OOS"]][["RMSE"]][[1]])
}
compOOS <- function(f){
  return(f[["OOS"]][["RMSE"]][[2]])
}

f_6_24 <- get_6_24()
ri_6_24 <- rankIS(f_6_24)
ci_6_24 <- compIS(f_6_24)  
ro_6_24 <- rankOOS(f_6_24)
co_6_24 <- compOOS(f_6_24)

f_6_12 <- get_6_12()
ri_6_12 <- rankIS(f_6_12)
ci_6_12 <- compIS(f_6_12)  
ro_6_12 <- rankOOS(f_6_12)
co_6_12 <- compOOS(f_6_12)

f_6_6 <- get_6_6()
ri_6_6 <- rankIS(f_6_6)
ci_6_6 <- compIS(f_6_6)  
ro_6_6 <- rankOOS(f_6_6)
co_6_6 <- compOOS(f_6_6)

f_6_3 <- get_6_3()
ri_6_3 <- rankIS(f_6_3)
ci_6_3 <- compIS(f_6_3)  
ro_6_3 <- rankOOS(f_6_3)
co_6_3 <- compOOS(f_6_3)

f_6_1 <- get_6_1()
ri_6_1 <- rankIS(f_6_1)
ci_6_1 <- compIS(f_6_1)  
ro_6_1 <- rankOOS(f_6_1)
co_6_1 <- compOOS(f_6_1)

f_3_24 <- get_3_24()
ri_3_24 <- rankIS(f_3_24)
ci_3_24 <- compIS(f_3_24)  
ro_3_24 <- rankOOS(f_3_24)
co_3_24 <- compOOS(f_3_24)

f_3_12 <- get_3_12()
ri_3_12 <- rankIS(f_3_12)
ci_3_12 <- compIS(f_3_12)  
ro_3_12 <- rankOOS(f_3_12)
co_3_12 <- compOOS(f_3_12)

f_3_6 <- get_3_6()
ri_3_6 <- rankIS(f_3_6)
ci_3_6 <- compIS(f_3_6)  
ro_3_6 <- rankOOS(f_3_6)
co_3_6 <- compOOS(f_3_6)

f_3_3 <- get_3_3()
ri_3_3 <- rankIS(f_3_3)
ci_3_3 <- compIS(f_3_3)  
ro_3_3 <- rankOOS(f_3_3)
co_3_3 <- compOOS(f_3_3)

f_3_1 <- get_3_1()
ri_3_1 <- rankIS(f_3_1)
ci_3_1 <- compIS(f_3_1)  
ro_3_1 <- rankOOS(f_3_1)
co_3_1 <- compOOS(f_3_1)

f_2_24 <- get_2_24()
ri_2_24 <- rankIS(f_2_24)
ci_2_24 <- compIS(f_2_24)  
ro_2_24 <- rankOOS(f_2_24)
co_2_24 <- compOOS(f_2_24)

f_2_12 <- get_2_12()
ri_2_12 <- rankIS(f_2_12)
ci_2_12 <- compIS(f_2_12)  
ro_2_12 <- rankOOS(f_2_12)
co_2_12 <- compOOS(f_2_12)

f_2_6 <- get_2_6()
ri_2_6 <- rankIS(f_2_6)
ci_2_6 <- compIS(f_2_6)  
ro_2_6 <- rankOOS(f_2_6)
co_2_6 <- compOOS(f_2_6)

f_2_3 <- get_2_3()
ri_2_3 <- rankIS(f_2_3)
ci_2_3 <- compIS(f_2_3)  
ro_2_3 <- rankOOS(f_2_3)
co_2_3 <- compOOS(f_2_3)

f_2_1 <- get_2_1()
ri_2_1 <- rankIS(f_2_1)
ci_2_1 <- compIS(f_2_1)  
ro_2_1 <- rankOOS(f_2_1)
co_2_1 <- compOOS(f_2_1)

forecastingperformance <- data.frame(titel=c("Albacore_Comps","Albacore_Ranks","Albacore_Comps","Albacore_Ranks","Albacore_Comps","Albacore_Ranks"),
                                     I1 = c(ci_2_1,ri_2_1,ci_3_1,ri_3_1,ci_6_1,ri_6_1),
                                     I2 = c(ci_2_3,ri_2_3,ci_3_3,ri_3_3,ci_6_3,ri_6_3),
                                     I3 = c(ci_2_6,ri_2_6,ci_3_6,ri_3_6,ci_6_6,ri_6_6),
                                     I4 = c(ci_2_12,ri_2_12,ci_3_12,ri_3_12,ci_6_12,ri_6_12),
                                     I5 = c(ci_2_24,ri_2_24,ci_3_24,ri_3_24,ci_6_24,ri_6_24),
                                     O1 = c(co_2_1,ro_2_1,co_3_1,ro_3_1, co_6_1,ro_6_1),
                                     O2 = c(co_2_3,ro_2_3,co_3_3,ro_3_3, co_6_3,ro_6_3),
                                     O3 = c(co_2_6,ro_2_6,co_3_6,ro_3_6, co_6_6,ro_6_6),
                                     O4 = c(co_2_12,ro_2_12,co_3_12,ro_3_12, co_6_12,ro_6_12),
                                     O5 = c(co_2_24,ro_2_24,co_3_24,ro_3_24, co_6_24,ro_6_24))


colnames(forecastingperformance)[2:11] <- paste0("Var", rep(1:10))
colnames(forecastingperformance)[1] <- "h -->"

# Erstelle die GT-Tabelle
tbl <- gt(forecastingperformance) %>%
  # Spaltennamen für Anzeige anpassen
  cols_label(
    Var1 = "1", Var2 = "3", Var3 = "6", Var4 = "12", Var5 = "24",
    Var6 = "1", Var7 = "3", Var8 = "6", Var9 = "12", Var10 = "24"
  ) %>%
  # Gruppierung für Zeitperioden mit Tab Spanner
  tab_spanner(label = "2010m1-2019m12", columns = c(Var1, Var2, Var3, Var4, Var5)) %>%
  tab_spanner(label = "2020m1-2023m12", columns = c(Var6, Var7, Var8, Var9, Var10)) %>%
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
  # Dünne Trennlinie zwischen den Tab-Spannern
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_column_spanners()
  ) %>%
  # Dünne Linien zwischen Zeilen-Gruppen
  tab_options(
    row_group.border.top.color = "black",
    row_group.border.top.width = px(1),
    row_group.border.bottom.color = "black",
    row_group.border.bottom.width = px(1),
    table_body.border.top.color = "black",
    table_body.border.top.width = px(1)
  ) %>%
  # Linie zwischen Spaltenüberschriften und Daten
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
    locations = cells_column_labels()
  ) %>%
  # Tabelle ohne überflüssigen Randstil
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
gtsave(tbl, "D:/Daten/table_RMSE_Comparison.pdf")