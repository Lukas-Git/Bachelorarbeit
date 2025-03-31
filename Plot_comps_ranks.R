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
.d <- `[`

insample <- 480:720
outsample <- 721:768
horizon.gap <- 12
window.size <- 240

data_ranks <- function(){
cores <- 8

horizon.gap <- 12

window.size <- 240

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl3.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"


insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
outsample3 <- outsample - 3
insample3 <- insample - 3
print(outsample3)
class(AssDataY) <- "numeric"



Asslvl2 <- assemblage(y = AssDataY[insample,1], x = AssDatax[insample, 1:216], 
                      x.weight = AssDataWeights[insample, 1:216], bench = AssDataBench[insample, 1:2],
                      pred.Y.OOS = AssDataY[outsample,1], comp.OOS = AssDatax[outsample, 1:216],
                      Bench.OOS = AssDataBench[outsample, 1:2], x.rank = AssDataRanks[insample3, 1:50],
                      rank.OOS = AssDataRanks[outsample3,1:50],
                      
                      train.size=1,
                      cores=cores, horizon.gap = horizon.gap, moving.average=c(),
                      model.select = c('comp','Bench','BInt', 'rank'), Progression=2)
return(Asslvl2)
}
data_comps <- function(){
  cores <- 8
  
  horizon.gap <- 12
  
  window.size <- 240
  
  end.dates <- "2019_DEC"
  
  step.estim.window <- 10000
  
  
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6xtra2.csv",quote = "\""))
  class(AssDatax) <- "numeric"
  AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\""))
  class(AssDataWeights) <- "numeric"
  AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
  class(AssDataBench) <- "numeric"
  AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
  AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
  class(AssDataRanks) <- "numeric"
  
  
  insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
  outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9-horizon.gap)
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
data_OOS <- function(meep){
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataY1.csv",quote = "\""))
  Daten <- AssDatax[outsample,2]
  Daten
  OOSDaten <- meep[["OOS"]][["Prediction"]]
  OOSDaten <- cbind(Daten, OOSDaten )
  OOSDaten <- as.data.table(OOSDaten)
  OOSDaten <- OOSDaten |>
    .d(, years := as.numeric(substr(Daten, 1, 4))) |>
    .d(, q := substr(Daten, 6, 8))|>
    .d(, t := ifelse(q == "JAN", 0,
                     ifelse(q == "FEB", 0.08,
                            ifelse(q == "MAR", 0.17, 
                                   ifelse(q == "APR", 0.25,
                                          ifelse(q == "MAY", 0.33,
                                                 ifelse(q == "JUN", 0.42, 
                                                        ifelse(q == "JUL", 0.5,
                                                               ifelse(q == "AUG", 0.58,
                                                                      ifelse(q == "SEP", 0.67, 
                                                                             ifelse(q == "OCT", 0.75,
                                                                                    ifelse(q == "NOV", 0.83, 0.92))))))))))))|>
    .d(, Daten := years + t + horizon.gap*(1/12))|>
    .d(, years := NULL)|>
    .d(, t := NULL)|>
    .d(, q := NULL)|>
    .d(, comp := as.numeric(Mod.comp))|>
    .d(, Mod.comp := NULL)|>
    .d(, targ := as.numeric(Target))|>
    .d(, Target := NULL)|>
    .d(, bench := as.numeric(Mod.Bench))|>
    .d(, Mod.Bench := NULL)|>
    .d(, bint := as.numeric(Mod.BInt))|>
    .d(, Mod.BInt := NULL)|>
    .d(, rank := as.numeric(Mod.rank))|>
    .d(, Mod.rank := NULL)
  return(OOSDaten)
}
data_insample <- function(meep){
  AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\""))
  Daten <- AssDatax[insample,51]
  print(Daten)
  ISDaten <- meep[["All.in.sample.info"]][["Prediction"]]
  ISDaten <- cbind(Daten, ISDaten )
  ISDaten <- as.data.table(ISDaten)
  ISDaten <- ISDaten |>
    .d(, years := as.numeric(substr(Daten, 1, 4))) |>
    .d(, q := substr(Daten, 6, 8))|>
    .d(, t := ifelse(q == "JAN", 0,
                     ifelse(q == "FEB", 0.08,
                            ifelse(q == "MAR", 0.17, 
                                   ifelse(q == "APR", 0.25,
                                          ifelse(q == "MAY", 0.33,
                                                 ifelse(q == "JUN", 0.42, 
                                                        ifelse(q == "JUL", 0.5,
                                                               ifelse(q == "AUG", 0.58,
                                                                      ifelse(q == "SEP", 0.67, 
                                                                             ifelse(q == "OCT", 0.75,
                                                                                    ifelse(q == "NOV", 0.83, 0.92))))))))))))|>
    .d(, Daten := years + t + horizon.gap*(1/12))|>
    .d(, years := NULL)|>
    .d(, t := NULL)|>
    .d(, q := NULL)|>
    .d(, comp := as.numeric(Mod.comp))|>
    .d(, Mod.comp := NULL)|>
    .d(, targ := as.numeric(Target))|>
    .d(, Target := NULL)|>
    .d(, bench := as.numeric(Mod.Bench))|>
    .d(, Mod.Bench := NULL)|>
    .d(, bint := as.numeric(Mod.BInt))|>
    .d(, Mod.BInt := NULL)|>
    .d(, rank := as.numeric(Mod.rank))|>
    .d(, Mod.rank := NULL)
    return(ISDaten)
}

ranks_all <- data_ranks()
comps_all <- data_comps()

ranks_insample <- data_insample(ranks_all)
comps_insample <- data_insample(comps_all)
ranks_OOS <- data_OOS(ranks_all)
comps_OOS <- data_OOS(comps_all)

dates_insample <- ranks_insample[["Daten"]]
target_insample <- ranks_insample[["targ"]]
target_OOS <- ranks_OOS[["targ"]]
ranks_insample <- ranks_insample[["rank"]]
dates_OOS <- ranks_OOS[["Daten"]]
ranks_OOS <- ranks_OOS[["rank"]]
comps_insample <- comps_insample[["comp"]]
comps_OOS <- comps_OOS[["comp"]]


dates <- c(c(dates_insample), c(dates_OOS))
ranks <- c(c(ranks_insample), c(ranks_OOS))
comps <- c(c(comps_insample), c(comps_OOS))
targ <- c(c(target_insample, c(target_OOS)))

combined_data <- data.table(Daten = dates, Ranks = ranks, Comps = comps, Target = targ)


ploten1 <- ggplot(combined_data, aes(Daten)) + 
  geom_line(aes(y = ranks, colour = "Ranks"), lwd = 0.75) +
  geom_line(aes(y = comps, colour = "Comps"), lwd = 0.75) +
  geom_line(aes(y = targ, colour = "Target"), lwd = 0.75) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  ##theme(
  ## plot.title = element_text(hjust = 0.5)  # Center the title
  ## ) +
  labs(color = "")

print(ploten1)
ggsave("D:/Daten/Plot_comps6_ranks3.pdf", height = 4, width = 12)


