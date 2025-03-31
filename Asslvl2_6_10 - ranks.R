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


cores <- 8

horizon.gap <- 6

window.size <- 240

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

AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
Daten <- AssDatax[outsample,16]
OOSDaten <- Asslvl2[["OOS"]][["Prediction"]]
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
          .d(, Daten := years + t)|>
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


ploten1 <- ggplot(OOSDaten, aes(Daten)) + 
          geom_line(aes(y = targ, colour = "targ"), lwd = 0.75) +
          geom_line(aes(y = comp, colour = "comp"), lwd = 0.75) +
          geom_line(aes(y = bench, colour = "bench"), lwd = 0.75) +
          geom_line(aes(y = bint, colour = "bint"), lwd = 0.75) +
          geom_line(aes(y = rank, colour = "rank"), lwd = 0.75) +
         theme_minimal() +
          scale_color_brewer() +
         ##theme(
         ## plot.title = element_text(hjust = 0.5)  # Center the title
         ## ) +
         labs(color = "")

print(ploten1)

AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\""))
Daten <- AssDatax[insample,16]
print(Daten)
ISDaten <- Asslvl2[["All.in.sample.info"]][["Prediction"]]
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
  .d(, Daten := years + t)|>
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

ploten2 <- ggplot(ISDaten, aes(Daten)) + 
  geom_line(aes(y = targ, colour = "targ"), lwd = 0.75) +
  geom_line(aes(y = comp, colour = "comp"), lwd = 0.75) +
  geom_line(aes(y = bench, colour = "bench"), lwd = 0.75) +
  geom_line(aes(y = bint, colour = "bint"), lwd = 0.75) +
  geom_line(aes(y = rank, colour = "rank"), lwd = 0.75) +
  theme_minimal() +
  scale_color_brewer() +
  ##theme(
  ## plot.title = element_text(hjust = 0.5)  # Center the title
  ## ) +
  labs(color = "")
print(ploten1)
print(ploten2)

  
print(Asslvl2)
print(Asslvl2[["OOS"]])
print(Asslvl2[["All.in.sample.info"]][["Prediction"]])
data.table::fwrite(ISDaten, "D:/Daten/asmblglvl2/Insample_solution_avgweights_1_10.csv")
data.table::fwrite(OOSDaten, "D:/Daten/asmblglvl2/Outofsample_solution_avgweights_1_10.csv")
saveRDS(Asslvl2, "D:/Daten/asmblglvl2/Asslvl2_1_10.rds")

ploten1 <- ploten1 +
  ggtitle("Plot Outofsample")
ggsave("D:/Daten/Outofsample_lvl2_1_10_Plot.pdf", height = 7.5, width = 10)

ploten2 <- ploten2 +
  ggtitle("Plot Insample")
ggsave("D:/Daten/Insample_lvl2_1_10_Plot.pdf", height = 7.5, width = 10)