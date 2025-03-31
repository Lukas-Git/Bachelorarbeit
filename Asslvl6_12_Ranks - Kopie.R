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

horizon.gap <- 12

window.size <- 240

end.dates <- "2019_DEC"

step.estim.window <- 100000


AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDataxlvl6xtra2.csv",quote = "\""))
class(AssDatax) <- "numeric"
AssDataWeights <- as.matrix(fread("D:/Daten/AssDataWeightslvl6_neu.csv",quote = "\""))
class(AssDataWeights) <- "numeric"
AssDataBench <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataBench.csv",quote = "\""))
class(AssDataBench) <- "numeric"
AssDataY <- as.matrix(fread("D:/Daten/asmblglvl2/FinalAssDataY12.csv",quote = "\""))
AssDataRanks <- as.matrix(fread("D:/Daten/ranks_lvl6.csv", quote = "\"" ))
class(AssDataRanks) <- "numeric"
# Funktion zum Ersetzen von NA mit dem Spaltenmittel (ab der dritten Zeile)
##replace_na_with_col_mean <- function(mat) {
  #for (j in seq_len(ncol(mat))) {
   # # Mittelwert der Spalte ab der dritten Zeile berechnen (NA ignorieren)
    #col_mean <- mean(mat[3:nrow(mat), j], na.rm = TRUE)
    
    # NA-Werte ab der dritten Zeile ersetzen
    #for (i in 3:nrow(mat)) {
 #     if (is.na(mat[i, j])) {
#        mat[i, j] <- col_mean
#      }
 #   }
  #}
 # return(mat)
#}

# NA-Werte ersetzen
#AssDatax <- replace_na_with_col_mean(AssDatax)



insample <- (which(AssDataY[,2]==end.dates)-window.size-horizon.gap):(which(AssDataY[,2]==end.dates)-horizon.gap)
outsample <- (which(AssDataY[,2]==end.dates)-horizon.gap+1):(nrow(AssDataY)-9)
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

# Daten einlesen
AssDatax <- as.matrix(fread("D:/Daten/asmblglvl2/AssDatax.csv", quote = "\""))

# Out-of-Sample Daten
Daten_OOS <- AssDatax[outsample, 16]
OOSDaten <- Asslvl2[["OOS"]][["Prediction"]]
OOSDaten <- as.data.table(cbind(Daten_OOS, OOSDaten))

# In-Sample Daten
Daten_IS <- AssDatax[(insample), 16]
ISDaten <- Asslvl2[["All.in.sample.info"]][["Prediction"]]
ISDaten <- as.data.table(cbind(Daten_IS, ISDaten))

# Funktion zur Datenbereinigung
bereinige_daten <- function(df, daten_col) {
  df[, years := as.numeric(substr(get(daten_col), 1, 4))]
  df[, q := substr(get(daten_col), 6, 8)]
  
  df[, t := fifelse(q == "JAN", 0,
                    fifelse(q == "FEB", 0.08,
                            fifelse(q == "MAR", 0.17, 
                                    fifelse(q == "APR", 0.25,
                                            fifelse(q == "MAY", 0.33,
                                                    fifelse(q == "JUN", 0.42, 
                                                            fifelse(q == "JUL", 0.5,
                                                                    fifelse(q == "AUG", 0.58,
                                                                            fifelse(q == "SEP", 0.67, 
                                                                                    fifelse(q == "OCT", 0.75,
                                                                                            fifelse(q == "NOV", 0.83, 
                                                                                                    0.92)))))))))))]  # Hier wurde die Schließung der Klammern korrigiert
  
  df[, Daten := years + t]
  df[, c("years", "t", "q") := NULL]  # Entfernt unnötige Spalten
  df[, comp := ((((as.numeric(Mod.comp)/100)+1)^12)-1)*100]
  df[, Mod.comp := NULL]
  df[, targ := ((((as.numeric(Target)/100)+1)^12)-1)*100]
  ##df[, comp := shift(comp,12,type = "lead")]
  df[, Target := NULL]
  df[, bench := as.numeric(Mod.Bench)]
  df[, Mod.Bench := NULL]
  df[, bint := as.numeric(Mod.BInt)]
  df[, Mod.BInt := NULL]
  df[, rank := ((((as.numeric(Mod.rank)/100)+1)^12)-1)*100]
  df[, Mod.rank := NULL]
  
  return(df)
}

# Daten bereinigen
OOSDaten <- bereinige_daten(OOSDaten, "Daten_OOS")
ISDaten <- bereinige_daten(ISDaten, "Daten_IS")

# Kombinieren der Daten
GesamtDaten <- rbindlist(list(ISDaten, OOSDaten), use.names = TRUE, fill = TRUE)

GesamtDaten[, targ := shift(targ,12,type = "lag")]
GesamtDaten[1:11, comp := NA] 

# Gesamtdaten plotten
ploten <- ggplot(GesamtDaten, aes(Daten)) + 
  geom_line(aes(y = targ, colour = "targ"), lwd = 0.75) +
  geom_line(aes(y = comp, colour = "comp"), lwd = 0.75) +
  #geom_line(aes(y = bench, colour = "bench"), lwd = 0.75) +
  #geom_line(aes(y = bint, colour = "bint"), lwd = 0.75) +
  #geom_line(aes(y = rank, colour = "rank"), lwd = 0.75) +
  theme_minimal() +
  scale_color_brewer(palette = "Pastel1") +
  labs(color = "")

print(ploten)

data.table::fwrite(Asslvl2[["All.in.sample.info"]][["Coefficients"]][["Mod.comp"]], "D:/Daten/rankcoeff12.csv")
data.table::fwrite(Asslvl2[["All.in.sample.info"]][["Coefficients"]][["Mod.rank"]], "D:/Daten/rankcoeff12_rank.csv")  
print(Asslvl2)
print(Asslvl2[["OOS"]])
print(Asslvl2[["All.in.sample.info"]][["Prediction"]])
data.table::fwrite(ISDaten, "D:/Daten/asmblglvl2/Insample_solution_avgweights_1_10.csv")
data.table::fwrite(OOSDaten, "D:/Daten/asmblglvl2/Outofsample_solution_avgweights_1_10.csv")
saveRDS(Asslvl2, "D:/Daten/asmblglvl2/Asslvl2_1_10.rds")

ploten1 <- ploten +
  ggtitle("Plot Albacore Comps")
ggsave("D:/Daten/SampleLvl6_12PlotComps.pdf", height = 4, width = 10)


