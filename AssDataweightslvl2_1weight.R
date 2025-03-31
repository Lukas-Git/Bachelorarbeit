library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

D <- fread("D:/Daten/asmblglvl2/AssDataWeights2.csv",quote = "\"")

timeframe <- (780 - (16*12+9)):780

i <- 1
averages <- D[780]

# Eine neue Datentabelle mit den gleichen Dimensionen erstellen,
# gefüllt mit den Durchschnittswerten
filled_dt <- as.data.table(matrix(rep(averages, each = nrow(D)), ncol = ncol(D)))
setnames(filled_dt, names(D))

# Ausgabe der neuen Datentabelle
print(filled_dt)

data.table::fwrite(filled_dt, "D:/Daten/AssDataWeightslvl2_1weight.csv") #speichern Datentabelle