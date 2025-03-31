library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

D <- fread("D:/Daten/AssDataWeightslvl3.csv",quote = "\"")

## 780 ist Enddatum, also 16*12 davor?? ==> geht bis September 23 ==> avg von Jan 2007 bis September 23 == 16*12+9 Monate ==> 

timeframe <- (780 - (16*12+9)):780

i <- 1
averages <- sapply(D[timeframe], mean)

# Eine neue Datentabelle mit den gleichen Dimensionen erstellen,
# gefüllt mit den Durchschnittswerten
filled_dt <- as.data.table(matrix(rep(averages, each = nrow(D)), ncol = ncol(D)))
setnames(filled_dt, names(D))

# Ausgabe der neuen Datentabelle
print(filled_dt)

data.table::fwrite(filled_dt, "D:/Daten/AssDataWeightslvl3_avg.csv") #speichern Datentabelle