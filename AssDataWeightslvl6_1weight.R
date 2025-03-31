library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

D <- fread("D:/Daten/AssDataWeightslvl6.csv",quote = "\"")

timeframe <- (780 - (16*12+9)):780

last_row_values <- D[780]


filled_dt <- as.data.table(matrix(rep(unlist(last_row_values), each = nrow(D)), ncol = ncol(D)))
setnames(filled_dt, names(D))

print(filled_dt)

data.table::fwrite(filled_dt, "D:/Daten/AssDataWeightslvl6_1weight.csv") #speichern Datentabelle