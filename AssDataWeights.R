library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

t284 <- fread("D:/Daten/weight_avg_lvl2.csv",quote = "\"")
t284proxy <- fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\"")
print(t284)
i <- 1

while(i<=nrow(t284proxy))
{
  j <- 1
  while(j<= 15)
  {
    t284proxy[[i,j]] <- t284[[j,2]]
    j <- j+1
  }
  i <- i+1
}

data.table::fwrite(t284proxy, "D:/Daten/asmblglvl2/AssDataWeights.csv")


