library(data.table)
library(ggplot2)
library(moments)  # Für Skewness und Kurtosis

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen
# CSV-Datei als Data Table öffnen
df<- fread("D:/Daten/asmblglvl2/AssDataY24.csv")
dates <- df[,2]
t284new <- df[,1]
i <- 1

while(i<= nrow(t284new))
{
  
  
    j <- 1
    while(j <= ncol(t284new))
    {
      name <- names(t284new[,..j])
      t284new[[i,j]] <- t284new[[i,j]]*100/3
      j <- j+1
    }
  i <- i + 1
}

t284 <- t284new |>
      .d(,Dates := dates)

data.table::fwrite(t284, "D:/Daten/asmblglvl2/FinalAssDataY24.csv")