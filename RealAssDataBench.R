library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

benchlow <- fread("D:/Daten/asmblglvl2/AssDataBench.csv",quote = "\"")
trimmed <- fread("D:/Daten/Unbearbeitete Daten/FBRDAL.csv", quote = "\"")
dates <- benchlow[,Dates]
benchlow <- benchlow[, Dates := NULL]
benchlow <- benchlow*100/3
trimmed3m <- trimmed[,2]

i <- 1
while(i<=nrow(trimmed))
{
  if(i < 3)
  {
    trimmed3m[i] <- NA
  }
  else
  {
    trimmed3m[i] <- (((((trimmed[i,2]/100)+1)^(1/12))*(((trimmed[i-1,2]/100)+1)^(1/12))*(((trimmed[i-2,2]/100)+1)^(1/12)))-1)*100/3
  }
  i <- i+1
}
789-575
ct <- c()
i <- 1

while( i <= 789)
{
  if(i<= 219)
  {
    ct <- c(ct, NA)
  }
  else
  {
    ct <- c(ct, trimmed3m[i-217])
  }
  i <- i+1
}

banchnew <- benchlow |>
    .d(, trimmed := ct)



banchnew <- banchnew |>
          .d(,Dates := dates)

data.table::fwrite(banchnew, "D:/Daten/asmblglvl2/FinalAssDataBench.csv")



