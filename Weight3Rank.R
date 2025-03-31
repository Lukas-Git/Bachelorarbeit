library(data.table)
AssDatax <-fread("D:/Daten/asmblglvl2/AssDataxlvl3.csv",quote = "\"")
out <- c((18:20),23,24,41)
AssDatax <- AssDatax[, -out, with = FALSE]
data.table::fwrite(AssDatax, "D:/Daten/asmblglvl2/AssDataxlvl3_core.csv")
mat <- AssDatax[4:nrow(AssDatax)]
mat[, "Dates" := NULL]
mat <- as.matrix(mat)
sortieren <- function(com)
{
  x <- com
  x[order(com, decreasing = TRUE)]
  
}
sortieralles <- function(reg)
{
  i <- 1 
  while(i <= nrow(reg))
  {
    why <- sortieren(reg[i,])
    reg[i,] <- why
    i <- i+1
  }
  reg
}
mat_final <- sortieralles(mat)
mat_final <- as.data.table(mat_final)

data.table::fwrite(mat_final, "D:/Daten/ranks_lvl3_core.csv")



