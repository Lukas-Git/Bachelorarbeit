library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

t284 <- fread("D:/Daten/284_Table_Y.csv",quote = "\"")

cnames_year <- t284[1,]
cnames_quarter <- t284[2,]
cnames <- c(paste(cnames_year, cnames_quarter, sep = "_"))
cnames[2] <- "item"

names(t284) <- cnames
cnames <- cnames[-1]
##t284[namen = ,] <- cnames
t284 <- t284[-(1:2),]
##t284[, 1] <- NULL

t284new <- transpose(t284)
print(t284new)

rnames <- t284new[1, ]
t284new <- t284new[-1,]

names(t284new) <- c(paste(rnames, "", sep = ""))

t284proxy <- t284new
i <- 1

while(i<= nrow(t284new))
{
  if(i< 13)
  {
    t284new[i,] <- NA
  }
  else
  {
    j <- 1
    while(j <= ncol(t284new))
    {
      name <- names(t284[,..j])
      t284new[[i,j]] <- ((as.numeric(t284proxy[[i,j]])/ as.numeric(t284proxy[[(i-12),j]]))^(1/12) - 1)*100
      j <- j+1
    }
  }
  i <- i + 1
}
cnames[1] <- "1959_JAN"
t284new <- t284new |>
  .d(,Dates := cnames)
t284new1 <- t284new
t284new3 <- t284new
t284new6 <- t284new
t284new24 <- t284new
i <- 1
while(i <= nrow(t284new))
{
  if(i<= nrow(t284new) - 12)
  {
    t284new[[i,1]] <- t284new[[(i+12),1]]
  }
  else
  {
    t284new[[i,1]] <- NA
  }
  i <- i+1
}

print(t284new)

data.table::fwrite(t284new, "D:/Daten/asmblglvl2/FinalAssDataY12.csv")

i <- 1
while(i <= nrow(t284new1))
{
  if(i<= nrow(t284new1) - 1)
  {
    t284new1[[i,1]] <- t284new1[[(i+1),1]]
  }
  else
  {
    t284new1[[i,1]] <- NA
  }
  i <- i+1
}

print(t284new1)

data.table::fwrite(t284new1, "D:/Daten/asmblglvl2/FinalAssDataY1.csv")

i <- 1
while(i <= nrow(t284new3))
{
  if(i<= nrow(t284new3) - 3)
  {
    t284new3[[i,1]] <- t284new3[[(i+3),1]]
  }
  else
  {
    t284new3[[i,1]] <- NA
  }
  i <- i+1
}

print(t284new3)

data.table::fwrite(t284new3, "D:/Daten/asmblglvl2/FinalAssDataY3.csv")

i <- 1
while(i <= nrow(t284new6))
{
  if(i<= nrow(t284new6) - 6)
  {
    t284new6[[i,1]] <- t284new6[[(i+6),1]]
  }
  else
  {
    t284new6[[i,1]] <- NA
  }
  i <- i+1
}

print(t284new6)

data.table::fwrite(t284new6, "D:/Daten/asmblglvl2/FinalAssDataY6.csv")

i <- 1
while(i <= nrow(t284new24))
{
  if(i<= nrow(t284new24) - 24)
  {
    t284new24[[i,1]] <- t284new24[[(i+24),1]]
  }
  else
  {
    t284new24[[i,1]] <- NA
  }
  i <- i+1
}

print(t284new24)

data.table::fwrite(t284new24, "D:/Daten/asmblglvl2/FinalAssDataY24.csv")