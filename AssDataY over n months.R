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
t284new1 <- t284new
t284new3 <- t284new
t284new6 <- t284new
t284new24 <- t284new
names(t284new) <- c(paste(rnames, "", sep = ""))


{
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
cnames[1] <- "1959_JAN"
t284new <- t284new |>
  .d(,Dates := cnames)
data.table::fwrite(t284new, "D:/Daten/asmblglvl2/FinalAssDataY12avgw.csv")
}
{
  t284proxy <- t284new1
  i <- 1
  while(i<= nrow(t284new1))
  {
    if(i< 2)
    {
      t284new[i,] <- NA
    }
    else
    {
      j <- 1
      while(j <= ncol(t284new1))
      {
        name <- names(t284[,..j])
        t284new1[[i,j]] <- ((as.numeric(t284proxy[[i,j]])/ as.numeric(t284proxy[[(i-1),j]]))^(1) - 1)*100
        j <- j+1
      }
    }
    i <- i + 1
  }
  i <- 1
  while(i <= nrow(t284new1))
  {
    if(i<= nrow(t284new1) - 1)
    {
      t284new1[[i,1]] <- t284new[[(i+1),1]]
    }
    else
    {
      t284new[[i,1]] <- NA
    }
    i <- i+1
  }
  cnames[1] <- "1959_JAN"
  t284new1 <- t284new1 |>
    .d(,Dates := cnames)
  data.table::fwrite(t284new, "D:/Daten/asmblglvl2/FinalAssDataY1avgw.csv")
}
{
  t284proxy <- t284new3
  i <- 1
  while(i<= nrow(t284new3))
  {
    if(i< 4)
    {
      t284new3[i,] <- NA
    }
    else
    {
      j <- 1
      while(j <= ncol(t284new3))
      {
        name <- names(t284[,..j])
        t284new3[[i,j]] <- ((as.numeric(t284proxy[[i,j]])/ as.numeric(t284proxy[[(i-3),j]]))^(1/3) - 1)*100
        j <- j+1
      }
    }
    i <- i + 1
  }
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
  cnames[1] <- "1959_JAN"
  t284new3 <- t284new3 |>
    .d(,Dates := cnames)
  data.table::fwrite(t284new, "D:/Daten/asmblglvl2/FinalAssDataY3avgw.csv")
}
{
  t284proxy <- t284new6
  i <- 1
  while(i<= nrow(t284new6))
  {
    if(i< 7)
    {
      t284new6[i,] <- NA
    }
    else
    {
      j <- 1
      while(j <= ncol(t284new6))
      {
        name <- names(t284[,..j])
        t284new6[[i,j]] <- ((as.numeric(t284proxy[[i,j]])/ as.numeric(t284proxy[[(i-6),j]]))^(1/6) - 1)*100
        j <- j+1
      }
    }
    i <- i + 1
  }
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
  cnames[1] <- "1959_JAN"
  t284new6 <- t284new6 |>
    .d(,Dates := cnames)
  data.table::fwrite(t284new, "D:/Daten/asmblglvl2/FinalAssDataY6avgw.csv")
}
{
  t284proxy <- t284new24
  i <- 1
  while(i<= nrow(t284new24))
  {
    if(i< 25)
    {
      t284new24[i,] <- NA
    }
    else
    {
      j <- 1
      while(j <= ncol(t284new24))
      {
        name <- names(t284[,..j])
        t284new24[[i,j]] <- ((as.numeric(t284proxy[[i,j]])/ as.numeric(t284proxy[[(i-24),j]]))^(1/24) - 1)*100
        j <- j+1
      }
    }
    i <- i + 1
  }
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
  cnames[1] <- "1959_JAN"
  t284new24 <- t284new24 |>
    .d(,Dates := cnames)
  data.table::fwrite(t284new, "D:/Daten/asmblglvl2/FinalAssDataY24avgw.csv")
}




