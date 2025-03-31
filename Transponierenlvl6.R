library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

t284 <- fread("D:/Daten/Table_244U_lvl_6.csv",quote = "\"")

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
  if(i< 4)
  {
    t284new[i,] <- NA
  }
  else
  {
    j <- 1
    while(j <= ncol(t284new))
    {
      name <- names(t284[,..j])
      t284new[[i,j]] <- ((as.numeric(t284proxy[[i,j]])/ as.numeric(t284proxy[[(i-3),j]])) - 1)*100/3
      j <- j+1
    }
  }
  i <- i + 1
}
cnames[1] <- "1959_JAN"
t284new <- t284new |>
  .d(,Dates := cnames)

t284new <- t284new[-(789:791),]
print(t284new)





data.table::fwrite(t284new, "D:/Daten/asmblglvl2/AssDataxlvl6old.csv")

