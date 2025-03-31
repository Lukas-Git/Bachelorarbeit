library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

t284 <- fread("D:/Daten/Table_244U_lvl_6_neu.csv",quote = "\"")
t284 <- t284[,-1]

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
  if(i< 6)
  {
    t284new[i,] <- NA
  }
  else
  {
    j <- 1
    while(j <= ncol(t284new))
    {
      name <- names(t284[,..j])
      t284new[[i,j]] <- ((((as.numeric(t284proxy[[i,j]]))/ (as.numeric(t284proxy[[i-3,j]])) + (as.numeric(t284proxy[[i-1,j]]))/ (as.numeric(t284proxy[[i-4,j]]))+(as.numeric(t284proxy[[i-2,j]]))/ (as.numeric(t284proxy[[i-5,j]])))/3)^(1/3)  - 1)*100
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


# Funktion zum Ersetzen von NA mit dem Spaltenmittel (ab der dritten Zeile)
replace_na_and_non_numeric_with_col_mean_dt <- function(dt) {
  for (col in names(dt)) {
    # Ganze Spalte in numerisch umwandeln, nicht umwandelbare Werte werden zu NA
    dt[, (col) := as.numeric(get(col))]
    
    # Mittelwert der Spalte ab der dritten Zeile berechnen (NA ignorieren)
    col_mean <- mean(dt[4:.N, get(col)], na.rm = TRUE)
    
    # NA-Werte ab der dritten Zeile ersetzen
    dt[4:.N, (col) := fifelse(is.na(get(col)), col_mean, get(col))]
  }
  return(dt)
}

t284new <- replace_na_and_non_numeric_with_col_mean_dt(t284new)

data.table::fwrite(t284new, "D:/Daten/asmblglvl2/AssDataxlvl6_avgi.csv")