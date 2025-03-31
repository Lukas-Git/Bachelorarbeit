library(here)
library(data.table)
.d <- `[`
dat <- fread("D:/Daten/Table284.csv")
read_data <- read.csv("D:/Daten/Table_2_8_4_Level 2 Data.csv")

BaseCol <- read_data[0, ]
j <- 1
cat("Total Rows:", nrow(read_data))
while( j <= nrow(read_data))
{
  nextCol <- read_data[j, ]
  thresh <- 5
  i <- 2
  ProxyCol <- nextCol
  while ( i <= ncol(read_data))
  {
    if(i >= thresh)
    {
      k <- i-3
      ProxyCol[i] = nextCol[i] / nextCol[k]
    }
    else
    {
      ProxyCol[i] = "No Value Obtainable"
    }
    i <- i + 1
  }
  if(j == 1)
  {	
    BaseCol <- ProxyCol
    print(1)
  } else
  {
    if(j == 2)
    {
      BaseCol_new <- data.frame(rbind(BaseCol, ProxyCol), stringsAsFactors = FALSE)
      BaseCol <- BaseCol_new
      print(2)
    }
    else	
    {
      BaseCol_new <- rbind(BaseCol, ProxyCol)
      BaseCol <- BaseCol_new
      print(3)
    }
  }
  j <- j+1
  print( j )
}
print(BaseCol)

head1 <- dat[1,]
head2 <- dat[2,]
head <- c(paste(head1, head2))
head[1] <- "Item"
names(dat) <- head
dat <- dat[-(1:2),]


jay <- 1
ei <- 2
while( jay <= nrow(dat))
{
  
  t <- 5
 ei <- 2
  while(ei <= ncol(dat))
  {
      if(ei >= t)
      {
        k <- ei - 3
       
        dat[jay, ei] <- BaseCol[jay, ei]
      }
      else
      {
        dat[jay, ei] <- "No Value Obtainable"
      }
      ei <- ei + 1
  }
  
  jay<-jay+1
}

dat1 <- dat |>
  .d(, aggregate := c(rep("Goods", 4), "Food", "Goods", "Energy", "Goods",
                      "Shelter", "Other", "Other", rep("Services", 4))) |>
  .d(order(aggregate))
dat1 <- melt(dat1, id.vars= c("Item", "aggregate"), variable.name = "year_mon", value.name = "Inflation")
dat1 <- dat1[(52:11820),Inflation := as.numeric(Inflation)]
dat1 <- dat1[,year := as.numeric(substr(year_mon, 1, 4))]
dat1 <- dat1[,m := substr(year_mon, 6, 9)]
dat1 <- dat1[, year_mon := NULL]
dat1 <- dat1[, mnum := ifelse(m == "JAN", 0, ifelse(m == "FEB", 0.08, ifelse(m == "MAR", 0.17, ifelse(m == "APR", 0.25, ifelse(m == "MAY", 0.33, ifelse(m == "JUN", 0.42, ifelse(m == "JUL", 0.5, ifelse(m == "AUG", 0.58, ifelse(m == "SEP", 0.67, ifelse(m == "OCT", 0.75, ifelse(m == "NOV", 0.83, 0.92)))))))))))]
dat1 <- dat1[, timep := year + mnum]
dat1 <- dat1 |> 
  .d(order(aggregate))
print(dat1)
write.csv(dat1, "D:/Daten/Table284_3m_long_v2.csv")
