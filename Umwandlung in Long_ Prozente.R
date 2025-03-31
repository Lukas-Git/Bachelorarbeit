library(here)
library(data.table)
.d <- `[`
dat <- fread("E:/Daten/Table_2_4_6_Level 2 Data.csv")

print(dat)

head1 <- dat[1,]
head2 <- dat[2,]
head <- c(paste(head1,head2))
head[1] <- "Item"
names(dat) <- head
dat <- dat[-(1:2),]

dat1 <- dat |>
      .d(, aggregate := c(rep("Goods", 4), "Food", "Goods", "Energy", "Goods",
                             "Shelter", "Other", "Other", rep("Services", 4))) |>
      .d(order(aggregate))
print(dat1)
dat1 <- melt(dat1, id.vars= c("Item", "aggregate"), variable.name = "year_mon", value.name = "expenditures")
dat1 <- dat1[,expenditures := as.numeric(expenditures)]
dat1 <- dat1[,year := as.numeric(substr(year_mon, 1, 4))]
dat1 <- dat1[,m := substr(year_mon, 6, 9)]
dat1 <- dat1[, year_mon := NULL]
dat1 <- dat1[, mnum := ifelse(m == "JAN", 0, ifelse(m == "FEB", 0.08, ifelse(m == "MAR", 0.17, ifelse(m == "APR", 0.25, ifelse(m == "MAY", 0.33, ifelse(m == "JUN", 0.42, ifelse(m == "JUL", 0.5, ifelse(m == "AUG", 0.58, ifelse(m == "SEP", 0.67, ifelse(m == "OCT", 0.75, ifelse(m == "NOV", 0.83, 0.92)))))))))))]
dat1 <- dat1[, timep := year + mnum]
sum1 <- rep(c(0,0), times = 1530)
print(dat1)
dat1 <- dat1 |> .d(order(timep))
i <- 1
j <- 1
v <- 0
t <- TRUE
k <- 0
while(i <= 3060)
{
  if(k == 0)
  {
     k <- dat1[i,timep]
  }
   if(k == dat1[i,timep])
   {
     v <- v + dat1[i,expenditures] 

   } else
   {
    
      sum1[(j:(i-1))] = v
     v <- dat1[i,expenditures]
    j <- i
    k <- dat1[i,timep]
   }
  if(i == 3060)
    {
    sum1[j:i] = v

   }
  i = i+1
}
dat1 <- dat1[, expsum := sum1]
dat1 <- dat1[, share := (expenditures / expsum) * 100]
dat1 <- dat1 |> 
        .d(order(aggregate))
print(dat1)
write.csv(dat1, "E:/Daten/Table_2_4_6_Level 2 Data long v2.csv")
