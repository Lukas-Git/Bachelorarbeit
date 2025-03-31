read_data <- read.csv("E:/Daten/Table_2_4_6_Level 2 Data.csv")
data <- data.frame(read.csv("E:/Daten/Table_2_8_4_Level 2 Data.csv"), stringsAsFactors = FALSE)
library(data.table)
j <- 1
i <- 2
while(i <= ncol(read_data))
{
	Coli <- read_data[i]
	val <- 0
	while( j <= nrow(read_data))
	{
		val <- val + read_data[j,i]
		j <- j + 1
	}
	j <- 1
	while( j <= nrow(read_data))
	{
		read_data[j,i] <- (read_data[j,i] / val) * 100
		j <- j + 1
	}
	j <- 1
	i <- i + 1
	print (i)
}
j <- 1
while( j <= nrow(read_data))
	{
		pl <- read_data[j,]
		pl <- pl[-1]
		print(paste("Maximum:", read_data[j,1], max(pl)))
		print(paste("Minimum:", read_data[j,1], min(pl)))
		j <- j+1
	}


print(read_data)
write.csv(long, "E:/Daten/deletable.csv")
