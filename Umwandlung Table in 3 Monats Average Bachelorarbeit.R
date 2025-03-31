read_data <- read.csv("E:/Daten/Table_2_8_4_Level 2 Data.csv")
data <- data.frame(read.csv("E:/Daten/Table_2_8_4_Level 2 Data.csv"), stringsAsFactors = FALSE)
library(data.table)
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

long <- melt(setDT(BaseCol), id.vars = c("Usage"), variable.name = "Month")
print(long)

write.csv(long, "E:/Daten/Table_2_8_4_3MonthInflation_long.csv")

ProxyCol1 <- ProxyCol
install.packages("here")
