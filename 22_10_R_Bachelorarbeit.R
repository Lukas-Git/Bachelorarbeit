read_data <- read.csv("D:/Daten/Table_2_8_4_Level 2 Data.csv")
data <- data.frame(read.csv("D:/Daten/Table_2_8_4_Level 2 Data.csv"), stringsAsFactors = FALSE)
library(data.table)
library(tidyr)
print(data)
i <- 2
cat("Total Columns: ", ncol(read_data))
cat("Total Rows:", nrow(read_data))
library(data.table)
newCol <- read_data[0, ]
print(newCol)

nextCol <- read_data[1, ]

print(nextCol)

Col2 <- read_data[2, ]
print(Col2)
Col2[5] <- 203
proxy <- data.frame(rbind(newCol, nextCol), stringsAsFactors = FALSE)
print(proxy)
rbind(proxy, YaCol = apply (proxy, 2, paste0, collapse = ", "))
Col <- proxy[2, ]

long <- melt(setDT(data), id.vars = c("Usage"), variable.name = "Month")
print(long)

write.csv(long, "E:/Daten/Table_2_8_4_long.csv")
