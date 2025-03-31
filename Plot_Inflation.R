library(data.table)
library(ggplot2)
library(MetBrewer) 
library(RColorBrewer)
library(patchwork)
.d <- `[`
dat <- fread("E:/Daten/Table_2_4_6_Level 2 Data long v2.csv")
names(dat)
print(dat)
plot <- ggplot(dat, aes(x = timep, y = share, group = Item)) + 
  geom_line(aes(color = Item), lwd = 0.5) +
  scale_color_viridis_d() +
  xlab("Year (Data measured at Monthly Frequency)") +
  theme_minimal() +
  theme( plot.title = element_text(hjust = 0.5)) + 
  labs(color = "") + 
  ggtitle("Inflation")
#scale_y_discrete(guide = guide_axis(n.dodge = 30))

ggsave("E:/Daten/Weights_Plot.pdf", height = 7.5, width = 10)
dat1 <- dat[aggregate %in% c("Goods")]
dat2 <- dat[aggregate %in% c("Energy", "Food", "Shelter", "Other")]
dat3 <- dat[aggregate %in% c("Services")]

toplot <- function(dat){
  plotter <- ggplot(dat, aes(x = timep, y = share, group = Item)) + 
    geom_line(aes(color = Item), lwd = 0.5) +
    scale_color_viridis_d() +
    xlab("Year (Data measured at Monthly Frequency)") +
    theme_minimal() +
    theme( plot.title = element_text(hjust = 0.5)) + 
    labs(color = "")
  
  return(plotter)
  
  
}

plot1 <- toplot(dat1) +
  ggtitle("Weight Goods")

ggsave("E:/Daten/Weight_Goods_Plot.pdf", height = 7.5, width = 10)

plot2 <- toplot(dat2) +
  ggtitle("Weight Other")

ggsave("E:/Daten/Weight_Other_Plot.pdf", height = 7.5, width = 10)

plot3 <- toplot(dat3) +
  ggtitle("Weight Services")

ggsave("E:/Daten/Weight_Goods_Services.pdf", height = 7.5, width = 10)