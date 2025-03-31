library(data.table)
library(ggplot2)
library(MetBrewer) 
library(RColorBrewer)
library(patchwork)
.d <- `[`
dat <- fread("E:/Daten/Table284_3m_long_v2.csv")
names(dat)
print(dat)
dat[, Inflation := signif(as.numeric(Inflation)*100 - 100, digits = 4 )]

plot <- ggplot(dat, aes(x = timep, y = Inflation, group = Item)) + 
        geom_line(aes(color = Item), lwd = 0.5) +
        scale_color_viridis_d() +
        xlab("Year (Data measured at Monthly Frequency)") +
        theme_minimal() +
        theme( plot.title = element_text(hjust = 0.5)) + 
        labs(color = "") + 
        ggtitle("Inflation")
        #scale_y_discrete(guide = guide_axis(n.dodge = 30))
ggsave("E:/Daten/Inflation_Plot.pdf", height = 7.5, width = 10)
dat1 <- dat[aggregate %in% c("Goods")]
dat2 <- dat[aggregate %in% c("Energy", "Food", "Shelter", "Other")]
dat3 <- dat[aggregate %in% c("Services")]

toplot <- function(dat){
  plotter <- ggplot(dat, aes(x = timep, y = Inflation, group = Item)) + 
         geom_line(aes(color = Item), lwd = 0.5) +
          scale_color_viridis_d() +
          xlab("Year (Data measured at Monthly Frequency)") +
         theme_minimal() +
        theme( plot.title = element_text(hjust = 0.5)) + 
        labs(color = "")
  
  return(plotter)
  
  
}


plot1 <- toplot(dat1) +
          ggtitle("Inflation Goods")

ggsave("E:/Daten/Inflation_Goods_Plot.pdf", height = 7.5, width = 10)

plot2 <- ggplot(dat2, aes(x = timep, y = Inflation, group = Item)) + 
        geom_line(aes(color = Item), lwd = 0.5) +
        scale_color_viridis_d() +
        xlab("Year (Data measured at Monthly Frequency)") +
        theme_minimal() +
        theme( plot.title = element_text(hjust = 0.5)) + 
        labs(color = "") + 
        ggtitle("Inflation Other")

ggsave("E:/Daten/Inflation_Other_Plot.pdf", height = 7.5, width = 10)

plot3 <- ggplot(dat3, aes(x = timep, y = Inflation, group = Item)) + 
  geom_line(aes(color = Item), lwd = 0.5) +
  scale_color_viridis_d() +
  xlab("Year (Data measured at Monthly Frequency)") +
  theme_minimal() +
  theme( plot.title = element_text(hjust = 0.5)) + 
  labs(color = "") + 
  ggtitle("Inflation Services")

ggsave("E:/Daten/Inflation_Services_Plot.pdf", height = 7.5, width = 10)



