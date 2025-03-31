rm(list = ls())

library(data.table)
library(glmnet)
library(pracma)
library(CVXR)
library(foreach)
library(doParallel)
library(stats)
library(methods)
library(Matrix)
library(iterators)
library(datasets)
library(base)
library(Assemblage)
library(ggplot2)
library(here)
library(MetBrewer)
library(RColorBrewer)
library(patchwork)
.d <- `[`

coef12 <- as.matrix(fread("D:/Daten/rankcoeff12.csv",quote = "\""))

df <- data.frame(Kategorie = c("Vehicles", "House.G.", "Rec.G.", "O. dur. G.","Food G.", "Clothing", "Energy", "O. ndur G.", "Shelter", "Health", "Transpo", "Rec. S.", "Food S.", "Fin. S.", "Other S."),
                 percent_Weights = c(coef12))
df$Kategorie <- factor(df$Kategorie, levels = c("Vehicles", "House.G.", "Rec.G.", "O. dur. G.","Food G.", "Clothing", "Energy", "O. ndur G.", "Shelter", "Health", "Transpo", "Rec. S.", "Food S.", "Fin. S.", "Other S."))

ggplot(df, aes(x = Kategorie, y = percent_Weights)) +
  geom_bar(stat = "identity") +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_brewer(palette = "Spectral")

ggsave("D:/Daten/coefflvl212.pdf", height = 4, width = 10)


