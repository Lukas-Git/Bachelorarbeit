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


coef24 <- as.matrix(fread("D:/Daten/rankcoeff624.csv",quote = "\""))
coef12 <- as.matrix(fread("D:/Daten/rankcoeff612.csv",quote = "\""))
coef6 <- as.matrix(fread("D:/Daten/rankcoeff66.csv",quote = "\""))
coef3 <- as.matrix(fread("D:/Daten/rankcoeff63.csv",quote = "\""))
coef1 <- as.matrix(fread("D:/Daten/rankcoeff61.csv",quote = "\""))

mvp <- 1:7
fdhe <- 8:17
rgv <- 18:31
odg <- 32:38
fbpo <- 39:60
cf <- 61:66
gaoeg <- 67:70
ong <- 71:91
hu <- 92:100
hc <- 101:107
ts <- 108:117
rs <- 118:133
fsa <- 134:140
fsi <- 141:152
os <- 153:185

getvalue <- function(r){
  ##a1 <- coef1[r]
  ##a2 <- coef3[r]
  ##a3 <- coef6[r]
  a4 <- coef12[r]
  ##a5 <- coef24[r]
  ##i1 <- sum(a1)
  ##i2 <- sum(a2)
  ##i3 <- sum(a3)
  i4 <- mean(a4)
  ##i5 <- sum(a5)
  ##avg <- mean(i1,i2,i3,i4,i5)
  return(i4)
}



mvp_value <- getvalue(mvp)*100
fdhe_value <- getvalue(fdhe)*100
rgv_value <- getvalue(rgv)*100
odg_value <- getvalue(odg)*100
fbpo_value <- getvalue(fbpo)*100
cf_value <- getvalue(cf)*100
gaoeg_value <- getvalue(gaoeg)*100
ong_value <- getvalue(ong)*100
hu_value <- getvalue(hu)*100
hc_value <- getvalue(hc)*100
ts_value <- getvalue(ts)*100
rs_value <- getvalue(rs)*100
fsa_value <- getvalue(fsa)*100
fsi_value <- getvalue(fsi)*100
os_value <- getvalue(os)*100

all_value <- sum(mvp_value, fdhe_value,rgv_value,odg_value,fbpo_value,cf_value,gaoeg_value,ong_value,hu_value,hc_value,ts_value,rs_value,fsa_value,fsi_value,os_value)

mvp_value <- (mvp_value/all_value)*100
fdhe_value <- (fdhe_value/all_value)*100
rgv_value <- (rgv_value/all_value)*100
odg_value <- (odg_value/all_value)*100
fbpo_value <- (fbpo_value/all_value)*100
cf_value <- (cf_value/all_value)*100
gaoeg_value <- (gaoeg_value/all_value)*100
ong_value <- (ong_value/all_value)*100
hu_value <- (hu_value/all_value)*100
hc_value <- (hc_value/all_value)*100
ts_value <- (ts_value/all_value)*100
rs_value <- (rs_value/all_value)*100
fsa_value <- (fsa_value/all_value)*100
fsi_value <- (fsi_value/all_value)*100
os_value <- (os_value/all_value)*100

df <- data.frame(Kategorie = c("Vehicles", "House.G.", "Rec.G.", "O. dur. G.","Food G.", "Clothing", "Energy", "O. ndur G.", "Shelter", "Health", "Transpo", "Rec. S.", "Food S.", "Fin. S.", "Other S."),
                 percent_Weights = c(mvp_value, fdhe_value,rgv_value,odg_value,fbpo_value,cf_value,gaoeg_value,ong_value,hu_value,hc_value,ts_value,rs_value,fsa_value,fsi_value,os_value))

df$Kategorie <- factor(df$Kategorie, levels = c("Vehicles", "House.G.", "Rec.G.", "O. dur. G.","Food G.", "Clothing", "Energy", "O. ndur G.", "Shelter", "Health", "Transpo", "Rec. S.", "Food S.", "Fin. S.", "Other S."))

ggplot(df, aes(x = Kategorie, y = percent_Weights)) +
  geom_bar(stat = "identity") +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_brewer(palette = "Spectral")

ggsave("D:/Daten/Coeff_lvl6_aggr_lvl2.pdf", height = 5, width = 8)