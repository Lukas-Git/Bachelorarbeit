library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

D <- fread("D:/Daten/weightslvl6.csv",quote = "\"")

Movepa <- c('DNDCRG', 'DNFCRG', 'IA001081', 'IA001083', 'DNETRG', 'DMARRG', 'DREERG', 'DUTNRG', 'DUTMRG', 'DTATRG', 'DPAARG')
Fuduho <- c('DFNRRG', 'DCLFRG', 'DFLRRG', 'DWCTRG', 'DMHARG', 'DSEARG', 'DCHNRG', 'DNECRG', 'DHDWRG', 'DLWNRG')
Regove <- c('DTVSRG', 'DOVARG', 'DAUDRG', 'DRTDRG', 'DRTDRG', 'DOVERG', 'DCAMRG', 'DCPPRG', 'DCPSRG', 'DOIPRG', 'DSPGRG', 'DMCYRG', 'DBCYRG', 'DBOARG','DAIRRG', 'DREVRG', 'DRBKRG', 'DMSCRG')
odogo <- c('DJLYRG', 'DWTCRG', 'DTMERG', 'DEYERG', 'DEBKRG', 'DLUGRG', 'DTCERG')
Fubeoco <- c('DGRARG', 'DBAKRG', 'DBEERG', 'DPORRG', 'DMEARG', 'DPOURG', 'DFISRG', 'DMILRG', 'DDAIRG', 'DGGSRG', 'DFATRG', 'DFRURG', 'DVEGRG', 'DPFVRG', 'DSWERG', 'DSWERG', 'DOFDRG', 'DCTMRG', 'DJNBRG', 'DLIQRG', 'DWINRG', 'DMLTRG', 'DFFDRG')
Clfo <- c('DWGCRG', 'DMBCRG', 'DCICRG', 'DCSMRG', 'DMICRG', 'DSHURG')
Gaot <- c('DGASRG', 'DLUBRG', 'DOILRG', 'DLPFRG')
Otngo <- c('DRXDRG', 'DNRDRG', 'DOMPRG', 'DDOLRG', 'DPRPRG', 'DFLORG', 'DFLMRG', 'DCLERG', 'DPAPRG', 'DLINRG', 'DSEWRG', 'DMHPRG', 'DOPHRG','DCOSRG', 'DEAPRG', 'DTOBRG', 'DMAGRG', 'DSTYRG', 'DARTRG', 'DARSRG', 'DREMRG')
Hau <- c('DTMHRG',  'IA000630', 'DOMHRG', 'DOSTRG', 'DFARRG', 'DGRHRG', 'DWSMRG', 'DREFRG', 'DELCRG', 'DGHERG')
Hc <- c('DPHYRG', 'DDENRG', 'DHHCRG', 'DMLBRG', 'DOMSRG', 'DOMORG',  'DFPHRG', 'DGVHRG',  'DFPNRG')
Ts <- c('DVMRRG', 'DALERG', 'DTLERG', 'DMVRRG', 'DPFTRG', 'DIRRRG', 'DIBURG', 'DTAXRG', 'DIMTRG', 'DORTRG', 'DAITRG', 'DWATRG')
Rs <- c('DMDFRG', 'DORSRG', 'DMOVRG', 'DLIGRG', 'DSPERG', 'DMUSRG', 'DCTVRG', 'DFDVRG', 'DPICRG', 'DAPIRG', 'IA000233', 'IA000232', 'DCASRG', 'DLOTRG', 'DPARRG', 'DVETRG', 'DHOLRG', 'DRRERG')
Foseac <- c('DAPMRG', 'DCFDRG', 'DMFDRG', 'DHOTRG', 'DSCHRG', 'DMSLRG', 'DOPMRG')
Fisei <- c('DIMCRG', 'DIMNRG', 'DPENRG', 'DFEERG', 'DMUTRG', 'DPMIRG', 'DTRURG', 'DLIFRG', 'DFIPRG', 'DFIBRG', 'DMINRG', 'DIINRG', 'DPWCRG', 'DTINRG', 'DDCTRG', 'DICTRG')
Os <- c('DLOCRG', 'DLDTRG', 'DCELRG', 'DPSTRG', 'DODSRG', 'DINTRG', 'DGEDRG', 'DPEDRG', 'DESCRG', 'DNSCRG', 'DVEDRG', 'DGALRG', 'DTAPRG', 'DGENRG', 'DTHERG', 'DUNSRG', 'DAXSRG', 'DFUNRG', 'DBBBRG', 'DMPCRG', 'DDRYRG', 'DLGRRG', 'DSCLRG', 'DCHCRG', 'DELDRG', 'DMENRG', 'DFAMRG', 'DVOCRG', 'DCFORG', 'DSIARG', 'DSADRG', 'DDMSRG', 'DMSERG', 'DFRERG', 'DERERG', 'DMHSRG', 'DAFTRG', 'DUSTRG', 'DUSSRG', 'DFTURG', 'DMEFRG', 'DEFSRG', 'DNPHRG2', 'DNPNRG3', 'DRELRG2', 'DGIVRG2')



Os[1]
a <- Os[1]
a
D[,DLOCRG]
D[,Os, with = FALSE]

together <- function(lvl2){
  D[,lvl2, with = FALSE]
}

sum_rows <- function(dt) {
  # Berechnet die Summe jeder Zeile und gibt sie als Vektor zurück
  row_sums <- rowSums(dt)
  return(row_sums)
}

getsum <- function(lvl){
  sum_rows(together(lvl))
}

getlast <- function(a){
  a <- a[190:204]
  a
}

newweights <- data.table(getsum(Movepa), getsum(Fuduho), getsum(Regove), getsum(odogo), getsum(Fubeoco), getsum(Clfo), getsum(Gaot), getsum(Otngo), getsum(Hau), getsum(Hc), getsum(Ts), getsum(Rs), getsum(Foseac), getsum(Fisei), getsum(Os) )

weights <- fread("D:/Daten/Table_2_4_6_Level 2 Data long v2.csv",quote = "\"")

mvp <- weights |>
  .d(Item %in% "Motor_vehicles_and_parts") |>
  .d(, "share") 
mvp <- mvp[190:204]

fadhe <- weights |>
  .d(Item %in% "Furnishings_and_durable_household_equipment")|>
  .d(, "share")
fadhe <- fadhe[190:204]


rgav <- weights |>
  .d(Item %in% "Recreational_goods_and_vehicles")|>
  .d(, "share")
rgav <- rgav[190:204]

odg <-weights |>
  .d(Item %in% "Other_durable_goods")|>
  .d(, "share")
odg <- odg[190:204]

fbp <- weights |>
  .d(Item %in% "Food_and_beverages_purchased_for_off-premises_consumption")|>
  .d(, "share")
fbp <- fbp[190:204]

caf <- weights |>
  .d(Item %in% "Clothing_and_footwear")|>
  .d(, "share")
caf <- caf[190:204]

gaoeg <- weights |>
  .d(Item %in% "Gasoline_and_other_energy_goods")|>
  .d(, "share")
gaoeg <- gaoeg[190:204]

ong <- weights |>
  .d(Item %in% "Other_nondurable_goods")|>
  .d(, "share")
ong <- ong[190:204]

hau <- weights |>
  .d(Item %in% "Housing_and_utilities")|>
  .d(, "share")
hau <- hau[190:204]

hc <- weights |>
  .d(Item %in% "Health_care")|>
  .d(, "share")
hc <- hc[190:204]

ts <- weights |>
  .d(Item %in% "Transportation_services")|>
  .d(, "share")
ts <- ts[190:204]

rs <- weights |>
  .d(Item %in% "Recreation_services")|>
  .d(, "share")
rs <- rs[190:204]

fsaa <- weights |>
  .d(Item %in% "Food_services_and_accommodations")|>
  .d(, "share")
fsaa <- fsaa[190:204]

fsai <- weights |>
  .d(Item %in% "Financial_services_and_insurance")|>
  .d(, "share")
fsai <- fsai[190:204]

os <- weights |>
  .d(Item %in% "Other_services")|>
  .d(, "share")
os<- os[190:204]

newweightsnew <- data.table(mvp, fadhe, rgav, odg, fbp, caf, gaoeg, ong, hau, hc, ts, rs, fsaa, fsai, os)
newweightsnew <- newweightsnew / 100
newweightscomplete <- data.table(rbind(newweights, newweightsnew, use.names = FALSE))
setnames(newweightscomplete, c("Motor_vehicles_and_parts", "Furnishings_and_durable_household_equipment", "Recreational_goods_and_vehicles", "Other_durable_goods", "Food_and_beverages_purchased_for_off-premises_consumption", "Clothing_and_footwear", "Gasoline_and_other_energy_goods", "Other_nondurable_goods", "Housing_and_utilities", "Health_care", "Transportation_services", "Recreation_services", "Food_services_and_accommodations", "Financial_services_and_insurance", "Other_services"))
t284proxy <- fread("D:/Daten/asmblglvl2/AssDatax.csv",quote = "\"")
row <- t284proxy[,Dates]
newweightscomplete <- data.table(newweightscomplete,row)
out <- c(5,7,13)
newweightscomplete[, (out) := 0]

data.table::fwrite(newweightscomplete, "D:/Daten/AssDataWeightslvl2_core2.csv")

library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

D <- fread("D:/Daten/weightslvl6.csv",quote = "\"")

nmv <- c('DNDCRG', 'DNFCRG', 'IA001081', 'IA001083')
npumv <- c('DNETRG', 'DMARRG', 'DREERG', 'DUTNRG', 'DUTMRG')
mvps <- c('DTATRG', 'DPAARG')
faf <- c('DFNRRG', 'DCLFRG', 'DFLRRG', 'DWCTRG')
ha <- c('DMHARG', 'DSEARG')
gthu <- c('DCHNRG', 'DNECRG')
tehg <- c('DHDWRG', 'DLWNRG')
vapi <- c('DTVSRG', 'DOVARG', 'DAUDRG', 'DRTDRG', 'DRTDRG', 'DOVERG', 'DCAMRG', 'DCPPRG', 'DCPSRG', 'DOIPRG')
sesga <- c('DSPGRG')
srv <- c( 'DMCYRG', 'DBCYRG', 'DBOARG','DAIRRG', 'DREVRG')
rb <- c('DRBKRG')
mi <- c('DMSCRG')
jw <- c('DJLYRG', 'DWTCRG')
tae <- c('DTMERG', 'DEYERG')
eb <- c('DEBKRG')
lspi <- c('DLUGRG')
trce <- c('DTCERG')
fnbfo <- c('DGRARG', 'DBAKRG', 'DBEERG', 'DPORRG', 'DMEARG', 'DPOURG', 'DFISRG', 'DMILRG', 'DDAIRG', 'DGGSRG', 'DFATRG', 'DFRURG', 'DVEGRG', 'DPFVRG', 'DSWERG', 'DSWERG', 'DOFDRG', 'DCTMRG', 'DJNBRG')
abpoc <- c('DLIQRG', 'DWINRG', 'DMLTRG')
fpacof <- c('DFFDRG')
ga <- c('DWGCRG', 'DMBCRG', 'DCICRG')
ocmf <- c('DCSMRG', 'DMICRG', 'DSHURG')
mvflf <- c('DGASRG', 'DLUBRG')
foof <- c('DOILRG', 'DLPFRG')
paomp <- c('DRXDRG', 'DNRDRG', 'DOMPRG')
ri <- c('DDOLRG', 'DPRPRG', 'DFLORG', 'DFLMRG' )
hs <- c('DCLERG', 'DPAPRG', 'DLINRG', 'DSEWRG', 'DMHPRG' )
pcp <- c('DOPHRG', 'DCOSRG', 'DEAPRG' )
to <- c('DTOBRG')
mns <- c('DMAGRG', 'DSTYRG')
hou <- c('DTMHRG', 'IA000630', 'DOMHRG', 'DOSTRG', 'DFARRG', 'DGRHRG' )
hu <- c('DWSMRG', 'DREFRG', 'DELCRG', 'DGHERG')
os <- c('DPHYRG', 'DDENRG', 'DHHCRG', 'DMLBRG', 'DOMSRG', 'DOMORG')
hnhs <- c( 'DFPHRG', 'DGVHRG',  'DFPNRG', 'DNPNRG3')
mvs <- c('DVMRRG', 'DALERG', 'DTLERG', 'DMVRRG', 'DPFTRG')
pt <- c('DIRRRG', 'DIBURG', 'DTAXRG', 'DIMTRG', 'DORTRG', 'DAITRG', 'DWATRG')
mcscptm <- c('DMDFRG', 'DORSRG', 'DMOVRG', 'DLIGRG', 'DSPERG', 'DMUSRG')
apipes <- c('DCTVRG', 'DFDVRG', 'DPICRG', 'DAPIRG', 'IA000233', 'IA000232')
ga <- c('DCASRG', 'DLOTRG', 'DPARRG')
ors <- c('DVETRG', 'DHOLRG', 'DRRERG')
fs <- c('DMSLRG', 'DOPMRG', 'DAPMRG', 'DCFDRG', 'DMFDRG')
ac <- c('DHOTRG', 'DSCHRG')
fsi <- c('DIMCRG', 'DIMNRG', 'DPENRG', 'DFEERG', 'DMUTRG', 'DPMIRG', 'DTRURG', 'DDCTRG', 'DICTRG')
ins <- c('DLIFRG', 'DFIPRG', 'DFIBRG', 'DMINRG', 'DIINRG', 'DPWCRG', 'DTINRG')
co <- c('DLOCRG', 'DLDTRG', 'DCELRG', 'DPSTRG', 'DODSRG', 'DINTRG')
es <- c('DGEDRG', 'DPEDRG', 'DESCRG', 'DNSCRG', 'DVEDRG')
pos <- c('DGALRG', 'DTAPRG', 'DGENRG', 'DTHERG', 'DUNSRG', 'DAXSRG', 'DFUNRG')
pccs <- c('DBBBRG', 'DMPCRG', 'DDRYRG', 'DLGRRG', 'DSCLRG')
ssra <- c('DRELRG2', 'DGIVRG2', 'DCHCRG', 'DELDRG', 'DMENRG', 'DFAMRG', 'DVOCRG', 'DCFORG', 'DSIARG', 'DSADRG')
hm <- c('DDMSRG', 'DMSERG', 'DFRERG', 'DERERG', 'DMHSRG')







together <- function(lvl2){
  D[,lvl2, with = FALSE]
}

sum_rows <- function(dt) {
  # Berechnet die Summe jeder Zeile und gibt sie als Vektor zurück
  row_sums <- rowSums(dt)
  return(row_sums)
}

getsum <- function(lvl){
  sum_rows(together(lvl))
}

getlast <- function(a){
  a <- a[190:204]
  a
}

newweights <- data.table(getsum(nmv), getsum(npumv), 
                         getsum(mvps), getsum(faf), 
                         getsum(ha), getsum(gthu), 
                         getsum(tehg), getsum(vapi), 
                         getsum(sesga), getsum(srv), 
                         getsum(rb), getsum(mi), 
                         getsum(jw), getsum(tae), 
                         getsum(eb), getsum(lspi),
                         getsum(trce), getsum(fnbfo),
                         getsum(abpoc), getsum(fpacof),
                         getsum(ga), getsum(ocmf),
                         getsum(mvflf), getsum(foof),
                         getsum(paomp), getsum(ri),
                         getsum(hs), getsum(pcp),
                         getsum(to), getsum(mns),
                         getsum(hou), getsum(hu),
                         getsum(os), getsum(hnhs),
                         getsum(mvs), getsum(pt),
                         getsum(mcscptm), getsum(apipes),
                         getsum(ga), getsum(ors),
                         getsum(fs), getsum(ac),
                         getsum(fsi), getsum(ins),
                         getsum(co), getsum(es),
                         getsum(pos), getsum(pccs),
                         getsum(ssra), getsum(hm))
newweights

weights <- fread("D:/Daten/Table246lvl3.csv",quote = "\"")
cnames_year <- weights[1,]
cnames_quarter <- weights[2,]
cnames <- c(paste(cnames_year, cnames_quarter, sep = "_"))
cnames[2] <- "item" 
names(weights) <- cnames
cnames <- cnames[-1]
##t284[namen = ,] <- cnames
weights <- weights[-(1:2),]

weightst <- transpose(weights)


rnames <- weightst[1, ]

weightst <- weightst[-1,] 



names(weightst) <- c(paste(rnames, "", sep = ""))


print(weightst)

weightsp <- weightst

i = 1

while(i <= nrow(weightst))
{
  print(i)
  proxy <- weightst[i,]
  print(proxy)
  j <- 1
  summe <- 0
  while(j <= ncol(proxy))
  {
    summe <- summe + as.numeric(proxy[[1,j]])
    print(summe)
    j<- j+1
  }
  
  proxy <- as.numeric(proxy)/summe
  
  j <- 1
  while(j<= length(proxy))
  {
    weightst[i,j] <- proxy[j]
    j  <- j+1
  }
  
  i <- i + 1
}
weightst <- weightst[190:204] 
newweightscomplete <- data.table(rbind(newweights, weightst, use.names = FALSE))
newweightscomplete
out <- c((18:20),23,24,41)
newweightscomplete[, (out) := 0]


data.table::fwrite(newweightscomplete, "D:/Daten/AssDataWeightslvl3_core2.csv")

library(data.table)

.d <- `[`##Um tunnel Operatoren einfacher anwendbar zu machen

D <- fread("D:/Daten/weightslvl6.csv",quote = "\"")

nmv <- c('DNDCRG', 'DNFCRG', 'IA001081', 'IA001083','DNETRG', 'DMARRG', 'DREERG', 'DUTNRG','DUTMRG','DTATRG', 
         'DPAARG', 'DFNRRG', 'DCLFRG', 'DFLRRG', 'DWCTRG', 'DMHARG', 'DSEARG', 'DCHNRG', 'DNECRG', 'DHDWRG', 
         'DLWNRG', 'DTVSRG', 'DOVARG', 'DAUDRG', 'DRTDRG', 'DOVERG', 'DCAMRG', 'DCPPRG', 'DCPSRG',   
         'DOIPRG', 'DSPGRG', 'DMCYRG', 'DBCYRG', 'DBOARG','DAIRRG', 'DREVRG', 'DRBKRG', 'DMSCRG', 'DJLYRG', 
         'DWTCRG', 'DTMERG', 'DEYERG', 'DEBKRG', 'DLUGRG', 'DTCERG', 'DGRARG', 'DBAKRG', 'DBEERG', 'DPORRG', 
         'DMEARG', 'DPOURG', 'DFISRG', 'DMILRG', 'DDAIRG', 'DGGSRG', 'DFATRG', 'DFRURG', 'DVEGRG', 'DPFVRG', 
         'DSWERG', 'DOFDRG', 'DCTMRG', 'DJNBRG', 'DLIQRG', 'DWINRG', 'DMLTRG', 'DFFDRG', 'DWGCRG', 
         'DMBCRG', 'DCICRG', 'DCSMRG', 'DMICRG', 'DSHURG', 'DGASRG', 'DLUBRG', 'DOILRG', 'DLPFRG', 'DRXDRG', 
         'DNRDRG', 'DOMPRG', 'DDOLRG', 'DPRPRG', 'DFLORG', 'DFLMRG', 'DCLERG', 'DPAPRG', 'DLINRG', 'DSEWRG', 
         'DMHPRG', 'DOPHRG', 'DCOSRG', 'DEAPRG', 'DTOBRG', 'DMAGRG', 'DSTYRG', 'DARTRG', 'DARSRG', 'DREMRG', 'DTMHRG', 'IA000630', 'DOMHRG', 
         'DOSTRG', 'DFARRG', 'DGRHRG', 'DWSMRG', 'DREFRG', 'DELCRG', 'DGHERG', 'DPHYRG', 'DDENRG', 'DHHCRG', 'DMLBRG', 'DOMSRG', 'DOMORG',
         'DNPHRG2', 'DFPHRG', 'DGVHRG', 'DNPNRG3','DFPNRG', 'DVMRRG', 'DALERG', 'DTLERG', 
         'DMVRRG', 'DPFTRG', 'DIRRRG', 'DIBURG', 'DTAXRG', 'DIMTRG', 'DORTRG', 'DAITRG', 'DWATRG', 'DMDFRG', 
         'DORSRG', 'DMOVRG', 'DLIGRG', 'DSPERG', 'DMUSRG', 'DCTVRG', 'DFDVRG', 'DPICRG', 'DAPIRG', 'IA000233', 'IA000232',
         'DCASRG', 'DLOTRG', 'DPARRG', 'DVETRG', 'DHOLRG', 'DRRERG', 'DMSLRG', 'DOPMRG', 'DAPMRG', 
         'DCFDRG', 'DMFDRG', 'DHOTRG', 'DSCHRG', 'DIMCRG', 'DIMNRG', 'DPENRG', 'DFEERG', 'DDCTRG', 'DICTRG', 'DMUTRG', 'DPMIRG',
         'DTRURG', 'DLIFRG', 'DFIPRG', 'DFIBRG', 'DMINRG', 'DIINRG', 'DPWCRG', 'DTINRG', 
         'DLOCRG', 'DLDTRG', 'DCELRG', 'DPSTRG', 'DODSRG', 'DINTRG', 'DGEDRG', 'DPEDRG', 'DESCRG', 'DNSCRG', 
         'DVEDRG', 'DGALRG', 'DTAPRG', 'DGENRG', 'DTHERG', 'DUNSRG', 'DAXSRG', 'DFUNRG', 'DBBBRG', 'DMPCRG', 
         'DDRYRG', 'DLGRRG', 'DSCLRG', 'DCHCRG', 'DELDRG', 'DMENRG', 'DFAMRG', 'DVOCRG', 'DCFORG', 'DSIARG', 'DSADRG', 'DRELRG2', 'DGIVRG2',   
         'DDMSRG', 'DMSERG', 'DFRERG', 'DERERG', 'DMHSRG', 'DAFTRG', 'DUSTRG', 'DUSSRG', 'DFTURG','DMEFRG', 'DEFSRG')







together <- function(lvl2){
  D[,lvl2, with = FALSE]
}

sum_rows <- function(dt) {
  # Berechnet die Summe jeder Zeile und gibt sie als Vektor zurück
  row_sums <- rowSums(dt)
  return(row_sums)
}

getsum <- function(lvl){
  sum_rows(together(lvl))
}

getlast <- function(a){
  a <- a[190:204]
  a
}

newweights <- data.table(together(nmv))
newweights

weights <- fread("D:/Daten/Table_246_lvl6_neu.csv",quote = "\"")
weights <- weights[,-1]
data <- weights
fill_value <- 1 / 216
for (col in names(data)) {
  set(data, i = which(is.na(data[[col]])), j = col, value = fill_value)
}
weights <- data
cnames_year <- weights[1,]
cnames_quarter <- weights[2,]
cnames <- c(paste(cnames_year, cnames_quarter, sep = "_"))
cnames[2] <- "item" 
names(weights) <- cnames
cnames <- cnames[-1]
##t284[namen = ,] <- cnames
weights <- weights[-(1:2),]

weightst <- transpose(weights)


rnames <- weightst[1, ]

weightst <- weightst[-1,] 



names(weightst) <- c(paste(rnames, "", sep = ""))


print(weightst)

weightsp <- weightst

i = 1

while(i <= nrow(weightst))
{
  print(i)
  proxy <- weightst[i,]
  print(proxy)
  j <- 1
  summe <- 0
  while(j <= ncol(proxy))
  {
    summe <- summe + as.numeric(proxy[[1,j]])
    print(summe)
    j<- j+1
  }
  
  proxy <- as.numeric(proxy)/summe
  
  j <- 1
  while(j<= length(proxy))
  {
    weightst[i,j] <- proxy[j]
    j  <- j+1
  }
  
  i <- i + 1
}
weightst <- weightst[190:204] 
newweightscomplete <- data.table(rbind(newweights, weightst, use.names = FALSE))
newweightscomplete
out <- c((46:67),(74:78),(150:155))
newweightscomplete[, (out) := 0]

data.table::fwrite(newweightscomplete, "D:/Daten/AssDataWeightslvl6_core2.csv")