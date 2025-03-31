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
out <- c(5,7,12)
newweightscomplete <- newweightscomplete[, -out, with = FALSE]

data.table::fwrite(newweightscomplete"D:/Daten/AssDataWeightslvl2_core.csv)
