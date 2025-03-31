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
newweightscomplete <- newweightscomplete[, -out, with = FALSE]

data.table::fwrite(newweightscomplete, "D:/Daten/AssDataWeightslvl6_core.csv")
