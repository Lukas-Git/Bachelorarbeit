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
newweightscomplete <- newweightscomplete[, -out, with = FALSE]


data.table::fwrite(newweightscomplete, "D:/Daten/AssDataWeightslvl3_core.csv")