require(PBSmodelling)
require(reshape2)
library(ggplot2)

create.annual.catch.summary <- function(year, side, wd=wd) {
  setwd(file.path(wd, "Syrah","outputFiles"))
  if(side == 'west') {
    n.districts <- 1
    n.stocks <- 3
    names.districts <- 'Nushagak'
    names.stocks <- c('Igushik', 'Wood', 'Nushagak')
    esc.dist <- c(325,325,325)
    esc.stream <- c(100,300,700)
  }
  if(side == 'east') {
    n.districts <- 3
    n.stocks <- 5
    names.districts <- c('Naknek-Kvichak','Egegik','Ugashik')
    names.stocks <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik')	
    esc.dist <- c(324,324,324,322,321)
    esc.stream <- c(100,500,600,100,100)
  }
  if(side != 'west' & side != 'east') { print('##### ERROR side selection is incorrect'); stop(); }
  
#  ac.data <- na.omit(read.csv(paste(wd, "/R/ageComp.annual.csv", sep=""), header=TRUE, stringsAsFactors=FALSE)[,-1])
  
  #Read the data
  if(side == 'west') {
    temp.data <- readList(paste('WestSide/WestSide_',year,'.out',sep='')) 
  }else {
    temp.data <- readList(paste('EastSide/EastSide_',year,'.out',sep=''))  
  }
   
  #Pertinent variables
  n.agecomps <- temp.data$nagecomps
  agecomps <- temp.data$AgeCompLabels
  
   temp.catch.s <- list() 
   catch.s <- list()
  ##### FILL IN THE ARRAY #####
  temp.catch <- temp.data$catchByGroup

  for(s in 1:n.stocks) {
    ref <- ((s-1)*n.agecomps+1):(s*n.agecomps) 
    if(side == 'west') {
	  temp.catch.s[[s]]<- temp.catch[ref]/sum(temp.catch[ref]) 
	  catch.s[[s]]<- temp.catch[ref]
    }else {
	  temp.catch.s[[s]] <- temp.catch[,ref]/rowSums(temp.catch[,ref]) 
	  catch.s[[s]]<- temp.catch[,ref]
    }
  }#next s
  age.stock <- function (listdata) {
  RR <- as.data.frame(do.call(rbind,listdata))
  names(RR) <-  c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4") 					 
  if(side == 'east') {				 
  RR$stock <- c(rep('Kvichak',3),rep('Alagnak',3),rep('Naknek',3),rep('Egegik',3),rep('Ugashik',3))
  RR$dist <- rep(c(324,322,321),5) 
   } else {
  RR$stock <- names.stocks 
  RR$dist <- 325 
   }  
  return(RR)
   }

   out <- list(
   RR = age.stock(temp.catch.s),
   predCatch.a.s = age.stock(catch.s),
   Catch = temp.data$obsCatch,
   obsCatchAge = temp.data$obsAgeCompCatch,
   predCatchAge = temp.data$predAgeCompCatch,   
   obsCatchGen = temp.data$obsGenComp,
   predCatchGen = temp.data$predGenComp   
   )
   return(out)
  }
  
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.6  reshapeWL: Reshape from wide to long  -----  
#  idvar: column names that is used for colun
#  timevar: new column name
#  v.names: new variable names
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
 reshapeWL <- function(df,idvar,timevar,v.names){  
      dfname <- names(df)[!(names(df)%in%idvar)]
  out <- reshape(df,direction='long',idvar=idvar,varying=dfname,
          timevar = timevar,v.names=v.names,times=dfname)
     return(out)}  
	 
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  1.7  reshapeLW: Reshape from long to wide -----  
#  idvar: column names that is used for colun
#  timevar: column name: that becomes wide 
#  v.names: variable names of the wide 
#'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 	 
reshapeLW <- function(df,idvar=idvar,v.names=v.names,timevar=timevar){
  dfs <- df[,c(idvar,timevar,v.names)]
   w <- reshape(dfs,direction='wide',timevar=timevar,v.names=v.names,idvar=idvar)
   wname <- names(w)[-c(1:length(idvar))]
   vhead <-paste0(v.names,'.')
   wname <- gsub(vhead,'',wname)
   names(w)[-c(1:length(idvar))] <- wname
   return(w)
}
  

test <- data.frame()
############## CONTROL SECTION ##############
#Set working directory
wd <- file.path("C:","Projects","Bristol_Bay","Bristol-Bay-Run-Reconstruction")
setwd(file.path(wd, "magma"))
year <- 2022
										
# Read Magma by district Stock Age    
	 magma.data <- readRDS(paste0("stock_age_",year,"_stats_dis.Rds"))     	 
	 magma <- data.frame(do.call(rbind,magma.data))
# Get number of age and stock group to creadte dist and Age 
	 ngroup <- length(unique(magma$group))
	 nage <- length(unique(magma$age))
	 magma$dist <- c(rep(321,ngroup),rep(322,ngroup),rep(324,ngroup),rep(325,ngroup),rep(326,ngroup))
	 magma$Age <- paste0(substr(magma$age,1,1),'.',substr(magma$age,2,2))
	 names(magma)[1] <- 'stock'
temp <- aggregate(mean~dist+stock,sum,data=magma)
magma <- merge(magma,temp,by=c('dist','stock'))
magma$p.age <- with(magma,mean.x/mean.y)

# Read Magma by district Stock 	){)} 
     magma.data2 <- readRDS(paste0("pop_",year,"_district.Rds")) 	 
	 magma2 <- as.data.frame(do.call(rbind,magma.data2))
#	 magma2$dist <- as.numeric(substr(rownames(magma2),7,9))	 
	 magma2$dist <- c(rep(321,ngroup),rep(322,ngroup),rep(324,ngroup),rep(325,ngroup),rep(326,ngroup))
	 names(magma2)[1] <- 'stock'
     temp <- aggregate(mean~dist,sum,data=magma2)
	magma2 <- merge(magma2,temp,by=c('dist'))
	magma2$p.stock <- with(magma2,mean.x/mean.y)
    temp <- reshapeLW(magma2,idvar=c('dist'),timevar='stock',v.names='p.stock')

temp$year <- year
#write.csv(temp, paste0('magma_gen_',year,'.csv'),row.names=FALSE)
# Read RR data 
west <- create.annual.catch.summary(year,'west',wd)
east <- create.annual.catch.summary(year,'east',wd)


# Combine Stock specific age comp and stock proportion
magma <- merge(magma[,c('dist','stock','Age','p.age')],magma2[,c('dist','stock','p.stock')],by=c('dist','stock'))
# MGpas: stock-specific age compostion at each district
# MGps:  stock compostion at each district
names(magma)[4:5] <- c('MGpas','MGps')
# MGps :  Stock by age proportion
magma$MGpsa <- with(magma,MGpas*MGps)
magma$TAge <- with(magma,as.numeric(substr(Age,1,1))+as.numeric(substr(Age,3,3))+1)
ob.Catch.d <- data.frame(dist = c(324,322, 321, 325), h = c(east$Catch, west$Catch))
# combine Catch 
MG.Catch <- merge(magma,ob.Catch.d,by='dist')
MG.Catch$MGas <- with(MG.Catch,MGpsa*h)
MG.Catch.as <- aggregate(MGas~dist+stock+TAge,sum,data=MG.Catch)

#RRpas: stock-specific age compostion at each district
RR <- reshapeWL(rbind(east$RR,west$RR),idvar = c('dist','stock'), timevar='Age',v.nam='RRpas')
RR.t <- merge(RR,magma,by=c('dist','stock','Age'),all=TRUE)
RR.t[is.na(RR.t)] <- 0
RR.catch.as <- reshapeWL(rbind(east$predCatch.a.s,west$predCatch.a.s),idvar = c('dist','stock'), timevar='Age',v.name='RRas')
RR.catch.as$TAge <- with(RR.catch.as,as.numeric(substr(Age,1,1))+as.numeric(substr(Age,3,3))+1)
RR.Catch.as <- aggregate(RRas~dist+stock+TAge,sum,data=RR.catch.as)
# Combine Catch by district 
Catch.as <- merge(MG.Catch.as, RR.Catch.as, by=c('dist','stock','TAge'),all=TRUE)
Catch.as[is.na(Catch.as)] <- 0
Catch.as$dist.name <- with(Catch.as,ifelse(dist==324,'NaknekKvichak',
    ifelse(dist==322,'Egegik',ifelse(dist==321,'Ugashik',
	ifelse(dist==325,'Nushagak',ifelse(dist==326,'Togiak','None'))))))

#templ <- reshapeWL(Catch.as,idvar = c('dist','dist.name','stock','TAge'), timevar='Model',v.name='n' )
#ggplot(templ, aes(x=TAge,y=n,fill=Model))+
#geom_bar(stat='identity',position=position_dodge())+
#labs(title = year)+
#facet_grid(dist.name~stock,axes='all',axis.labels='margins',scale='free_y') 


Catch.as.l <- reshapeWL(Catch.as,idvar = c('dist','dist.name','stock','TAge'), timevar='Model',v.name='n' )
Gen.catch <- aggregate(n~stock+TAge+Model,sum,data=Catch.as.l)
ggplot(Gen.catch, aes(x=TAge,y=n,fill=Model))+
geom_bar(stat='identity',position=position_dodge())+
labs(title = year)+
facet_wrap(~stock,axes='all',axis.labels='margins',scale='free_y') 





# Sum Genetic 
# Observed
ob.Catch.gen.e <- data.frame(east$obsCatchGen)
ob.Catch.gen.e$dist <- c(324,322,321)
names(ob.Catch.gen.e) <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik','dist')
ob.Catch.gen.e <- melt(ob.Catch.gen.e,variable.name='stock',id='dist',value.name='ob.ps')
ob.Catch.gen.w <- data.frame(ob.ps=west$obsCatchGen)
ob.Catch.gen.w$dist <- c(325)
ob.Catch.gen.w$stock <- c('Igushik', 'Wood', 'Nushagak')
ob.Catch.gen <- rbind(ob.Catch.gen.e,ob.Catch.gen.w[,c('dist','stock','ob.ps')])

# Predicted
pr.Catch.gen.e <- data.frame(east$predCatchGen)
pr.Catch.gen.e$dist <- c(324,322,321)
names(pr.Catch.gen.e) <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik','dist')
pr.Catch.gen.e <- melt(pr.Catch.gen.e,variable.name='stock',id='dist',value.name='pr.ps')
pr.Catch.gen.w <- data.frame(pr.ps=west$predCatchGen)
pr.Catch.gen.w$dist <- c(325)
pr.Catch.gen.w$stock <- c('Igushik', 'Wood', 'Nushagak')
pr.Catch.gen <- rbind(pr.Catch.gen.e,pr.Catch.gen.w[,c('dist','stock','pr.ps')])

# Combine observed and predicted 
ob.Catch.gen <- merge(ob.Catch.gen,pr.Catch.gen,by=c('dist','stock'))
# Combine with magma genetic stock data and extract only stocks used for RR
ob.Catch.gen <- merge(ob.Catch.gen, magma2[,c('dist','stock','mean')],by=c('dist','stock'))
# Standardize genetic data 
temp2 <-aggregate(mean~dist,FUN = sum,data =ob.Catch.gen)
ob.Catch.gen <- merge(ob.Catch.gen,temp2, by ='dist')
# m.ps is MAGMA derived standardized stock proportion 
ob.Catch.gen$m.ps <- with(ob.Catch.gen,mean.x/mean.y)
ob.Catch.gen <- ob.Catch.gen[,c('dist','stock','ob.ps','pr.ps','m.ps')]
# Rename 
names(ob.Catch.gen)[4] <- 'RRps'
ob.Catch.gen$difM <- with(ob.Catch.gen,abs(ob.ps-m.ps)) 
#aggregate(cbind(difM)~dist,FUN=sum, data=ob.Catch.gen)

# Sum Catch Age 
ob.Catch.age <- data.frame(rbind(east$obsCatchAge,west$obsCatchAge))
ob.Catch.age$dist <- c(324,322,321,325)
names(ob.Catch.age) <- c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4",'dist')
prd.Catch.age <- data.frame(rbind(east$predCatchAge,west$predCatchAge))
prd.Catch.age$dist <- c(324,322,321,325)
names(prd.Catch.age) <- c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4",'dist')
Catch.age.p <- melt(ob.Catch.age,variable.name='Age',id='dist',value.name='ob.pa')
Catch.age.pred <- melt(prd.Catch.age,variable.name='Age',id='dist',value.name='pr.pa')
Catch.age.p <- merge(Catch.age.p,Catch.age.pred, by =c('dist','Age'))
Catch.age.p$Age <- as.numeric(as.character(Catch.age.p$Age))
names(Catch.age.p)[4]  <- 'RRpa'

# Create district by Age to only observed age 
temp <- Catch.age.p[Catch.age.p$ob.pa>0,]
tempAge <- unique(temp$Age)
tempAge <- data.frame(Age=rep(tempAge[order(tempAge)],4),
			dist=c(rep(321,length(tempAge)),rep(322,length(tempAge)),rep(324,length(tempAge)),rep(325,length(tempAge))))
Catch.age.p <- merge(Catch.age.p,tempAge, by = c('dist','Age'),all.y=TRUE)


# merge limited age with run reconstruction 
RR.t <- merge(RR.t,tempAge,by=c('dist','Age'),all.y=TRUE)
# merge with genetic data 
RR.t <- merge(RR.t,ob.Catch.gen, by = c('dist','stock'),all=TRUE)
RR.t[is.na(RR.t)] <- 0
RR.t$dif <- with(RR.t,abs(RRpas-MGpas))
#aggregate(dif~stock+dist,FUN=sum, data=RR.t)
temp.magma <- aggregate(cbind(MGpsa)~dist+Age,FUN=sum, data=RR.t)
Catch.age <- merge(temp.magma, Catch.age.p,by=c('dist','Age'),all=TRUE)
names(Catch.age)[3] <- 'MGpa'
Catch.age$difR <- with(Catch.age,abs(RRpa-ob.pa))
Catch.age$difM <- with(Catch.age,abs(MGpa-ob.pa)) 
#aggregate(cbind(difR,difM)~dist,FUN=sum, data=Catch.age)
# Add catch 
RR.t <- merge(RR.t,ob.Catch.d, by = c('dist'))
RR.t <- merge(RR.t,RR.catch.as,by=c('dist','Age','stock'),all.x=TRUE)
RR.t$CMG <- with(RR.t,h*MGpsa)
RR.t$CMG2 <- with(RR.t,h*MGpas*m.ps)
RR.t$CRR <- with(RR.t,h*RRpas*RRps)
RR.t[is.na(RR.t)] <- 0
Cstock <- aggregate(cbind(CMG,CMG2,CRR,RRas)~Age+stock,FUN=sum, data=RR.t)
Cstock
aggregate(cbind(CMG,CMG2,CRR,RRas)~stock,FUN=sum, data=RR.t)
MGH <- merge(magma2,ob.Catch.d, by = c('dist'))
MGH$CMG <- with(MGH,mean*h)
aggregate(cbind(CMG)~stock,FUN=sum, data=MGH)


#windows(record=TRUE)

windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS"),
  D=windowsFont("Times New Roman")
)
########  Add Texts  #######################################################
windows(record=TRUE,width = 12, height = 9,family = 'D')

# Total Catch by age by stock 
stock <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik','Igushik', 'Wood', 'Nushagak','North Peninsula','Togiak','Kuskokwim')
par(mfrow=c(3,5),mar = c(3,2,2,2),oma = c(6,6,2,2)) 
for(i in 1:11){
barplot(t(Cstock[which(Cstock$stock==stock[i]),c('RRas','CMG')]),col=c(1,'white'),
beside=TRUE, names.arg =as.character(Cstock[which(Cstock$stock==stock[i]),c('Age')]),las = 3)
title(main = paste(stock[i]))
 }	 
plot.new()	 
legend('topleft',legend=c('RR','MAGMA'), fill= c(1,'white'),bty='n') 




par(mfrow=c(4,5),mar = c(2,1,1,1),oma = c(6,6,2,2)) 
dist_stock <- data.frame(dist=c(rep(324,5),rep(322,5),rep(321,5),rep(325,3)), dist.n=c(rep('Naknek-Kvichak',5),rep('Egegik',5),rep('Ugashik',5),rep('Nushagak',3)),
 stock= c(rep(c('Kvichak','Alagnak','Naknek','Egegik','Ugashik'),3),'Igushik', 'Wood', 'Nushagak'))
for(i in 1:18){
barplot(t(RR.t[which(RR.t$dist==dist_stock[i,1]&RR.t$stock==dist_stock[i,3]),c('RRpas','MGpas')]),col=c(1,'white'),
beside=TRUE,ylim=c(0,1.0), names.arg =tempAge$Age, yaxt='n',las = 3)
 if(i %in% c(1:5)) title(main = paste(dist_stock[i,3]))
 if(i %in% c(16:18)) title(main = paste(dist_stock[i,3]))
 if(i %in% c(1,6,11,16)) { 
      axis(2, seq(0,1.0,0.2),las=3, font=2)
 mtext(paste(dist_stock[i,2]),side=2,line =2.5)
     }
 
 if(i==1)legend('topleft',legend=c('RR','MAGMA'), fill= c(1,'white'),bty='n')  

}

########  Add Texts  #######################################################
mtext('Age propportion', side = 2, line = 4, outer = TRUE)
mtext("Scale Age", side = 1, line = 2, outer = TRUE,las=1)



#  Stock proportion by district 
dist <- c(324,322,321,325)
dist.name <- c('Naknek-Kvichak','Egegik','Ugashik','Nushagak')
par(mfrow=c(2,4),mar = c(2,1.5,1.5,2),oma = c(3,3,3,3))
for(i in 1:4){
temp1 <- t(ob.Catch.gen[which(ob.Catch.gen$dist==dist[i]),c('RRps','m.ps','ob.ps')])
if(i<4){
barplot(as.matrix(temp1),col=c(1,'white','Gray'),beside=TRUE,ylim=c(0,1.0),
 names.arg = c('Alagnak','Egegik','Kvichak','Naknek','Ugashik'),
 main = paste(dist.name[i])
   )} else {
barplot(as.matrix(temp1),col=c(1,'white','Gray'),beside=TRUE,ylim=c(0,1.0),
 names.arg = c('Igushik','Nushagak','Wood'),
 main = paste(dist.name[i])
   )   
   }
 if(i==1) legend('topleft',legend=c('RR','MAGMA','OBS'), fill= c(1,'white','Gray'),bty='n')  
  if(i==1) mtext('Stock Proportion',side=2,line =2.5) 
}

#  Age proportion by district 
for(i in 1:4){
temp1 <- t(Catch.age[which(Catch.age$dist==dist[i]),c('RRpa','MGpa','ob.pa')])
barplot(as.matrix(temp1),col=c(1,'white','Gray'),beside=TRUE,ylim=c(0,1.0),
 names.arg = unique(Catch.age$Age)
   )
 if(i==1) legend('topleft',legend=c('RR','MAGMA','OBS'), fill= c(1,'white','Gray'),bty='n') 
 if(i==1) mtext('Age Proportion',side=2,line =2.5) 
}


########  Add Texts  #######################################################
mtext(paste("Harvest by Dist,Age",year), side = 3, line = 0, outer = TRUE)
mtext('Harvest', side = 2, line = 1, outer = TRUE)
mtext("Age", side = 1, line = 1, outer = TRUE)


for(i in 1:4){
temp1 <- t(Catch.age[which(Catch.age$dist==dist[i]),c('RRpa','MGpa','ob.pa')])
barplot(as.matrix(temp1),col=c(1,'white','Gray'),beside=TRUE,ylim=c(0,1.0),
 names.arg = unique(Catch.age$Age)
   )
 if(i==1) legend('topleft',legend=c('RR','MAGMA','OBS'), fill= c(1,'white','Gray'),bty='n') 
 if(i==1) mtext('Age Proportion',side=2,line =2.5) 
}
#legend('topright',legend=c('RR','magma','obs'), fill= c(1,'white','Gray'),bty='n') 
########  Add Texts  #######################################################
mtext(paste("Harvest by Dist,Age",year), side = 3, line = 0, outer = TRUE)
mtext('Harvest', side = 2, line = 1, outer = TRUE)
mtext("Age", side = 1, line = 1, outer = TRUE)



  
#windows(record=TRUE)
########  Add Texts  #######################################################
par(mfrow=c(4,5),mar = c(2,2,3,2),oma = c(3,3,3,3)) 
for(i in 1:18){
 barplot(t(RR.t[which(RR.t$dist==dist_stock[i,1]&RR.t$stock==dist_stock[i,3]),c('Prop','mean')]),col=c(2,3),beside=TRUE,ylim=c(0,1),
 names.arg = c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4")
					 ,
 main = paste(dist_stock[i,1],dist_stock[i,2],'\n',dist_stock[i,3]),las=3
)   
}
legend('topright',legend=c('RR','magma'), fill= c(2,3),bty='n') 
########  Add Texts  #######################################################
mtext(paste("Harvest age prob by dist,stock",year), side = 3, line = 0, outer = TRUE,las=1)
mtext('Age prop', side = 2, line = 1, outer = TRUE)
mtext("Age", side = 1, line = 1, outer = TRUE,las=1)

temp.RR <- aggregate(c.RR~stock+Age,FUN=sum, data=RR.t)
temp.magma <- aggregate(c.magma~stock+Age,FUN=sum, data=RR.t)
Catch.age <- merge(temp.RR,temp.magma, by = c('stock','Age'),all=TRUE)
stock.t <- c('Kvichak','Alagnak','Naknek','Egegik','Ugashik','Igushik', 'Wood', 'Nushagak')
par(mfrow=c(2,4),mar = c(2,1.5,1.5,2),oma = c(3,3,3,3))
for(i in 1:8){
barplot(t(Catch.age[which(Catch.age$stock==stock.t[i]),3:4]),col=c(2,3),beside=TRUE,
 names.arg = c("0.1","0.2","0.3","0.4","0.5",
                     "1.1","1.2","1.3","1.4","1.5",
                     "2.1","2.2","2.3","2.4",
                     "3.1","3.2","3.3","3.4"),
 main = paste(stock.t[i]),las=3
   )
}
legend('topright',legend=c('RR','magma'), fill= c(2,3),bty='n') 
########  Add Texts  #######################################################
mtext(paste("Harvest by stock,Age",year), side = 3, line = 0, outer = TRUE)
mtext('Harvest', side = 2, line = 1, outer = TRUE)
mtext("Age", side = 1, line = 1, outer = TRUE)


