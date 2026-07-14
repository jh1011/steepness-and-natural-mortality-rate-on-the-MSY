#demo for JH as of 13 June 2023
library(stringr);
library(PBSadmb);

shell("dir *")

exe.file=c("asap3mod11");   #JH.exe;
sampledata.DAT= read.csv(file="logis.csv", header=FALSE); 
fn0 = as.matrix(sampledata.DAT); #we have to make fn0 a matrix;

#
m=c(seq(0.1,1.0,by=0.05),seq(0.01,0.09,by=0.01))
m=seq(0.01,0.09, by=0.01)
m=seq(1.1,2.0,by=0.01)
#m=seq(0.1,1.0,by=0.1)
m=c(0.07,0.08,0.15)
h=c(0.3,0.4,1.0)
h=seq(0.3,1.0, by=0.05)
h=seq(0.2,0.3,by=0.01)
h=seq(0.25,1.0,by=0.05)
h=0.55
mh=expand.grid(m,h)

nn <- dim(mh)[1];
mh[57,]

dim(fn0)
#dim(mh)[3];

###345
yr=fn0[2,1]
age=fn0[4,1]

###lambda for index  line 606
li = rep(2,2)
lii = c(li,rep(-999,dim(fn0)[2]-length(li)))
####lambda for catch line 607
lc = 1.5
lcc = c(lc,rep(-999,dim(fn0)[2]-1))
#####effective sample size line 697
ess = matrix(data=100,nrow=yr,ncol=1)
ls = matrix(data=-999,nrow=yr,ncol=9)
esss = cbind(ess,ls)
####lambda for recruit line 791
lr = 0.5
lrr = c(lr,rep(-999,dim(fn0)[2]-1))
####lambda for N1
N1 = 1.2
N11 = c(N1,rep(-999,dim(fn0)[2]-1))



#fn0[2,]= rnorm(10,mean=3, sd=1);
#fn0[3,]= rlnorm(10, meanlog=log(10), sdlog=log(1.5));
rm(summ.save.save)
rm(summ.save)
rm(summ.save.save.save)
rm(summ.save.save.save.save)
rm(summ.save.save.save.save.save)
summ.save=list();
summ.save.save=list();
summ.save.save.save=list()
summ.save.save.save.save=list()
summ.save.save.save.save.save=list()
summ.save.save.save.save.save.save=list()
summ.save.save.save.save.save.save.save=list()


for (i in 1:nn) {
  #for (i in 79:nn) {
  #for (i in 51:nn) {
  #for (i in 64:nn) {
  #for (i in 63:nn) {
  #i=302
  #i=1
  #i=47
  #i=93
  #i=139
  #i=185
  #i=231
  #i=277
  #making M matrix
  M=matrix(data=mh[i,1],nrow=yr,ncol=age)
  ff=matrix(data=-999,nrow=yr,ncol=dim(fn0)[2]-age)
  Mff=cbind(M,ff)
  #making h vector
  hh=c(mh[i,2],rep(-999,dim(fn0)[2]-1))
  
  #hh=c(0.2,rep(-999,9))
  #making sample size
  #j=1
  #eff=matrix(data=25, nrow=21,ncol=j)
  #com=matrix(data=-999, nrow=21, ncol=dim(fn0)[2]-j)
  #ef=cbind(eff,com)
  #making lambda for index
  #li=c(1.5,rep(-999,dim(fn0)[2]-1))
  #making lambda for catch
  #lc=c(3.5,rep(-999,dim(fn0)[2]-1))
  #making lambda for recruit
  #lr=c(0.6,rep(-999,dim(fn0)[2]-1))
  #changing value
  ##natural mortality
  fn0[8:61,] <- Mff
  ###steepness
  fn0[1026, ] <- hh
  ###lambda for index
  #fn0[786, ] <- lii
  ###lambda for catch
  #fn0[787, ] <- lcc
  ###effective sample size
  #fn0[897:950,] <- esss
  ###lambda for recruit
  #fn0[1011, ] <- lrr
  ####lambda for N1
  #fn0[1009,] <- N11
  
  ##sample size
  #fn0[300:320,] <- ef
  #lambda for index
  #fn0[255,] <- li
  #lambda for catch
  #fn0[256,] <- lc
  #lambda for recruit
  #fn0[348,] <- lr
  
  
  write.table(fn0, file = "fn0.dat", sep = "\t", col.names = FALSE, 
              row.names = FALSE);
  fnames = shell("dir /b fn0.dat", intern = TRUE);
  fn = fnames;
  
  runAD(exe.file, paste("-ind", fn, sep = " "), logfile = FALSE, 
        verbose = T);
  
  iout1 = readRep(exe.file, ".std")
  MSY = iout1$value[419]
  Fmsy = iout1$value[421]
  para.sel = iout1$value[1:2]
  logf1 = iout1$value[3]
  logf = iout1$value[4:56]
  nf = length(logf)
  tf = c()
  logr =iout1$value[57:110]
  nr = length(logr)
  tr=c()
  logn = iout1$value[111:135]
  nnn=length(logn)
  tn=c()
  q = iout1$value[136:137]
  sc = iout1$value[148]
  index.sel = iout1$value[138:147]
  
  iout4 = readRep(exe.file, ".Rep")
  
  #354:376
  #for(i in 1:nrow(nabun)){
  #for(j in 1:ncol(nabun)){
  #abun[i]=iout4[i+353]
  #abun1 = as.numeric(strsplit(iout4[i+353], split=' ')[[1]][2])
  #abun2 = as.numeric(strsplit(iout4[i+353], split=' ')[[1]][3])
  #abun3 = as.numeric(strsplit(iout4[i+353], split=' ')[[1]][4])
  #abun4 = as.numeric(strsplit(iout4[i+353], split=' ')[[1]][5])
  #abun5 = as.numeric(strsplit(iout4[i+353], split=' ')[[1]][6])
  #abun6 = as.numeric(strsplit(iout4[i+353], split=' ')[[1]][7])
  #nabun[i,j]=
  #  }
  #}
  #abun.v <- as.numeric(abun)
  b.h=c()
  tsel=c()
  tsc=c()
  tf1=c()
  tq=c()
  para.seln=c()
  index.seln=c()
  
  for (j in 1:nf){
    if(logf[j]<=-15.0 | logf[j]>=15.0){
      tf[j] = c(1)
    } else {
      tf[j] = c(0)
    }
  }
  equalf = all(tf==0)
  if(equalf){
    equalf=TRUE
  } else {
    equalf=FALSE
  }
  for (w in 1:nr){
    if(logr[w]<=-15.0 | logr[w]>=15.0){
      tr[w]= c(1)
    } else {
      tr[w] = c(0)
    }
  }
  equalr = all(tr==0)
  if(equalr){
    equalr=TRUE
  } else {
    equalr=FALSE
  }
  for (e in 1:nnn){
    if(logn[e]<=-15.0 | logn[e]>=15.0){
      tn[e]=c(1)
    } else {
      tn[e]=c(0)
    }
  }
  equaln = all(tn==0)
  if(equaln){
    equaln=TRUE
  } else {
    equaln=FALSE
  }
  
  for (v in 1:length(para.sel)){
    if(para.sel[v]<=0 | para.sel[v]>=26.0){
      para.seln[v]=c(1)
    } else {
      para.seln[v]=c(0)
    }
  }
  equalsel = all(para.seln==0)
  if(equalsel){
    equalsel=TRUE
  } else {
    equalsel=FALSE
  }
   
  
  for (n in 1:length(index.sel)){
    if(index.sel[n]<=0 | index.sel[n]>=26.0){
      index.seln[n]=c(1)
    } else {
      index.seln[n]=c(0)
    }
  }
  equalseli = all(index.seln==0)
  if(equalseli){
    equalseli=TRUE
  } else {
    equalseli=FALSE
  }
  
  if(logf1<=-15.0 | logf1 >=2.0){
    tf1 <- FALSE
  } else {
    tf1 <- TRUE
  }   
  for (l in 1:length(q)){
    if(q[l]<=-30.0 | q[l]>=5.0){
      tq= FALSE
    } else {
      tq=TRUE
    }
  }
  if(sc <=-1.0 | sc>=200.0){
    tsc = FALSE
  }  else {
    tsc = TRUE
  }
  
  if(equalf == TRUE & equalr == TRUE &
     equaln == TRUE & equalsel == TRUE & 
     tf1 == TRUE & tq == TRUE & tsc == TRUE & equalseli == TRUE){
    b.h=c("TRUE")
  } else {
    b.h = c("False")
  }
  biomass = iout4[1067:1120]
  class(biomass)
  year=c()
  Jan1B=c()
  SSB=c()
  ii=c()
  #for (ii in 1:yr) {
    #year[ii] = as.numeric(strsplit(biomass, split = ' ')[[ii]][1])
    #Jan1B[ii] = as.numeric(strsplit(biomass, split = ' ')[[ii]][2])
    #SSB[ii] = as.numeric(strsplit(biomass, split = ' ')[[ii]][3])
  #};
  bio <- data.frame("year"=year, "Jan1B"=Jan1B, "SSB"=SSB)
  
  #pars = iout$name
  #npar = length(pars)
  #outmat = matrix(0, ncol = (2 * npar), nrow = 1)
  #stds = paste("std.", pars, sep = "")
  #cols = c(pars, stds)
  #dimnames(outmat) = list(NULL, cols)
  #outvec = c(iout$value, iout$std)
  #outmat[1, ] = outvec
  
  #iout2 = readRep(exe.file, ".cor")
  #iout2 = c(iout2$al)
  #cor.albe = iout2[length(iout2)]
  
  iout3 = readRep(exe.file, ".par")
  nll.iout3 = as.character(list(iout3[1]))
  objective=as.numeric(strsplit(nll.iout3, split= ' ')[[1]][11])
  max.g=as.numeric(strsplit(nll.iout3, split= ' ')[[1]][17])
  
  iout4 = readRep(exe.file, ".Rep")
  sel.dir = c(iout4[725])
  a.1 = as.numeric(strsplit(sel.dir, split=' ')[[1]][2])
  a.2 = as.numeric(strsplit(sel.dir, split=' ')[[1]][3])
  a.3 = as.numeric(strsplit(sel.dir, split=' ')[[1]][4])
  a.4 = as.numeric(strsplit(sel.dir, split=' ')[[1]][5])
  a.5 = as.numeric(strsplit(sel.dir, split=' ')[[1]][6])
  a.6 = as.numeric(strsplit(sel.dir, split=' ')[[1]][7])
  a.7 = as.numeric(strsplit(sel.dir, split=' ')[[1]][8])
  a.8 = as.numeric(strsplit(sel.dir, split=' ')[[1]][9])
  a.9 = as.numeric(strsplit(sel.dir, split=' ')[[1]][10])
  a.10 = as.numeric(strsplit(sel.dir, split=' ')[[1]][11])
  a.11 = as.numeric(strsplit(sel.dir, split=' ')[[1]][12])
  a.12 = as.numeric(strsplit(sel.dir, split=' ')[[1]][13])
  a.13 = as.numeric(strsplit(sel.dir, split=' ')[[1]][14])
  a.14 = as.numeric(strsplit(sel.dir, split=' ')[[1]][15])
  a.15 = as.numeric(strsplit(sel.dir, split=' ')[[1]][16])
  a.16 = as.numeric(strsplit(sel.dir, split=' ')[[1]][17])
  a.17 = as.numeric(strsplit(sel.dir, split=' ')[[1]][18])
  a.18 = as.numeric(strsplit(sel.dir, split=' ')[[1]][19])
  a.19 = as.numeric(strsplit(sel.dir, split=' ')[[1]][20])
  a.20 = as.numeric(strsplit(sel.dir, split=' ')[[1]][21])
  a.21 = as.numeric(strsplit(sel.dir, split=' ')[[1]][22])
  a.22 = as.numeric(strsplit(sel.dir, split=' ')[[1]][23])
  a.23 = as.numeric(strsplit(sel.dir, split=' ')[[1]][24])
  a.24 = as.numeric(strsplit(sel.dir, split=' ')[[1]][25])
  a.25 = as.numeric(strsplit(sel.dir, split=' ')[[1]][26])
  a.26 = as.numeric(strsplit(sel.dir, split=' ')[[1]][27])

  
  #sel.ndir = c(iout4[1642])
  sel.dir.num = c(a.1, a.2, a.3, a.4, a.5, a.6, a.7, a.8, a.9, a.10, a.11, a.12, a.13, a.14, a.15, a.16, a.17, a.18, a.19, a.20, 
                  a.21, a.22, a.23, a.24, a.25, a.26)
  #a.1.n = as.numeric(strsplit(sel.ndir, split=' ')[[1]][2])
  #a.2.n = as.numeric(strsplit(sel.ndir, split=' ')[[1]][3])
  #a.3.n = as.numeric(strsplit(sel.ndir, split=' ')[[1]][4])
  #a.4.n = as.numeric(strsplit(sel.ndir, split=' ')[[1]][5])
  #a.5.n = as.numeric(strsplit(sel.ndir, split=' ')[[1]][6])
  #a.6.n = as.numeric(strsplit(sel.ndir, split=' ')[[1]][7])
  #sel.ndir.num = c(a.1.n, a.2.n, a.3.n, a.4.n, a.5.n, a.6.n)
  al = str_trim(c(iout4[1464]))
  be = str_trim(c(iout4[1465]))
  al.num = as.numeric(substr(al,8, str_length(al)))
  be.num = as.numeric(substr(be,7, str_length(be)))
  fec = c(iout4[1690])
  f.1 = as.numeric(strsplit(fec, split=' ')[[1]][2])
  f.2 = as.numeric(strsplit(fec, split=' ')[[1]][3])
  f.3 = as.numeric(strsplit(fec, split=' ')[[1]][4])
  f.4 = as.numeric(strsplit(fec, split=' ')[[1]][5])
  f.5 = as.numeric(strsplit(fec, split=' ')[[1]][6])
  f.6 = as.numeric(strsplit(fec, split=' ')[[1]][7])
  f.7 = as.numeric(strsplit(fec, split=' ')[[1]][8])
  f.8 = as.numeric(strsplit(fec, split=' ')[[1]][9])
  f.9 = as.numeric(strsplit(fec, split=' ')[[1]][10])
  f.10 = as.numeric(strsplit(fec, split=' ')[[1]][11])
  f.11 = as.numeric(strsplit(fec, split=' ')[[1]][12])
  f.12 = as.numeric(strsplit(fec, split=' ')[[1]][13])
  f.13 = as.numeric(strsplit(fec, split=' ')[[1]][14])
  f.14 = as.numeric(strsplit(fec, split=' ')[[1]][15])
  f.15 = as.numeric(strsplit(fec, split=' ')[[1]][16])
  f.16 = as.numeric(strsplit(fec, split=' ')[[1]][17])
  f.17 = as.numeric(strsplit(fec, split=' ')[[1]][18])
  f.18 = as.numeric(strsplit(fec, split=' ')[[1]][19])
  f.19 = as.numeric(strsplit(fec, split=' ')[[1]][20])
  f.20 = as.numeric(strsplit(fec, split=' ')[[1]][21])
  f.21 = as.numeric(strsplit(fec, split=' ')[[1]][22])
  f.22 = as.numeric(strsplit(fec, split=' ')[[1]][23])
  f.23 = as.numeric(strsplit(fec, split=' ')[[1]][24])
  f.24 = as.numeric(strsplit(fec, split=' ')[[1]][25])
  f.25 = as.numeric(strsplit(fec, split=' ')[[1]][26])
  f.26 = as.numeric(strsplit(fec, split=' ')[[1]][27])

  
  
  
  
  
  
  fecu = c(f.1, f.2, f.3, f.4, f.5, f.6,f.7,f.8,f.9,f.10,f.11,f.12,f.13,f.14,f.15,f.16,f.17,f.18,f.19,f.20,
           f.21,f.22,f.23,f.24,f.25,f.26)
  
  
  
  
  #summ = list(cbind(outmat, cor.albe),iout3);
  summ = list("objective.function"=objective,"max.g"=max.g,"selectivity.dir"=sel.dir.num,
              "selectivity.ndir"=sel.ndir.num, "bound.heat"=b.h,
              "MSY"=MSY, "Fmsy"=Fmsy, "alpha"=al.num, "beta"=be.num, "biomass"=bio, "R0"=exp(sc),
              "sel1"=para.sel[1],"sel2"=para.sel[2],#"sel3"=para.sel[3], "sel4"=para.sel[4], "sel5"=para.sel[5],
              #"sel6"=para.sel[6], "sel7"=para.sel[7], "sel8"=para.sel[8], "sel9"=para.sel[9],
              "fecundity"=fecu);
  #summ.save[[i]]=summ;
  #summ.save.save[[i]]=summ;
  summ.save.save.save[[i]]=summ;
  #summ.save.save.save.save[[i]]=summ;
  #summ.save.save.save.save.save[[i]]=summ;
  #summ.save.save.save.save.save.save[[i]]=summ;
  #summ.save.save.save.save.save.save.save[[i]]=summ;
} 



summ.save
data.summ <- lapply(summ.save, paste, collaspe=" ")
data.summ <- as.data.frame(do.call("rbind",data.summ))
names(data.summ)=c("obj.fn","max.g","selectivity.dir","selectivity.ndir","bound.heat","msy","fmsy","alpha","beta")
data.summ.dff <- data.frame("h"=mh$Var2[1:dim(data.summ)[1]],"m"=mh$Var1[1:dim(data.summ)[1]],
                            "obj.fn"=as.numeric(data.summ$obj.fn),"max.g"=as.numeric(data.summ$max.g), 
                            "bound.heat"=data.summ$bound.heat,
                            "msy"=as.numeric(data.summ$msy),"Fmsy"=as.numeric(data.summ$fmsy),
                            "sel"=data.summ$selectivity.dir,
                            "a"=as.numeric(data.summ$alpha),"b"=as.numeric(data.summ$beta))
dim(data.summ.dff)
data.summ.dfff <- na.omit(data.summ.dff)
dim(data.summ.dfff)
data.summ.dfff$bound.heat
filtered.dff <- data.summ.dfff[data.summ.dfff$max.g <= 0.01 & data.summ.dfff$msy <=1.00e+9 & data.summ.dfff$Fmsy <= 2.98 ,]
dim(filtered.dff)
filtered.dfff <- rbind(filtered.dff[2:6,],filtered.dff[8:10,],filtered.dff[12:14,],filtered.dff[16:18,],
                       filtered.dff[20:21,],filtered.dff[23:24,],filtered.dff[26:27,],filtered.dff[29:30,])

filtered.dfff[filtered.dfff$obj.fn==min(filtered.dfff$obj.fn),]
h0.6 = filtered.dfff[filtered.dfff$h == 0.6,]



write.xlsx(filtered.dff, "over.xlsx")
sm =min(filtered.dff$obj.fn)
filtered.dff[filtered.dff$obj.fn==sm, ]
h0.3_1 = filtered.dff[filtered.dff$h==0.3,]
plot(h0.3_1$m, h0.3_1$obj.fn,type="l")
m0.65 = rbind(filtered.save.df[1,],filtered.dff[filtered.dff$m==0.65,])
plot(m0.65$h, m0.65$obj.fn,type="l")
right = rbind(filtered.save.df[1,],filtered.dff)
write.xlsx(right, "right.xlsx")

filtered.df[100:199,]
install.packages("openxlsx")
library(openxlsx)
write.xlsx(filtered.df, "filtered.df.xlsx")
#1333.78
rank.df <- data.frame(filtered.df, rank=rank(filtered.df$obj.fn))
#rank.dff <- na.omit(rank.df)
dim(rank.df)
dim(filtered.df)
dim(rank.df)
#rank2 = 948.9816, 300

summ.save.save
data.summ.save <- lapply(summ.save.save, paste, collaspe=" ")
data.summ.save.df.df <- as.data.frame(do.call("rbind",data.summ.save))
names(data.summ.save.df.df)=c("obj.fn","max.g","selectivity.dir","selectivity.ndir","bound.heat","msy","fmsy","alpha","beta","biomass","R0")
data.summ.save.dff <- data.frame("h"=mh$Var2[1:dim(data.summ.save.df.df)[1]],"m"=mh$Var1[1:dim(data.summ.save.df.df)[1]],
                                 "obj.fn"=as.numeric(data.summ.save.df.df$obj.fn),"max.g"=as.numeric(data.summ.save.df.df$max.g), 
                                 "bound.heat"=data.summ.save.df.df$bound.heat,
                                 "msy"=as.numeric(data.summ.save.df.df$msy), "Fmsy"=as.numeric(data.summ.save.df.df$fmsy),
                                 "sel"=data.summ.save.df.df$selectivity.dir,"a"=as.numeric(data.summ.save.df.df$alpha), "b"=as.numeric(data.summ.save.df.df$beta))
                                 "R0"=as.numeric(data.summ.save.df.df$R0))

dim(data.summ.save.dff)
data.summ.save.dfff <- na.omit(data.summ.save.dff)
dim(data.summ.save.dfff)
filtered.save.df <- data.summ.save.dfff[data.summ.save.dfff$max.g <= 0.01 & data.summ.save.dfff$msy <= 1.000e+9 & data.summ.save.dfff$Fmsy < 3.0,]
dim(filtered.save.df)

library(readxl)
write.xlsx(filtered.save.df,"redfish.xlsx")
filtered.save.df[1,]
sm.save =min(filtered.save.df$obj.fn)
#1604.199, m= 0.37, h=1.0
filtered.save.df[filtered.save.df$obj.fn==sm.save,]####index=2, c =1.5, ess =100, recruit=0.5


R000 = filtered.save.df$R0/1000000
m = filtered.save.df$m
par(mar=c(5,3.9,4,1))
plot(m, R000,xlab="",ylab="",cex=1.2)
mtext(expression("M ("*year^-1*")"),side=1,line=3,at=-0.1,cex=1.5)
mtext("(a)", side=3,line=0.3, at=-2)
mtext("(b)",side=3, line=0.3, at=0.1)

library(openxlsx)
mhfm <- rbind(filtered.df, filtered.save.df)

write.xlsx(mhfm, "mhfm2.xlsx")


summ.save.save.save
data.summ.save.save <- lapply(summ.save.save.save, paste, collaspe=" ")
data.summ.save.save.df.df <- as.data.frame(do.call("rbind",data.summ.save.save))
names(data.summ.save.save.df.df)=c("obj.fn","max.g","selectivity.dir","selectivity.ndir","bound.heat","msy","fmsy","alpha","beta","biomass","R0")
data.summ.save.save.dff <- data.frame("h"=mh$Var2[1:dim(data.summ.save.save.df.df)[1]],
                                      "m"=mh$Var1[1:dim(data.summ.save.save.df.df)[1]],
                                      "obj.fn"=as.numeric(data.summ.save.save.df.df$obj.fn),
                                      "max.g"=as.numeric(data.summ.save.save.df.df$max.g), 
                                      "bound.heat"=data.summ.save.save.df.df$bound.heat,
                                      "msy"=as.numeric(data.summ.save.save.df.df$msy),
                                      "Fmsy"=as.numeric(data.summ.save.save.df.df$fmsy),
                                      #"sel"=data.summ.save.save.df.df$selectivity.dir,"a"=data.summ.save.save.df.df$alpha, "b"=data.summ.save.save.df.df$beta,
                                      "R0"=data.summ.save.save.df.df$R0)

R005 <- as.numeric(filtered$R0)/1000000
m005 <- filtered$m
par(mar=c(5,5,4,2)+0.1)
plot(m005,R005, xlab= "", ylab="", pch=1,cex.lab=1.5)
par(mfrow=c(1,3))
dim(data.summ.save.save.dff)
data.summ.save.save.dfff <- na.omit(data.summ.save.save.dff)
dim(data.summ.save.save.dfff)
filtered.save.save.df <- data.summ.save.save.dfff[data.summ.save.save.dfff$max.g <= 0.01 & data.summ.save.save.dfff$msy <= 1.000e+9,]
filtered <- rbind(data.summ.save.save.dfff[1:8,],data.summ.save.save.dfff[17:28,])
sm.save.save =min(filtered.save.save.df$obj.fn)
filtered.save.save.df[filtered.save.save.df$obj.fn==sm.save.save,]
dim(filtered.save.save.df)
sne <- rbind(filtered.save.df,filtered.save.save.df)
sm = min(sne$obj.fn)
h0.35 = sne[sne$h==0.35,]
plot(h0.35$m, h0.35$obj.fn,type="l")
m0.71 = sne[sne$m==0.71,]

h0.7 = filtered.save.save.df[filtered.save.save.df$h==0.7,]
m0.8 = filtered.save.save.df[filtered.save.save.df$m==0.8,]
plot(h0.7$m,h0.7$obj.fn,type="l")
plot(m0.8$h,m0.8$obj.fn)


summ.save.save.save.save
length(summ.save.save.save.save)
data.summ.save.save.save <- lapply(summ.save.save.save.save, paste, collaspe=" ")
data.summ.save.save.save.df <- as.data.frame(do.call("rbind",data.summ.save.save.save))
names(data.summ.save.save.save.df)=c("obj.fn","max.g","selectivity.dir","selectivity.ndir","bound.heat","msy","fmsy","alpha","beta")
data.summ.save.save.save.dff <- data.frame("h"=mh$Var2[1:dim(data.summ.save.save.save.df)[1]],
                                           "m"=mh$Var1[1:dim(data.summ.save.save.save.df)[1]],
                                           "obj.fn"=as.numeric(data.summ.save.save.save.df$obj.fn),
                                           "max.g"=as.numeric(data.summ.save.save.save.df$max.g), 
                                           "bound.heat"=data.summ.save.save.save.df$bound.heat,
                                           "msy"=as.numeric(data.summ.save.save.save.df$msy))
                                           "selectivity"=data.summ.save.save.save.df$selectivity.dir,
                                           "Fmsy"=as.numeric(data.summ.save.save.save.df$fmsy))
dim(data.summ.save.save.save.dff)
data.summ.save.save.save.dfff <- na.omit(data.summ.save.save.save.dff)
dim(data.summ.save.save.save.dfff)
filtered.save.save.save.df <- data.summ.save.save.save.dfff[data.summ.save.save.save.dfff$max.g <= 0.01 & data.summ.save.save.save.dfff$msy <= 1.000e+9,]
write(filtered.save.save.save.df,)
sm.save.save.save =min(filtered.save.save.save.df$obj.fn)
filtered.save.save.save.df[filtered.save.save.save.df$obj.fn==sm.save.save.save,]
h0.3 <- filtered.save.save.save.df[filtered.save.save.save.df$h==0.3,]
m0.7 <- filtered.save.save.save.df[filtered.save.save.save.df$m==0.7,]
plot(m0.65$h,m0.65$obj.fn)
#1330.204





####h0.5, m0.1 낮음


summ.save.save.save.save.save
length(summ.save.save.save.save.save)
data.summ.save.save.save.save <- lapply(summ.save.save.save.save.save, paste, collaspe=" ")
data.summ.save.save.save.save.df <- as.data.frame(do.call("rbind",data.summ.save.save.save.save))
names(data.summ.save.save.save.save.df)=c("obj.fn","max.g","selectivity.dir","selectivity.ndir","bound.heat","msy","fmsy","alpha","beta")

data.summ.save.save.save.save.dff <- data.frame("h"=mh$Var2[1:dim(data.summ.save.save.save.save.df)[1]],
                                                "m"=mh$Var1[1:dim(data.summ.save.save.save.save.df)[1]],
                                                "obj.fn"=as.numeric(data.summ.save.save.save.save.df$obj.fn),
                                                "max.g"=as.numeric(data.summ.save.save.save.save.df$max.g), 
                                                "bound.heat"=data.summ.save.save.save.save.df$bound.heat,
                                                "msy"=data.summ.save.save.save.save.df$msy)
dim(data.summ.save.save.save.save.dff)
data.summ.save.save.save.save.dfff <- na.omit(data.summ.save.save.save.save.dff)
data.summ.save.save.save.save.dfff[1:150,]
dim(data.summ.save.save.save.save.dfff)
filtered.save.save.save.save.df <- data.summ.save.save.save.save.dfff[data.summ.save.save.save.save.dfff$max.g <= 0.01 & data.summ.save.save.save.save.dfff$msy <= 1.000e+9,]
sm.save.save.save.save =min(filtered.save.save.save.save.df$obj.fn)
#1329.042

filtered.save.save.save.save.df[filtered.save.save.save.save.df$obj.fn==sm.save.save.save.save,]
h0.5 = filtered.save.save.save.save.df[filtered.save.save.save.save.df$h==0.5,]
plot(h0.5$m,h0.5$obj.fn)
m0.7 = filtered.save.save.save.save.df[filtered.save.save.save.save.df$m==0.7,]



summ.save.save.save.save.save.save
length(summ.save.save.save.save.save.save)
df <- lapply(summ.save.save.save.save.save.save, paste, collaspe=" ")
df.df <- as.data.frame(do.call("rbind",df))
names(df.df)=c("obj.fn","max.g","selectivity.dir","selectivity.ndir","bound.heat","msy","fmsy","alpha","beta")

dff <- data.frame("h"=mh$Var2[1:dim(df.df)[1]],
                                                     "m"=mh$Var1[1:dim(df.df)[1]],
                                                     "obj.fn"=as.numeric(df.df$obj.fn),
                                                     "max.g"=as.numeric(df.df$max.g), 
                                                     "bound.heat"=df.df$bound.heat,
                                                     #"selectivity"=data.summ.save.save.save.save.save.df$selectivity.dir,
                                                     "msy"=as.numeric(df.df$msy),
                                                     "Fmsy"=as.numeric(df.df$fmsy))
dim(dff)
dfff <- na.omit(dff)

dim(dfff)
filtered.dfff <- dfff[dfff$max.g <= 0.01 & dfff$msy <= 1.0E+9 & dfff$Fmsy <= 2.99,]
dim(filtered.dfff)
sm.filtered.dfff =min(filtered.dfff$obj.fn)
#1329.01

filtered.dfff[filtered.dfff$obj.fn==sm.filtered.dfff,]
h0.55 = filtered.save.save.save.save.save.save.df[filtered.save.save.save.save.save.save.df$h==0.55,]
m0.7 = filtered.save.save.save.save.save.save.df[filtered.save.save.save.save.save.save.df$m==0.7,]


summ.save.save.save.save.save.save.save
length(summ.save.save.save.save.save.save.save)
data.summ.save.save.save.save.save.save.save <- lapply(summ.save.save.save.save.save.save.save, paste, collaspe=" ")
data.summ.save.save.save.save.save.save.save.df <- as.data.frame(do.call("rbind",data.summ.save.save.save.save.save.save.save))
names(data.summ.save.save.save.save.save.save.save.df)=c("obj.fn","max.g","selectivity.dir","selectivity.ndir","bound.heat","msy","fmsy","alpha","beta")

data.summ.save.save.save.save.save.save.save.dff <- data.frame("h"=mh$Var2[1:dim(data.summ.save.save.save.save.save.save.save.df)[1]],
                                                          "m"=mh$Var1[1:dim(data.summ.save.save.save.save.save.save.save.df)[1]],
                                                          "obj.fn"=as.numeric(data.summ.save.save.save.save.save.save.save.df$obj.fn),
                                                          "max.g"=as.numeric(data.summ.save.save.save.save.save.save.save.df$max.g), 
                                                          "bound.heat"=data.summ.save.save.save.save.save.save.save.df$bound.heat,
                                                          "msy"=data.summ.save.save.save.save.save.save.save.df$msy,
                                                          "fmsy"=data.summ.save.save.save.save.save.save.save.df$fmsy)
dim(data.summ.save.save.save.save.save.save.save.dff)
data.data.summ.save.save.save.save.save.save.save.dfff <- na.omit(data.summ.save.save.save.save.save.save.save.dff)
data.data.summ.save.save.save.save.save.save.save.dfff
dim(data.data.summ.save.save.save.save.save.save.save.dfff)
filtered.save.save.save.save.save.save.save.dff<- data.data.summ.save.save.save.save.save.save.save.dfff[data.data.summ.save.save.save.save.save.save.save.dfff$max.g <= 0.01 & data.data.summ.save.save.save.save.save.save.save.dfff <= 1.0E+9,]
dim(filtered.save.save.save.save.save.save.save.dff)
sm.save.save.save.save.save.save.save =min(filtered.save.save.save.save.save.save.save.dff$obj.fn)
#1329.01

filtered.save.save.save.save.save.save.save.dff[filtered.save.save.save.save.save.save.save.dff$obj.fn==sm.save.save.save.save.save.save.save,]


plot(filtered.save.df$h, filtered.save.df$obj.fn, xlab="h",ylab="objective function",type="l")
abline(v=1.0)




mmsy=rbind(filtered.df, filtered.save.df,filtered.save.save.df, filtered.save.save.save.df, filtered.save.save.save.save.df)
mmmsy=data.frame(mmsy$h,mmsy$m,mmsy$msy)
mmmmsy =mmmsy[17:dim(mmmsy)[1],]
names(mmmmsy)=c("h","m","msy")



ind=which(filtered.df$obj.fn==sm)
filtered.df[330,]
rank.df[ind[2],]
rank.df[which(rank.df$rank==9),]
min.obj <- min(filtered.dff$obj.fn)
indices <- which(data.summ.dff$max.g <= 0.01)
length(indices)
dim(filtered.dff)
i.filter <- which(filtered.dff$obj.fn == min.obj)
filtered.dff[i.filter,]
AIC <- 2*filtered.dff$obj.fn+2*51
rank(filtered.dff)



a=filtered.save.save.save.save.df$h
b=filtered.save.save.save.save.df$m
tem.grid.=expand.grid(a,b)
names(tem.grid.)=c("a","b")
newdata=data.frame(a=tem.grid.$a,b=tem.grid.$b,Like = rep(NA,dim(tem.grid.)[1]))
library(lattice)
trellis.par.set("axis.line", list(col="transparent"))
testfig1=wireframe(mmmmsy$msy~mmmmsy$m*mmmmsy$h, 
                   data=mmmmsy, drape = F,
                   xlab="h", ylab='M', zlab='MSY'); 
testfig1;

contourplot(filtered.save.save.save.save.df$obj.fn~filtered.save.save.save.save.df$h*filtered.save.save.save.save.df$m)
wireframe(filtered.save.save.save.save.df$obj.fn~filtered.save.save.save.save.df$h*filtered.save.save.save.save.df$m,
          data = filtered.save.save.save.save.save.save.df, drape = FALSE,
          scales=list(arrow=FALSE, cex=0.5, col="black", font=3),xlab = "h", ylab= "M", zlab="obj.fn")
panel.lines3d(h = c(1.0,1.0), M= c(0.37,0.37), obj.fn
              install.packages("scatterplot3d")
              library(scatterplot3d)
              p <- scatterplot3d(filtered.save.save.save.save.df[,1:3],)
              points3d(x=1.0, y=0.37, z=1327.47, color="red",lwd=2)
              lines3d(c(1.0, 1.0), c(0.37, 0.37), c(1327.47, filtered_save_df$col3[1]),
                      col = "red", lwd = 2)
              
              install.packages("rgl")
              library(rgl)
              
              
              cloud(obj.fn ~ h * m, data = filtered.save.save.save.save.df,
                    screen = list(x = -90, y = 70), panel.3d.cloud = panel.3dbars,
                    xlab = "h", ylab = "M", zlab = "objective function")
              
              # 추가로 선 그리기
              panel.lines3d(x = c(1.0, 1.0), y = c(0.37, 0.37), z = c(1327.47, filtered_save_df$col3[1]),
                            col = "red", lwd = 2, add = TRUE)
              
              
              install.packages("plotly")
              library(plotly)
              plot3d(mmmmsy$m, mmmmsy$h, mmmmsy$msy, type = "s",
                     xlab = "M", ylab = "h", zlab = "MSY")
              p <- plot_ly(filtered.save.save.save.save.df, x=filtered.save.save.save.save.df$h,
                           y=filtered.save.save.save.save.df$m,
                           z=filtered.save.save.save.save.df$obj.fn,
                           layout(scene = list(xaxis = list(title = "h"),
                                               yaxis = list(title="M"),
                                               zaxis = list(title="objective function")),
                                  annotation = list(
                                    x=1.0, y=0.37, text= best)
                           ))
              install.packages("writexl")
              library(writexl)
              write_xlsx(filtered.save.save.save.save.df,"ddf.xlsx")
              write_xlsx(mmmmsy,"msy.xlsx")
              add_markers(p)
              
              
              cloud(obj.fn )
              
              
              library(lattice)
              wireframe(mmmmsy$msy~mmmmsy$h*mmmmsy$m,
                        data = mmmmsy, drape = FALSE,
                        scales=list(arrow=FALSE, cex=0.5, col="black", font=3),xlab = "h", ylab= "M", zlab="msy")
              
              
              wireframe(msy ~ m * h, data = mmmmsy,
                        xlab = "M", ylab = "h", zlab = "MSY")
              
              library(ggplot2)
              options(scipen=100)
              par(mfrow=c(1,2))
              par(oma=c(2,0,0,0))
              p <- ggplot()+
                geom_line(mapping=aes(x=m, y=msy),data=data.summ.save.save.dff)
              
              plot(filtered.save.save.df$m, filtered.save.save.df$msy, type="l", xlab="",ylab=c("MT"))
              plot(filtered.save.save.df$m, filtered.save.save.df$Fmsy, type = "l", xlab="", ylab=c("1/year"))
              mtext("M", side=1, outer=T,at=0.5)
              
              
              
              