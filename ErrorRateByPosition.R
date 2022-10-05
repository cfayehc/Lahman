#same as ErrorRateByPosition2 on 10/5/2022 - Works 

library(Lahman)
library(dplyr)

#set the working directory
setwd("E:/Research/Lahman-master")

getResidue <- function(x,y,svar) {
  #  x<-d$Chance
  #  y<-d$svar
  relation<-lm(y~x)
  relation<-lm(formula = y~x)
  print(relation)
  summary(relation)
  
  graphlabel<-paste(svar," vs Chances all players",ppos,">",GG,"Games",sep=" ")
  #graphname<-paste("EvA",ppos,"linear.png",sep="-")
  #png(file=graphname)
  plot(x=x,y=y, xlab="Chances", ylab=svar, main=graphlabel,col="blue",abline(lm(y~x)),cex=1.3,pch=16)
  #dev.off()
  svar.res=resid(relation)
  #  d<-cbind(d,svar.res)
  #  print(d)
}

#Populate rangef with rf =range factor, rfg = rangefactor per game, rfga=adjust rangefactor per game, chance, 
#fieldp = fielding percentage, E.rate=rate of errors per chance
rangef <- select(Fielding,playerID, yearID, POS, G, InnOuts, PO, A, E)
rangef <- mutate(rangef,playerID, yearID, POS, G, PO, A, E, rf=(PO+A), rfg=rf/G, Chance=PO+A+E, fieldp=rf/Chance)
#,  E.rate=E/Chance,)]
#dont need E.rate as E.rate = 1 - fieldingp

#Choose Position and make a variable based on the position select number of games
#filter list by position and number of games played  --and year?
GG=82
ppos="2B"  
year=1910
year2=2022

str1<-"rf"
dfname<-paste(str1,ppos,sep="")
#filter the rangefactor by position and number of games
#assign(dfname,filter(rangef,G>GG,POS==ppos))
assign(dfname,filter(rangef,G>GG,POS==ppos,yearID>=year & yearID<=year2))
#print(get(dfname))

residues<-get(dfname)

E.res<-getResidue(get(dfname)$Chance,get(dfname)$E,'E')
residues<-cbind(residues,E.res)

#rf.res<-getResidue(get(dfname)$Chance,get(dfname)$rf,'rf') #same as -E.res
#residues<-cbind(residues,rf.res)

PO.res<-getResidue(get(dfname)$Chance,get(dfname)$PO,'PO')
residues<-cbind(residues,PO.res)

A.res<-getResidue(get(dfname)$Chance,get(dfname)$A,'A')
residues<-cbind(residues,A.res)

residues<-cbind(residues,pE=residues$E.res/get(dfname)$Chance)  #probability of E.res per chance?
assign(dfname,residues)

topp <- filter(get(dfname), (rank(E.res)<=20))
print(topp[order(-topp$rf), ] )

#rf.res2<-getResidue(get(dfname)$rfg,get(dfname)$E.res,'rf.res')
#residues<-cbind(get(dfname),rf.res2)
