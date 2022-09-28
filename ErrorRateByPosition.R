library(Lahman)
library(dplyr)

#set the working directory
setwd("E:/Research/Lahman-master")

#Populate rangef with rf =range factor, rfg = rangefactor per game, rfga=adjust rangefactor per game, chance, 
#fieldp = fielding percentage, E.rate=rate of errors per chance
rangef <- select(Fielding,playerID, yearID, POS, G, InnOuts, PO, A, E)
rangef <- mutate(rangef,playerID, yearID, POS, G, PO, A, E, rf=(PO+A), rfg=rf/G, rfga=(PO+A-E)/G, Chance=PO+A+E, fieldp=rf/Chance, E.rate=E/Chance,)

#Choose Position and make a variable based on the position select number of games
#filter list by position and number of games played  --and year?
GG=82
ppos="3B"  
year=1910

str1<-"rf"
dfname<-paste(str1,ppos,sep="")
#filter the rangefactor by position and number of games
#assign(dfname,filter(rangef,G>GG,POS==ppos))
assign(dfname,filter(rangef,G>GG,POS==ppos,yearID>=year))
#print(get(dfname))

######################################################################
#Plot E/chance and fit
x<-get(dfname)$Chance
y<-get(dfname)$E
graphlabel<-paste("Errors vs Chances all players",ppos,">",GG,"Games",sep=" ")
#plot(x=x,y=y, xlab="Chances", ylab="Errors", main=graphlabel)
Erelation<-lm(y~x)
Erelation<-lm(formula = y~x)
print(Erelation)
summary(Erelation)

graphlabel<-paste("Errors vs Chances all players",ppos,">",GG,"Games",sep=" ")
#graphname<-paste("EvA",ppos,"linear.png",sep="-")
#png(file=graphname)
plot(x=x,y=y, xlab="Chances", ylab="Errors", main=graphlabel,col="blue",abline(lm(y~x)),cex=1.3,pch=16)
#dev.off()

######################################################################
#Plot rf/chance and fit
#x<-get(dfname)$Chance
y<-get(dfname)$rf
#graphlabel<-paste("rf vs Chances all players",ppos,">",GG,"Games",sep=" ")
#plot(x=x,y=y, xlab="Chances", ylab="range factor", main=graphlabel)
rfrelation<-lm(y~x)
rfrelation<-lm(formula = y~x)
print(rfrelation)
summary(rfrelation)

graphlabel<-paste("range factor vs Chances all players",ppos,">",GG,"Games",sep=" ")
#graphname<-paste("EvA",ppos,"linear.png",sep="-")
#png(file=graphname)
plot(x=x,y=y, xlab="Chances", ylab="range factor", main=graphlabel,col="blue",abline(lm(y~x)),cex=1.3,pch=16)
#dev.off()

######################################################################
#Plot PO/chance and fit
#x<-get(dfname)$Chance
y<-get(dfname)$PO
#graphlabel<-paste("rf vs Chances all players",ppos,">",GG,"Games",sep=" ")
#plot(x=x,y=y, xlab="Chances", ylab="range factor", main=graphlabel)
POrelation<-lm(y~x)
POrelation<-lm(formula = y~x)
print(POrelation)
summary(POrelation)

graphlabel<-paste("Putouts vs Chances all players",ppos,">",GG,"Games",sep=" ")
#graphname<-paste("POvA",ppos,"linear.png",sep="-")
#png(file=graphname)
plot(x=x,y=y, xlab="Chances", ylab="Putouts", main=graphlabel,col="blue",abline(lm(y~x)),cex=1.3,pch=16)
#dev.off()

######################################################################
#Plot Assists/chance and fit
#x<-get(dfname)$Chance
y<-get(dfname)$A
#graphlabel<-paste("Assists vs Chances all players",ppos,">",GG,"Games",sep=" ")
#plot(x=x,y=y, xlab="Chances", ylab="Assists", main=graphlabel)
Arelation<-lm(y~x)
Arelation<-lm(formula = y~x)
print(Arelation)
summary(Arelation)

graphlabel<-paste("Assists vs Chances all players",ppos,">",GG,"Games",sep=" ")
#graphname<-paste("AvCh",ppos,"linear.png",sep="-")
#png(file=graphname)
plot(x=x,y=y, xlab="Chances", ylab="Assists", main=graphlabel,col="blue",abline(lm(y~x)),cex=1.3,pch=16)
#dev.off()

######################################################################
#A check Plot (PO+Assists)/chance and fit
#x<-get(dfname)$Chance
#y<-get(dfname)$A+get(dfname)$PO
#graphlabel<-paste("range factor vs Chances all players",ppos,">",GG,"Games",sep=" ")
#plot(x=x,y=y, xlab="Chances", ylab="range factor", main=graphlabel)
#rf2relation<-lm(y~x)
#rf2relation<-lm(formula = y~x)
#print(rf2relation)
#summary(rf2relation)

#graphlabel<-paste("Range factor vs Chances all players",ppos,">",GG,"Games",sep=" ")
#graphname<-paste("rf2vCh",ppos,"linear.png",sep="-")
#png(file=graphname)
#plot(x=x,y=y, xlab="Chances", ylab="range factor", main=graphlabel,col="blue",abline(lm(y~x)),cex=1.3,pch=16)
#dev.off()
#looks like range factor woo!

######################################################################
#calculate residues
#dfnameres <- paste(get(dfname),"res",sep=".")
error.res=resid(Erelation) #error residue == difference between linear model and number of errors
rf.res=resid(rfrelation) #error residue == difference between linear model and number of errors
PO.res=resid(POrelation) #error residue == difference between linear model and number of errors
A.res=resid(Arelation) #error residue == difference between linear model and number of errors


######################################################################
residues<-cbind(get(dfname),E.res=error.res, rf.res, PO.res, A.res)
print(residues[order(residues$E.res), ] )
pE<-c(residues$E.res/residues$Chance) #What percent of chances is the error residue
residues<-cbind(residues,pE)
print(residues[order(residues$pE), ] )

people <- select(tbl_df(People), playerID, nameLast, nameFirst)#add names
residues <- merge(residues, people, all.x = TRUE)
print(residues[order(residues$pE), ] )

topp <- filter(residues, (rank(pE)<=100))
print(topp[order(topp$pE), ] )

