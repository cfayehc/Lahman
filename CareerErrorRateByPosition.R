#Run this after ErrorRateByPosition2
library(plyr); library(dplyr)


summstats <- function(d) {
  require('plyr')
  NAmassage <- function(x) {
    # Takes a column vector and replaces NAs by zeros
    x[is.na(x)] <- 0
    x
  }
  vars <- c('G', 'InnOuts', 'PO', 'A', 'E', 'rf', 'rfg',
             'Chance', 'fieldp', 'E.res', 'PO.res', 'A.res', 'pE')
  d2 <- apply(d[, vars], 2, NAmassage)
  d2 <- if(is.vector(d2)) {as.data.frame(as.list(d2)) } else {
    as.data.frame(d2) }
#  d2 <- mutate(d2,
#               rf=rf/G,  #range factor / game
#               rfa=rfacta/G,  #range factor adjusted / game
#               Chn.G=Chance/G,  #chances per gam
#               E.rate.G=E.rate/G,  #E.rate/G
#               E.res.G=E.res/G,    #E.res/G
#  )
#  print(d2)
  data.frame(d)
}


careerTotals <- function(d) {
  require('plyr')
  sumstats <-
    as.data.frame(as.list(colSums(as.matrix(d[, 4:16, drop = FALSE]),
                                  na.rm = TRUE)))
  cstats <- with(d, data.frame(
    beginYear = min(yearID),
    endYear = max(yearID)
#    nyears = (endYear-beginYear)
#    nyears = sum(stint == 1L),
#    nteams = length(unique(teamID))  
    ))
  extrastats <- mutate(sumstats,
                       rfg=rf/G,
                       rfi=rf/InnOuts,
#                       rfga=(rf-E)/G,
                       fieldp=rf/Chance,  #range factor / game
                       E.rate=E/Chance,
                       pE = E.res/Chance,
#                       E.rate.G=E.rate/G,
                       E.res.G=E.res/G*162
#                       PA = AB + BB + HBP + SH + SF,                              # plate appearances
#                       TB = H + X2B + 2 * X3B + 3 * HR,                           # total bases
#                       SlugplayerBattingProfiles <- dlply(Batting, .(playerID), summstats)Pct = ifelse(AB > 0, round(TB/AB, 3), NA),             # slugging percentage
#                       OBP = ifelse(PA > 0,                                       # on-base percentage
#                                    round((H + BB + HBP)/(PA - SH - SF), 3), NA),
#                       OPS = round(OBP + SlugPct, 3),
 #                      BABIP = ifelse(AB > 0, round(H/(AB - SO), 3), NA)  
)
  cbind(cstats, extrastats)
#  d <- mutate(d,
#               rfg=rf/G,
#               rfga=(rf-E)/G,
#               fieldp=(P0+A)/Chance,  #range factor / game
 #              AChn=Chance/G,  #chances per gam
#               E.rate.G=E.rate/G,  #E.rate/G
 #              E.res.G=E.res/G,    #E.res/G
#  )
}



playerfieldingProfiles <- dlply(get(dfname), .(playerID),summstats)
#print(playerfieldingProfiles)
playerfieldingProfiles[['whitefr01']]
# Summarize into career summaries
# the output object is a *data frame*
careerfieldingProfiles <- ldply(playerfieldingProfiles, careerTotals)

# Individual player season records
# The difference in calls has to do with the types of objects
# from which one is extracting information
#playerfieldingProfiles[['whitefr01']]
#subset(careerfieldingProfiles, playerID == 'whitefr01') # Frank White

topp <- filter(careerfieldingProfiles, G>500)
topp <- filter(topp, (rank(E.res.G)<=30))
print(topp[order(-topp$rfg), ] )
