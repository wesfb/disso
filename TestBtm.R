install.packages("BradleyTerry2")

library(BradleyTerry2)
foo <-  data("citations", package = "BradleyTerry2")
countsToBinomial(citations)

citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")

citeModel <- BTm(cbind(win1, win2), journal1, journal2, ~ journal,
                 id = "journal", data = citations.sf)
??BTm

BTm(outcome = cbind(win1, win2), player1 = journal1, player2 = journal2,
    formula = ~journal, id = "journal", data = citations.sf)

faltlizards <- flatlizards

data("flatlizards", package = "BradleyTerry2")

lizModel <- BTm(1, winner, loser, ~ SVL[..] + (1|..),
                data = flatlizards)

summary(lizModel)

example(flatlizards)


#in order to use the bradley- terry model in r we must re-write the data table as
#a data fram in which we have a winner column, a loser column and a count or frequency column
#called binomial frequency

#winner column
winner<-rep(c("Agassi", "Federer","Henman","Hewitt", "Roddick"), each=5)

#loser column 
loser<-rep(c("Agassi", "Federer","Henman","Hewitt", "Roddick"), times=5)

Agassi <- c(NA, 0,0,1,1)
Federer <-c(6,NA,3,9,5)
Henman <- c(0,1,NA,0,1)
Hewitt <- c(0,0,2,NA,3)
Roddick <- c(0,0,1,2, NA)

count<- c( Agassi, Federer,Henman,Hewitt, Roddick )

tennis<- data.frame(winner,loser,count)
tennis.table <- t(data.frame(Agassi,Federer,Henman,Hewitt, Roddick))
colnames(tennis.table)<- c("Agassi", "Federer","Henman","Hewitt", "Roddick")

tennis
as.table(tennis)

tennis1<- countsToBinomial(tennis.table)
tennis1

btmodel1 <- BTm(cbind(win1,win2), player1, player2, data= tennis1)
btmodel1

summary(btmodel1)

foo_ <- countsToBinomial(foo)

ref
player_i_factor <- factor(head_to_head_results$player_i)
player_j_factor <- factor(head_to_head_results$player_j)


gome_factor <- factor(baseball$home.team)
away_factor <- factor(baseball$away.team)

nlevels(away_factor)

