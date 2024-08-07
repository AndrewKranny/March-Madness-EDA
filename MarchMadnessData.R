rm(list = ls())
library('tidyverse')

# Describing Raw Variables

# Tempo -> number of possessions per 40 minutes
#       -> possessions is estimated becuase it 
#       -> is not an official NCAA stat
#       -> formula for possessions
#       -> FGA-OR+TO+0.42*FTA, where FTA = 0.475 (uncertain)

# Offensive Efficiency -> # of points per 100 possesstions
# Defensive Efficiency -> # of points allowed per 100 possessions

# AdjEM -> Adjusted efficiency margin
# AdjO -> Offensive efficiency adjusted for opponents
# AdjD -> Defensive efficiency adjested for opponents
# AdjT -> Adjusted tempo (possessions / 40 minutes)

# Importing data
(season23 <- read_csv('summary23.csv'))
(offfour <- read_csv('offenseff23.csv'))
(deffour <- read_csv('defenseff23.csv'))
(pointdist <- read_csv('pointdist23.csv'))
(offmisc <- read_csv('miscoff23.csv'))
(defmisc <- read_csv('miscdef23.csv'))
(winloss <- read_csv('winloss23.csv'))
(height <- read_csv('height23.csv'))

# Editing winloss dataset to be tidy and more manageable
winloss_fixed <- winloss %>%
  separate(winloss, into = c("win", "loss"), sep = "/") %>%
  mutate(win=parse_number(win),loss = parse_number(loss)) %>%
  mutate(winrate = (win / (win+loss))*100)

# Editing the datasets to include win, loss, and winrate
seasonWin <- left_join(season23, winloss_fixed)
heightWin <- left_join(height, winloss_fixed)
offfourWin <- left_join(offfour, winloss_fixed)
offmiscWin <- left_join(offmisc, winloss_fixed)
deffourWin <- left_join(deffour, winloss_fixed)

# Creating a new dataset with the percentage of each stat according
# to the highest value in each stat
# stat / max(stat)
percStat <- mutate(season23,
    tempoPerc = AdjTempo / max(AdjTempo),
    AdjOEPerc = AdjOE / max(AdjOE),
    AdjDEPerc = 1-(AdjDE / max(AdjDE)),
    AdjEMPerc = AdjEM / max(AdjEM)) %>%
  select(-(Tempo:RankAdjEM)) %>%
  mutate(offfour,
         OeFGPctPerc = eFGPct / max(eFGPct),
         OTOPctPerc = 1-(TOPct / max(TOPct)),
         OFTRatePerc = FTRate / max(FTRate)) %>%
  select(-(eFGPct:RankFTRate)) %>%
  mutate(deffour,
         DeFGPctPerc = 1-(eFGPct / max(eFGPct)),
         DTOPctPerc = TOPct / max(TOPct),
         DFTRatePerc = 1-(FTRate / max(FTRate)) %>%
  select(-(eFGPct:RankFTRate)) %>%
  mutate(offmisc,
         FG2PctPerc = FG2Pct / max(FG2Pct),
         FG3PctPerc = FG3Pct / max(FG3Pct),
         FTPctPerc = FTPct / max(FTPct),
         BlkPctPerc = BlockPct / max(BlockPct),
         FG3RatePerc = FG3Rate / max(FG3Rate),
         ARatePerc = ARate / max(ARate),
         StlRatePerc = StlRate / max(StlRate),
         NSTRatePerc = 1-(NSTRate / max(NSTRate)),
         OppFG2PctPerc = 1-(OppFG2Pct / max(OppFG2Pct)),
         OppFG3PctPerc = 1-(OppFG3Pct / max(OppFG3Pct)),
         OppFTPctPerc = 1-(OppFTPct / max(OppFTPct)),
         OppBlkPctPerc = 1-(OppBlockPct / max(OppBlockPct)),
         OppFG3RatePerc = 1-(OppFG3Rate / max(OppFG3Rate)),
         OppARatePerc = 1-(OppARate / max(OppARate)),
         OppStlRatePerc = 1-(OppStlRate / max(OppStlRate)),
         OppNSTRatePerc = OppNSTRate / max(OppNSTRate)) %>%
  select(-(FG2Pct:RankOppNSTRate)) %>%
  mutate(height, 
         SizePerc = Size / max(Size)) %>%
  select(-(Size:DR1Rank)))
  

# Stats from SeasonWin dataset
# Relationship between Tempo and winrate

plotTempo <- ggplot(data = seasonWin, mapping = aes(x = Tempo, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
ggsave("plotTempo.pdf", width = 8, height = 8)
(mod_Tempo <- lm(seasonWin$winrate ~ seasonWin$Tempo)$coefficients["seasonWin$Tempo"])
summary(lm(seasonWin$winrate ~ seasonWin$Tempo))
# Relationship between Offensive Efficiency and Winrate
plotAdjOE <- ggplot(data = seasonWin, mapping = aes(x = AdjOE, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_AdjOE <- lm(seasonWin$winrate ~ seasonWin$AdjOE)$coefficients["seasonWin$AdjOE"])
summary(lm(seasonWin$winrate ~ seasonWin$AdjOE))
# Relationship between Defensive Efficiency and Winrate
# Convert negatives
plotAdjDE <- ggplot(data = seasonWin, mapping = aes(x = -(AdjDE), y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_AdjDE <- -(lm(seasonWin$winrate ~ seasonWin$AdjDE)$coefficients["seasonWin$AdjDE"]))
summary(lm(seasonWin$winrate ~ seasonWin$AdjDE))
# Relationship between strength of schedule and Winrate
plotAdjEM <- ggplot(data = seasonWin, mapping = aes(x = AdjEM, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_AdjEM <- lm(seasonWin$winrate ~ seasonWin$AdjEM)$coefficients["seasonWin$AdjEM"])
summary(lm(seasonWin$winrate ~ seasonWin$AdjEM))


# Stats from offfourWin dataset
# Relationship between Effective field goal percent and winrate
ploteFGPct <- ggplot(data = offfourWin, mapping = aes(x = eFGPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_eFGPct <- lm(offfourWin$winrate ~ offfourWin$eFGPct)$coefficients["offfourWin$eFGPct"])
summary(lm(offfourWin$winrate ~ offfourWin$eFGPct))
# Relationship between Turnover Percentage and winrate
# Convert Negatives
plotTOPct <- ggplot(data = offfourWin, mapping = aes(x = -TOPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_TOPct <- -(lm(offfourWin$winrate ~ offfourWin$TOPct)$coefficients["offfourWin$TOPct"]))
summary((lm(offfourWin$winrate ~ offfourWin$TOPct)))
# Relationship between ORPct (Offensive Rating Pct) and winrate
plotORPct <- ggplot(data = offfourWin, mapping = aes(x = ORPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_ORPct <- (lm(offfourWin$winrate ~ offfourWin$ORPct)$coefficients["offfourWin$ORPct"]))
summary(lm(offfourWin$winrate ~ offfourWin$ORPct))
# Relationship between Free Throw rate and winrate
plotFTRate <- ggplot(data = offfourWin, mapping = aes(x = FTRate, y = winrate))+
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_FTRate <- lm(offfourWin$winrate ~ offfourWin$FTRate)$coefficients["offfourWin$FTRate"])
summary(lm(offfourWin$winrate ~ offfourWin$FTRate))


# Stats from Deffour dataset
# Relationship between Defensive eFGPct and winrate
plotDeFGPct <- ggplot(data = deffourWin, mapping = aes(x = -eFGPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm")
(mod_DeFGPct <- -(lm(deffourWin$winrate ~ deffourWin$eFGPct)$coefficients["deffourWin$eFGPct"]))
summary(lm(deffourWin$winrate ~ deffourWin$eFGPct))
# Relationship between Defensive Turnover Percentage and winrate
# Convert Negatives
plotDTOPct <- ggplot(data = deffourWin, mapping = aes(x = TOPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_DTOPct <- (lm(deffourWin$winrate ~ deffourWin$TOPct)$coefficients["deffourWin$TOPct"]))
summary(lm(deffourWin$winrate ~ deffourWin$TOPct))
# Relationship between Defensive Free Throw rate and winrate
plotDFTRate <- ggplot(data = deffourWin, mapping = aes(x = -FTRate, y = winrate))+
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_DFTRate <- -(lm(deffourWin$winrate ~ deffourWin$FTRate)$coefficients["deffourWin$FTRate"]))
summary(lm(deffourWin$winrate ~ deffourWin$FTRate))


# Stats from offmiscWin dataset
# Relationship between 2 point field goal pct and winrate
plotFG2Pct <- ggplot(data = offmiscWin, mapping = aes(x = FG2Pct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_FG2Pct <- lm(offmiscWin$winrate ~ offmiscWin$FG2Pct)$coefficients["offmiscWin$FG2Pct"])
summary(lm(offmiscWin$winrate ~ offmiscWin$FG2Pct))
# Relationship between 3 point field goal pct and winrate
plotFG3Pct <- ggplot(data = offmiscWin, mapping = aes(x = FG3Pct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_FG3Pct <- lm(offmiscWin$winrate ~ offmiscWin$FG3Pct)$coefficients["offmiscWin$FG3Pct"])
summary(lm(offmiscWin$winrate ~ offmiscWin$FG3Pct))
# Relationship between free throw percentage and winrate
plotFTPct <- ggplot(data = offmiscWin, mapping = aes(x = FTPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_FTPct <- lm(offmiscWin$winrate ~ offmiscWin$FTPct)$coefficients["offmiscWin$FTPct"])
summary(lm(offmiscWin$winrate ~ offmiscWin$FTPct))
# Relationship between Block Pct and winrate
plotBlockPct <- ggplot(data = offmiscWin, mapping = aes(x = BlockPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_BlockPct <- lm(offmiscWin$winrate ~ offmiscWin$BlockPct)$coefficients["offmiscWin$BlockPct"])
summary(lm(offmiscWin$winrate ~ offmiscWin$BlockPct))
# Relationship between 3 point FG rate and winrate
plotFG3Rate <- ggplot(data = offmiscWin, mapping = aes(x = FG3Rate, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_FG3Rate <- lm(offmiscWin$winrate ~ offmiscWin$FG3Rate)$coefficients["offmiscWin$FG3Rate"])
summary(lm(offmiscWin$winrate ~ offmiscWin$FG3Rate))
# Relationship between Assist Rate and winrate
plotARate <- ggplot(data = offmiscWin, mapping = aes(x = ARate, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_ARate <- lm(offmiscWin$winrate ~ offmiscWin$ARate)$coefficients["offmiscWin$ARate"])
summary(lm(offmiscWin$winrate ~ offmiscWin$ARate))
# Relationship between non steal turnover percentage
plotNSTRate <- ggplot(data = offmiscWin, mapping = aes(x = -NSTRate, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_NSTRate <- -(lm(offmiscWin$winrate ~ offmiscWin$NSTRate)$coefficients["offmiscWin$NSTRate"]))
summary(lm(offmiscWin$winrate ~ offmiscWin$NSTRate))
# Relationship between Opposing team 2 point field goal pct and winrate
plotOppFG2Pct <- ggplot(data = offmiscWin, mapping = aes(x = -OppFG2Pct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_OppFG2Pct <- -(lm(offmiscWin$winrate ~ offmiscWin$OppFG2Pct)$coefficients["offmiscWin$OppFG2Pct"]))
summary(lm(offmiscWin$winrate ~ offmiscWin$OppFG2Pct))
# Relationship between Opposing team 3 point field goal pct and winrate
plotOppFG3Pct <- ggplot(data = offmiscWin, mapping = aes(x = -OppFG3Pct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_OppFG3Pct <- -(lm(offmiscWin$winrate ~ offmiscWin$OppFG3Pct)$coefficients["offmiscWin$OppFG3Pct"]))
summary(lm(offmiscWin$winrate ~ offmiscWin$OppFG3Pct))
# Relationship between opposing free throw percentage and winrate
plotOppFTPct <- ggplot(data = offmiscWin, mapping = aes(x = -OppFTPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_OppFTPct <- -(lm(offmiscWin$winrate ~ offmiscWin$OppFTPct)$coefficients["offmiscWin$OppFTPct"]))
summary(lm(offmiscWin$winrate ~ offmiscWin$OppFTPct))
# Relationship between Opposing Block percentage and winrate
plotOppBlockPct <- ggplot(data = offmiscWin, mapping = aes(x=-OppBlockPct, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_OppBlkPct <- -(lm(offmiscWin$winrate ~ offmiscWin$OppBlockPct)$coefficients["offmiscWin$OppBlockPct"]))
summary(lm(offmiscWin$winrate ~ offmiscWin$OppBlockPct))
# Relationship between Opposing 3pt field goal rate and winrate
plotOppFG3Rate <- ggplot(data = offmiscWin, mapping = aes(x=-OppFG3Rate, y=winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_OppFG3Rate <-  -(lm(offmiscWin$winrate ~ offmiscWin$OppFG3Rate)$coefficients["offmiscWin$OppFG3Rate"]))
summary(lm(offmiscWin$winrate ~ offmiscWin$OppFG3Rate))
# Relationship between Opposing assist rate and winrate
plotOppARate <- ggplot(data = offmiscWin, mapping = aes(x=-OppARate, y = winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_OppARate <- -(lm(offmiscWin$winrate ~ offmiscWin$OppARate)$coefficients["offmiscWin$OppARate"]))
summary(lm(offmiscWin$winrate ~ offmiscWin$OppARate))
# Relationship between Opposing Non Steal Turnover Percentage and winrate
plotOppNSTRate <- ggplot(data = offmiscWin, mapping = aes(x=OppNSTRate, y=winrate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_OppNSTRate <- lm(offmiscWin$winrate ~ offmiscWin$OppNSTRate)$coefficients["offmiscWin$OppNSTRate"])
summary(lm(offmiscWin$winrate ~ offmiscWin$OppNSTRate))


# Stats from Height dataset
# Relationship between Height and winrate
plotSize <- ggplot(data = heightWin, mapping = aes(x = Size, y=winrate))+
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(mapping = aes(label = TeamName), size = 2.3)
(mod_Size <- lm(heightWin$winrate ~ heightWin$Size)$coeffecients["heightwin$Size"])
summary(lm(heightWin$winrate ~ heightWin$Size))


# Storing all plots in a pdf
allplots <- list(plotTempo, plotAdjOE, plotAdjDE, plotAdjEM, ploteFGPct,
                 plotTOPct, plotFTRate, plotDeFGPct, plotDTOPct,plotDFTRate,
                 plotFG2Pct, plotFG3Pct, plotFTPct, plotBlockPct, plotFG3Rate,
                 plotARate, plotNSTRate, plotOppFG2Pct, plotOppFG3Pct,
                 plotOppFTPct, plotOppBlockPct, plotOppFG3Rate, plotOppARate,
                 plotOppNSTRate, plotSize)
pdf("MarchMadnessPlots.pdf")
print(allplots)
dev.off()


# Determining which stats are statistically significant
# First create a new dataset that includes all of the stats from each dataset
total1 <- left_join(seasonWin, offfourWin)
totalx <- deffourWin %>%
  rename("DeFGPct" = "eFGPct", "DTOPct" = "TOPct", "DFTRate" = "FTRate")
total2 <- left_join(total1, deffourWin)
total3 <- left_join(total2, offmiscWin)
totalWin <- left_join(total3, heightWin)
totalWinModel <- lm(totalWin$winrate ~ totalWin$Tempo + totalWin$AdjOE -
                      totalWin$AdjDE + totalWin$AdjEM)
summary(totalWinModel)

percStat %>%
  mutate(rankScore = tempoPerc*mod_Tempo + AdjOEPerc*mod_AdjOE + AdjDEPerc*mod_AdjDE +
           AdjEMPerc*mod_AdjEM + OeFGPctPerc*mod_eFGPct + OTOPctPerc*mod_TOPct +
           OFTRatePerc*mod_FTRate + DeFGPctPerc*mod_DeFGPct + DTOPctPerc*mod_DTOPct +
           DFTRatePerc*mod_DFTRate + FG2PctPerc*mod_FG2Pct + FG3PctPerc*mod_FG3Pct +
           FTPctPerc*mod_FTPct + BlkPctPerc*mod_BlkPct + FG3RatePerc*mod_FG3Rate+ 
           ARatePerc*mod_ARate + NSTRatePerc*mod_NSTRate + OppFG2PctPerc*mod_OppFG2Pct +
           OppFG3PctPerc*mod_OppFG3PctPerc + OppFTPctPerc*mod_OppFTPct + OppBlkPctPerc*mod_OppBlkPct +
           OppFG3RatePerc*mod_OppFG3Rate + OppARatePerc*mod_OppARate + OppNSTRate*mod_OppNSTRate +
           SizePerc*mod_Size)

