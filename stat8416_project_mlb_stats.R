# Baseball Statistics
# STAT 8416 Intro to Data Science
# Project Team:
# Marcus Spotanski, Christine Kunz, Adam Einsel, Sebastion, Nick Walker

library(ggplot2)
library(readxl)
# Setting up and Cleaning Data
baseball <- read.csv('Teams.csv')
baseball1970 <- subset(baseball, yearID >= 1970)
names(baseball1970)[names(baseball1970) == 'yearID'] <- 'year'
names(baseball1970) <- toupper(names(baseball1970))
columnsDrop <- c('GHOME', 'CG', 'SHO', 'SV', 'IPOUTS', 'NAME', 'PARK',
                 'TEAMIDLAHMAN45', 'TEAMIDRETRO', 'LGID', 'RANK',
                 'DIVWIN', 'WCWIN', 'WSWIN', 'LGWIN', 'DIVID',
                 'TEAMID', 'FRANCHID', 'BPF', 'PPF')
cleanData <- baseball1970[, !(names(baseball1970)) %in% columnsDrop]

# Add Columns for BA, OBP, X1B, SLG, OPS, Win Percentage
cleanData$BA = (cleanData$H) / (cleanData$AB)
cleanData$OBP = (cleanData$H + cleanData$BB + cleanData$HBP) /
  (cleanData$AB + cleanData$BB + cleanData$HBP + cleanData$SF)
cleanData$X1B = (cleanData$H - cleanData$X2B - cleanData$X3B - cleanData$HR)
cleanData$SLG = (cleanData$X1B + (2 * cleanData$X2B) + 
                   (3 * cleanData$X3B) + (4 * cleanData$HR)) / cleanData$AB
cleanData$OPS = cleanData$OBP + cleanData$SLG
cleanData$WIN_PCT = cleanData$W/cleanData$G
cleanData$LOSS_PCT = cleanData$L/cleanData$G
View(cleanData)

# Create subsets of data for top 8 teams from each year and bottom 8 teams of each year
# SORT DATA BY YEAR and W

library(dplyr)
library(data.table)
library(reshape2)
dt = data.table(cleanData)
setorder(dt, YEAR, -W)
View(head(dt))
dt = dt %>% as.data.frame
# Grab 5 rows with the biggest W for each YEAR 
top8 = Reduce(rbind, by(dt, dt['YEAR'], head, n = 8))
View(head(top8, n=20))
# Grab Bottom 5 rows with the lowest W for each YEAR
bottom8 = Reduce(rbind, by(dt, dt['YEAR'], tail, n = 8))
setorder(bottom8, YEAR, W)
View(head(bottom8, n=25))

# Convert Data into Long form
# locks = c('YEAR', 'TEAMIDBR')
# top8Wide = melt(top8, id=locks)
# bottom8Wide = melt(bottom8, id=locks)

# Find Averages for each stat of each data set 
top8Avg = top8 %>%
  group_by(YEAR) %>%
  summarise(across(colnames(top8[,-c(1,2,28)]) , mean), .groups = 'keep') %>%
  as.data.frame


bottom8Avg = bottom8 %>%
  group_by(YEAR) %>%
  summarise(across(colnames(top8[,-c(1,2,28)]) , mean), .groups = 'keep') %>%
  as.data.frame

View(head(bottom8Avg, n=3))

# Graph each data set's individual statistics with that of the other

# # Hits (H)
# ggplot(NULL, aes(x,y)) +
#   geom_line(data=top8Avg, aes(x=YEAR, y=H), col='dark green') +
#   geom_line(data=bottom8Avg, aes(x=YEAR, y=H), col='blue') +
#   ggtitle('Top 8 MLB Teams vs Bottom 8 MLB Teams: Hits') +
#   xlab('Year') + ylab('Season Avg of Hits (H)') +
#   theme(axis.title=element_text(size=13))
  
  # In this graph, you can see that in every year, even with a few close calls,
  # the Top 8 teams in the MLB in terms of Wins (W) every year outperform the
  # Bottom 8 teams. You will notice large dips in the data around years 1982,
  # 1995, and 2020. 1982 and 1994 were years where player strikes occurred,
  # and in 2020 only 60 games were played due to the COVID-19 Global Pandemic.

# Standardize Both Data sets for Comparison
ignore = c('YEAR', 'G', 'L', 'W', 'WIN_PCT', 'LOSS_PCT')
scaled_b8 = scale(bottom8Avg[,!(names(bottom8Avg) %in% ignore)], )
scaled_t8 = scale(top8Avg[,!(names(top8Avg) %in% ignore)], )

# Find average difference across all years
avgDiffer = colMeans(scaled_t8 - scaled_b8)

# Find 5 highest differences by taking absolute value of the differences
avgDiffer = sort(abs(avgDiffer), decreasing = TRUE)

library(kable)
View(head(avgDiffer))
View(as.data.frame(avgDiffer))
# The 6 stats with the greatest differences by Yearly Avg of the
# Top and Bottom 8 teams are:
  # FP, SLG, BA, ER, R, ERA

colors=c('Top 8'='dark green', 'Bottom 8'='blue')
# Graph Relationship to determine what good teams do better at Vs bad teams
# Fielding Percentage (FP)
ggplot(NULL, aes(x,y)) +
  geom_line(data=top8Avg, aes(x=YEAR, y=FP, color="Top 8")) +
  geom_line(data=bottom8Avg, aes(x=YEAR, y=FP, color='Bottom 8')) +
  ggtitle('Top 8 MLB Teams vs Bottom 8 MLB Teams: Fielding %') +
  xlab('Year') + ylab('Season Avg of Fielding % (FP)') +
  theme(axis.title=element_text(size=13)) +
  scale_color_manual(values = colors) + labs(color='Subsets of Teams')
  # Shows that on average the Top Teams in the MLB have a better FP than the
  # Worst teams in the league. This makes sense as FP is associated with making
  # the correct play to get a player out. Lower values in this field indicate
  # more mistakes and errors occurring on the field, leading to more runs for
  # the opposing team, and thus, more wins.

# Slugging Percentage (SLG)
ggplot(NULL, aes(x,y)) +
  geom_line(data=top8Avg, aes(x=YEAR, y=SLG, color='Top 8')) +
  geom_line(data=bottom8Avg, aes(x=YEAR, y=SLG, color='Bottom 8')) +
  ggtitle('Top 8 MLB Teams vs Bottom 8 MLB Teams: Slugging %') +
  xlab('Year') + ylab('Season Avg of Slugging % (SLG)') +
  theme(axis.title=element_text(size=13)) +
  scale_color_manual(values = colors) + labs(color='Subsets of Teams')
  # Shows that on average the Top Teams in the MLB have a higher SLG than the
  # Worst teams in the league. This also makes sense as SLG is a hitting
  # statistic that revolves around scoring runs, which correlates with winning
  # more games.

# Batting Average (BA)
ggplot(NULL, aes(x,y)) +
  geom_line(data=top8Avg, aes(x=YEAR, y=BA, color='Top 8')) +
  geom_line(data=bottom8Avg, aes(x=YEAR, y=BA, color='Bottom 8')) +
  ggtitle('Top 8 MLB Teams vs Bottom 8 MLB Teams: Batting Average') +
  xlab('Year') + ylab('Season Avg of Batting Average (BA)') +
  theme(axis.title=element_text(size=13)) +
  scale_color_manual(values = colors) + labs(color='Subsets of Teams')
  # This graph shows that on average the Top Teams in the MLB have a higher BA
  # than the Bottom Teams. Similar to the relationship with SLG, BA relates to
  # batters getting on base, which in turn leads to more opportunities to score.
  # One reason this may has a smaller difference than SLG is that while both
  # stats track hitting statistics, SLG weights its numbers based on extra base 
  # hits (doubles, triples, home runs). These vairables often change the
  # momentum in games and lead to runs more than just single base hits or walks.

# Earned Runs (ER)
ggplot(NULL, aes(x,y)) +
  geom_line(data=top8Avg, aes(x=YEAR, y=ER, color='Top 8')) +
  geom_line(data=bottom8Avg, aes(x=YEAR, y=ER, color='Bottom 8')) +
  ggtitle('Top 8 MLB Teams vs Bottom 8 MLB Teams: Earned Runs') +
  xlab('Year') + ylab('Season Avg of Earned Runs (ER)') +
  theme(axis.title=element_text(size=13)) +
  scale_color_manual(values = colors) + labs(color='Subsets of Teams')
  # This graph shows that the Bottom 8 Teams have a higher ER total than the Top
  # 8 teams on average every year in the MLB. There are 3 occurrences of
  # outliers which are important to note: 1972, 1981, 1994, and 2020. In the
  # season, the total games for each team were shortened to be 60 due to the
  # COVID-19 global pandemic. In each of the other 3 years, labor strikes held
  # by the players shortened the seasons. ER refers to how many runs are given 
  # up by your team, thus it makes sense that the teams that lose more than the 
  # rest of the league give up more runs than the best teams in the league.
  #  (fewer games = fewer total runs = fewer total Wins)

# Runs (R)
ggplot(NULL, aes(x,y)) +
  geom_line(data=top8Avg, aes(x=YEAR, y=R, color='Top 8')) +
  geom_line(data=bottom8Avg, aes(x=YEAR, y=R, color='Bottom 8')) +
  ggtitle('Top 8 MLB Teams vs Bottom 8 MLB Teams: Runs') +
  xlab('Year') + ylab('Season Avg of Runs (R)') +
  theme(axis.title=element_text(size=13)) +
  scale_color_manual(values = colors) + labs(color='Subsets of Teams')
  # This graph shows that on average every year in the MLB, the Top 8 teams
  # accumulate more Runs than the Bottom 8 teams. This aligns with earlier
  # findings with ER that the more Runs you score, the more likely your team is
  # to Win.

# Earned Runs Allowed (ERA)
ggplot(NULL, aes(x,y)) +
  geom_line(data=top8Avg, aes(x=YEAR, y=ERA, color='Top 8')) +
  geom_line(data=bottom8Avg, aes(x=YEAR, y=ERA, color='Bottom 8')) +
  ggtitle('Top 8 MLB Teams vs Bottom 8 MLB Teams: Earned Runs Allowed') +
  xlab('Year') + ylab('Season Avg of Earned Runs Allowed (ERA)') +
  theme(axis.title=element_text(size=13)) +
  scale_color_manual(values = colors) + labs(color='Subsets of Teams')




