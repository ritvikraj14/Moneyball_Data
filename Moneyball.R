# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W) #there is a strong linear relation

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

str(moneyball)

# Regression model to predict runs scored using most important features
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

# Remove variable with least significance
RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

# Regression model to predict run allowed using pitching statistics
AllowedReg = lm(RS ~ OOBP + OSLG, data=moneyball)
summary(AllowedReg)

# corr for 2012 in playoff
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
cor(teamRank, wins2012)

# corr for 2013 in playoff
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2013)

# The correlation in the above two years between teamRank and wins2013 
# is of opposite sign. So, we can't use this to predict wins for teams in playoffs.

