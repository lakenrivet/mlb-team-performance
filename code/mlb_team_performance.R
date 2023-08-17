# Assessing MLB Team Performance
# Laken Rivet

# load necessary libraries
library(tidyverse)
library(Lahman)
library(broom)
library(ggrepel)
library(writexl)

# PART 1 - DETERMINE EXPONENT IN PYTHAGOREAN WINS FORMULA

teams <- read.csv(file = "raw_data/Teams.csv", header = TRUE)
tail(teams)

# filter team performance data for 2011-2021 with relevant variables
my_teams <- teams %>%
  filter(yearID > 2011) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA)

# check for 10 years total
my_teams %>% head()
my_teams %>% tail()

# create run differential (RD) and win percentage (Wpct) variables
my_teams <- my_teams %>% 
  mutate(RD = R - RA, 
         Wpct = W / (W + L))

# create separate data set for american league teams only
al <- my_teams %>%
  filter(lgID == "AL")

# create separate data set for national league teams only
nl <- my_teams %>%
  filter(lgID == "NL")

# create and save visualization showing relationship between run differential
# and win percentage
run_diff <- ggplot(my_teams, aes(x = RD, y = Wpct)) +
  geom_point() +
  labs(title = "Winning Percentage vs Run Differential in the MLB") +
  scale_x_continuous("Run Differential") +
  scale_y_continuous("Winning Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# display visualization
run_diff

# fit regression line to quantify relationship between RD and Wpct
linfit <- lm(Wpct ~ RD, data = my_teams)

# display regression line information
linfit
summary(linfit)

# display previous visualization with regression line fitted
run_diff +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = 'blue', size = 0.5)

# save run differential graphic
ggsave(filename = "output/run_diff.pdf")

# calculate predicted values and residuals
my_teams_aug <- augment(linfit, data = my_teams)

# calculate RMSE for linear model
resid_summary <- my_teams_aug %>%
  summarize(N = n(), avg = mean(.resid), RMSE = sqrt(mean(.resid^2)))

# display summary
resid_summary

# calculate regression lines for al and nl subsets
alfit <- lm(Wpct ~ RD, data = al)
nlfit <- lm(Wpct ~ RD, data = nl)

# display results of al and nl regression lines
alfit
nlfit

# calculate correlation between RD and Wpct
cor.test(x = my_teams$RD, y = my_teams$Wpct)

# calculate correlation coefficients for al and nl subsets
cor.test(x = al$RD, y = al$Wpct)
cor.test(x = nl$RD, y = nl$Wpct)

# apply the Pythagorean (PYT) formula for winning percentage
my_teams <- my_teams %>% 
  mutate(Wpct_pyt = R ^ 2 / (R ^ 2 + RA ^ 2))

# calculate residuals of predictions from PYT formula
my_teams <- my_teams %>% 
  mutate(residuals_pyt = Wpct - Wpct_pyt)

# calculate RMSE for PYT
my_teams %>%
  summarize(rmse = sqrt(mean(residuals_pyt ^ 2)))

# create log ratios of wins to losses and runs to runs allowed to calculate exponent
my_teams <- my_teams %>%
  mutate(logWratio = log(W / L),
         logRratio = log(R / RA))

# use a linear model (the 0 indicates there is no intercept) to predict the best
# exponent to minimize residuals
pytFit <- lm(logWratio ~ 0 + logRratio, data = my_teams)

# display results
pytFit

# apply the UPDATED PYT formula with new exponent (1.798)
my_teams <- my_teams %>% 
  mutate(Wpct_updated = R ^ 1.783 / (R ^ 1.783 + RA ^ 1.783))

# calculate residuals of predictions from PYT formula
my_teams <- my_teams %>% 
  mutate(residuals_updated = Wpct - Wpct_updated)

# calculate RMSE for PYT
my_teams %>%
  summarize(rmse = sqrt(mean(residuals_updated ^ 2)))

# check if the 2022 SF Giants are over- or under-performing
my_teams %>% 
  filter(teamID == "SFN", yearID == "2022")

# PART 2 - THREE PLAYERS COMPARED TO THE LEAGUE

# wOBA = Weighted on-base average
# RE24 = Run expectancy based on 24 base-out states
# O-Swing% = swings at pitches outside the zone / pitches outside the zone
# Z-Swing% = swings at pitches inside the zone / pitches inside the zone

# read in advanced batting stats from FanGraphs
leaguestats <- read.csv(file = "raw_data/2022_Stats_FanGraphs.csv", header = TRUE)

# rename O-Swing% and Z-Swing% columns for easier calculations later
leaguestats <- leaguestats %>%
  rename(Z_Swing = Z.Swing., O_Swing = O.Swing.)

# Alter format of O-Swing% and Z-Swing% to remove '%' sign and convert to numeric
leaguestats$O_Swing = as.numeric(gsub("[\\%,]", "", leaguestats$O_Swing))
leaguestats$Z_Swing = as.numeric(gsub("[\\%,]", "", leaguestats$Z_Swing))

# read in batting data from Lahman's Database 
batting <- read.csv(file = "raw_data/Batting.csv", header = TRUE)

# identify 3 players with most at bats for the 2022 season
# (1) Wilmer Flores - 525 AB
# (2) Thairo Estrada - 488 AB
# (3) Mike Yastrzemski - 485 AB
batting %>% filter(yearID == 2022 & teamID == "SFN") %>%
  arrange(desc(AB)) %>% 
  head(3)

# make sure data set has information for all three players
leaguestats %>% 
  filter(Team == "SFG") %>%
  arrange(desc(AB))

# create rank columns
leaguestats$wOBA_rank <- rank(-leaguestats$wOBA, ties.method = "min")
leaguestats$RE24_rank <- rank(-leaguestats$RE24, ties.method = "min")
leaguestats$Z_Swing_rank <- rank(-leaguestats$Z_Swing, ties.method = "min")

# Because it's better for O-Swing% to be lower, ranking must be reversed
leaguestats$O_Swing_rank <- rank(leaguestats$O_Swing, ties.method = "min")

# subset SF players to highlight in graphic
highlight_players <- leaguestats %>%
  filter(Team == "SFG") %>%
  arrange(desc(AB))

# wOBA visualization
ggplot(data = leaguestats, aes(x = AB, y = wOBA)) +
  geom_point() +
  geom_hline(yintercept = mean(leaguestats$wOBA), color = "red", linetype = "dashed") +
  labs(x = "At-Bats", y = "wOBA", title = "wOBA vs At-Bats in the MLB") +
  geom_point(data = highlight_players, color = "blue") +
  geom_label_repel(data = highlight_players, color = "blue", aes(label = paste(Name))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# save graphic
ggsave(filename = "output/wOBA.pdf")

# RE24 visualization
ggplot(data = leaguestats, aes(x = AB, y = RE24)) +
  geom_point() +
  geom_hline(yintercept = mean(leaguestats$RE24), color = "red", linetype = "dashed") +
  labs(x = "At-Bats", y = "RE24", title = "RE24 vs At-Bats in the MLB") +
  geom_point(data = highlight_players, color = "blue") +
  geom_label_repel(data = highlight_players, color = "blue", aes(label = paste(Name))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# save graphic
ggsave(filename = "output/RE24.pdf")

# O-Swing % Visualization
ggplot(data = leaguestats, aes(x = AB, y = O_Swing)) +
  geom_point() +
  geom_hline(yintercept = mean(leaguestats$O_Swing), color = "red", linetype = "dashed") +
  labs(x = "At-Bats", y = "O-Swing Percentage", title = "O-Swing Percentage vs At-Bats in the MLB") +
  geom_point(data = highlight_players, color = "blue") +
  geom_label_repel(data = highlight_players, color = "blue", aes(label = paste(Name))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_reverse()

# save graphic
ggsave(filename = "output/O_Swing.pdf")

# Z-Swing % Visualization
ggplot(data = leaguestats, aes(x = AB, y = Z_Swing)) +
  geom_point() +
  geom_hline(yintercept = mean(leaguestats$Z_Swing), color = "red", linetype = "dashed") +
  labs(x = "At-Bats", y = "Z-Swing Percentage", title = "Z-Swing Percentage vs At-Bats in the MLB") +
  geom_point(data = highlight_players, color = "blue") +
  geom_label_repel(data = highlight_players, color = "blue", aes(label = paste(Name))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# save graphic
ggsave(filename = "output/Z_Swing.pdf")

# create separate data frame with specified stats for report
exportstats <- leaguestats %>%
  filter(Team == "SFG") %>%
  select(Name, AB, wOBA, wOBA_rank, RE24, RE24_rank, O_Swing, O_Swing_rank, Z_Swing, Z_Swing_rank)

# export data frame to excel to make nicely formatted table for report
write_xlsx(exportstats, "output/fourstats.xlsx")

# PART 3 - PICK A NEW PLAYER AND CALCULATE ADDITIONAL RUNS

# read in people data from Lahman's Database 
people <- read.csv(file = "raw_data/People.csv", header = TRUE)

# only grab the necessary columns
people <- people %>%
  select(playerID, nameGiven, birthYear, weight, height, bats, throws)

# join with batting data set
batting <- left_join(batting, people, by = "playerID")

# only take batting stats from 2022 season and players who actually batted
batting2022 <- batting %>%
  filter(yearID == "2022", AB > 0)

# create team data set to create team season data runs model
teambats <- teams %>%
  select(yearID, lgID, teamID, AB, G, W, L, R, H, X2B, X3B, HR, BB, HBP, SB, CS, IPouts, E) %>%
  filter(yearID > 2011) %>%
  mutate(X1B = H - (X2B + X3B + HR), WLKS = BB + HBP) 

# create team season data run model
runmodel <- lm(R ~ X1B + X2B + X3B + HR + WLKS + SB + CS, data = teambats)

# check to make sure all coefficients have significant p-values
summary(runmodel)

# calculate predicted runs of average team in MLB (2012-2022)
avgteam <- teambats %>%
  select(AB, G, X1B, X2B, X3B, HR, WLKS, SB, CS, IPouts, E) %>%
  # calculate avg number of outs per game by dividing outs pitched (which equals innings pitched * 3)
  # by the number of games played (for this data set avg outs per game = 26.66128)
  mutate(Outs = IPouts / G) %>%
  colMeans()

# review average team stats
avgteam <- data.frame(t(avgteam))

# calculate errors / at bats
sum(teambats$E) / sum(teambats$AB)
# for data set, approximately 1.7% (0.01700398) of all at bats result in error

# first calculate runs above average for all three SFG players

# grab Wilmer Flores data to compare
flores <- batting2022 %>%
  filter(playerID == "florewi01")

# calculate Wilmer Flores' outs using formula from Mathletics Chapter 2
flores <- flores %>%
  mutate(pred_outs = (1-0.01700398)*AB - H + GIDP + SF + SH + CS, 
         pred_outs_per_game = pred_outs / 26.66128,
         WLKS = BB + HBP,
         X1B = H - (X2B + X3B + HR))

# calculate flores teammult
floresteammult <- (avgteam$IPouts - flores$pred_outs) / avgteam$IPouts

# calculate team stats with Flores' stats
flores_above_avg <- data.frame(X1B = c((avgteam$X1B * floresteammult) + flores$X1B),
                              X2B = c((avgteam$X2B * floresteammult) + flores$X2B),
                              X3B = c((avgteam$X3B * floresteammult) + flores$X3B),
                              HR = c((avgteam$HR * floresteammult) + flores$HR),
                              WLKS = c((avgteam$WLKS * floresteammult) + flores$WLKS),
                              SB = c((avgteam$SB * floresteammult) + flores$SB),
                              CS = c((avgteam$CS * floresteammult) + flores$CS))

# flores would add an additional 5.09 runs to an average MLB team
floresdiff <- predict(runmodel, newdata = flores_above_avg, type = "response") - predict(runmodel, newdata = avgteam, type = "response")

# re-run for Estrada
estrada <- batting2022 %>%
  filter(playerID == "estrath01")

# calculate Estrada's outs using formula from Mathletics Chapter 2
estrada <- estrada %>%
  mutate(pred_outs = (1-0.01700398)*AB - H + GIDP + SF + SH + CS, 
         pred_outs_per_game = pred_outs / 26.66128,
         WLKS = BB + HBP,
         X1B = H - (X2B + X3B + HR))

# calculate estrada teammult
estradateammult <- (avgteam$IPouts - estrada$pred_outs) / avgteam$IPouts

# calculate team stats with estrada's stats
estrada_above_avg <- data.frame(X1B = c((avgteam$X1B * estradateammult) + estrada$X1B),
                               X2B = c((avgteam$X2B * estradateammult) + estrada$X2B),
                               X3B = c((avgteam$X3B * estradateammult) + estrada$X3B),
                               HR = c((avgteam$HR * estradateammult) + estrada$HR),
                               WLKS = c((avgteam$WLKS * estradateammult) + estrada$WLKS),
                               SB = c((avgteam$SB * estradateammult) + estrada$SB),
                               CS = c((avgteam$CS * estradateammult) + estrada$CS))

# estrada would cost an average MLB team -2.40 runs
estradadiff <- predict(runmodel, newdata = estrada_above_avg, type = "response") - predict(runmodel, newdata = avgteam, type = "response")

# re-run for Yastrzemski
yas <- batting2022 %>%
  filter(playerID == "yastrmi01")

# calculate Yastrzemski's outs using formula from Mathletics Chapter 2
yas <- yas %>%
  mutate(pred_outs = (1-0.01700398)*AB - H + GIDP + SF + SH + CS, 
         pred_outs_per_game = pred_outs / 26.66128,
         WLKS = BB + HBP,
         X1B = H - (X2B + X3B + HR))

# calculate Yastrzemski teammult
yasteammult <- (avgteam$IPouts - yas$pred_outs) / avgteam$IPouts

# calculate team stats with yastrzemski's stats
yas_above_avg <- data.frame(X1B = c((avgteam$X1B * yasteammult) + yas$X1B),
                                X2B = c((avgteam$X2B * yasteammult) + yas$X2B),
                                X3B = c((avgteam$X3B * yasteammult) + yas$X3B),
                                HR = c((avgteam$HR * yasteammult) + yas$HR),
                                WLKS = c((avgteam$WLKS * yasteammult) + yas$WLKS),
                                SB = c((avgteam$SB * yasteammult) + yas$SB),
                                CS = c((avgteam$CS * yasteammult) + yas$CS))

# yastrzemski would add an additional 6.18 runs to an average MLB team
yasdiff <- predict(runmodel, newdata = yas_above_avg, type = "response") - predict(runmodel, newdata = avgteam, type = "response")

# calculate runs above average for potential new players

# identify Kolten Wong in data set
wong <- batting2022 %>%
  filter(playerID == "wongko01")

# calculate Kolten Wong's outs using formula from Mathletics Chapter 2
wong <- wong %>%
  mutate(pred_outs = (1-0.01700398)*AB - H + GIDP + SF + SH + CS, 
         pred_outs_per_game = pred_outs / 26.66128,
         WLKS = BB + HBP,
         X1B = H - (X2B + X3B + HR))

# calculate teammult
wongteammult <- (avgteam$IPouts - wong$pred_outs) / avgteam$IPouts

# calculate team stats with wong's stats
wong_above_avg <- data.frame(X1B = c((avgteam$X1B * wongteammult) + wong$X1B),
                              X2B = c((avgteam$X2B * wongteammult) + wong$X2B),
                              X3B = c((avgteam$X3B * wongteammult) + wong$X3B),
                              HR = c((avgteam$HR * wongteammult) + wong$HR),
                              WLKS = c((avgteam$WLKS * wongteammult) + wong$WLKS),
                              SB = c((avgteam$SB * wongteammult) + wong$SB),
                              CS = c((avgteam$CS * wongteammult) + wong$CS))

# wong would add an additional 6.26 runs to an average MLB team
wongdiff <- predict(runmodel, newdata = wong_above_avg, type = "response") - predict(runmodel, newdata = avgteam, type = "response")

# identify Gleyber Torres in data set
torres <- batting2022 %>%
  filter(playerID == "torregl01")

# calculate Gleyber Torres outs using formula from Mathletics Chapter 2
torres <- torres %>%
  mutate(pred_outs = (1-0.01700398)*AB - H + GIDP + SF + SH + CS, 
         pred_outs_per_game = pred_outs / 26.66128,
         WLKS = BB + HBP,
         X1B = H - (X2B + X3B + HR))

# calculate teammult
torresteammult <- (avgteam$IPouts - torres$pred_outs) / avgteam$IPouts

# calculate team stats with Gleyber Torres' stats
torres_above_avg <- data.frame(X1B = c((avgteam$X1B * torresteammult) + torres$X1B),
                             X2B = c((avgteam$X2B * torresteammult) + torres$X2B),
                             X3B = c((avgteam$X3B * torresteammult) + torres$X3B),
                             HR = c((avgteam$HR * torresteammult) + torres$HR),
                             WLKS = c((avgteam$WLKS * torresteammult) + torres$WLKS),
                             SB = c((avgteam$SB * torresteammult) + torres$SB),
                             CS = c((avgteam$CS * torresteammult) + torres$CS))

# Gleyber Torres would add an additional 5.58 runs to an average MLB team
torresdiff <- predict(runmodel, newdata = torres_above_avg, type = "response") - predict(runmodel, newdata = avgteam, type = "response")

# identify Jake Cronenworth in data set
cronen <- batting2022 %>%
  filter(playerID == "croneja01")

# calculate Jake Cronenworth outs using formula from Mathletics Chapter 2
cronen <- cronen %>%
  mutate(pred_outs = (1-0.01700398)*AB - H + GIDP + SF + SH + CS, 
         pred_outs_per_game = pred_outs / 26.66128,
         WLKS = BB + HBP,
         X1B = H - (X2B + X3B + HR))

# calculate teammult
cronenteammult <- (avgteam$IPouts - cronen$pred_outs) / avgteam$IPouts

# calculate team stats with Jake Cronenworth stats
cronen_above_avg <- data.frame(X1B = c((avgteam$X1B * cronenteammult) + cronen$X1B),
                                X2B = c((avgteam$X2B * cronenteammult) + cronen$X2B),
                                X3B = c((avgteam$X3B * cronenteammult) + cronen$X3B),
                                HR = c((avgteam$HR * cronenteammult) + cronen$HR),
                                WLKS = c((avgteam$WLKS * cronenteammult) + cronen$WLKS),
                                SB = c((avgteam$SB * cronenteammult) + cronen$SB),
                                CS = c((avgteam$CS * cronenteammult) + cronen$CS))

# Jake Cronenworth would add an additional 7.38 runs to an average MLB team
cronendiff <- predict(runmodel, newdata = cronen_above_avg, type = "response") - predict(runmodel, newdata = avgteam, type = "response")


# update 2022 seasonal data, dropping Estrada adding other players and calculating predicted runs

# first use original team data
SFG2022 <- teambats %>%
  filter(yearID == 2022 & teamID == "SFN")

predict(runmodel, newdata = SFG2022, type = "response")

# validate you get the same result using individual player batting data
individualSFG2022 <- batting2022 %>%
  filter(teamID == "SFN") %>%
  select(H, X2B, X3B, HR, BB, HBP, SB, CS) %>%
  mutate(X1B = H - (X2B + X3B + HR), WLKS = BB + HBP) %>%
  colSums() 

individualSFG2022 <- data.frame(t(individualSFG2022))

predict(runmodel, newdata = individualSFG2022, type = "response")

# now add wong and eliminate estrada
wongSFG2022 <- batting2022 %>%
  filter(teamID == "SFN" | playerID == "wongko01") %>%
  subset(playerID != "estrath01") %>%
  select(H, X2B, X3B, HR, BB, HBP, SB, CS) %>%
  mutate(X1B = H - (X2B + X3B + HR), WLKS = BB + HBP) %>%
  colSums() 

wongSFG2022 <- data.frame(t(wongSFG2022))

predict(runmodel, newdata = wongSFG2022, type = "response")

# now add torres and eliminate estrada
torresSFG2022 <- batting2022 %>%
  filter(teamID == "SFN" | playerID == "torregl01") %>%
  subset(playerID != "estrath01") %>%
  select(H, X2B, X3B, HR, BB, HBP, SB, CS) %>%
  mutate(X1B = H - (X2B + X3B + HR), WLKS = BB + HBP) %>%
  colSums() 

torresSFG2022 <- data.frame(t(torresSFG2022))

predict(runmodel, newdata = torresSFG2022, type = "response")

# now add cronenworth and eliminate estrada
cronenSFG2022 <- batting2022 %>%
  filter(teamID == "SFN" | playerID == "croneja01") %>%
  subset(playerID != "estrath01") %>%
  select(H, X2B, X3B, HR, BB, HBP, SB, CS) %>%
  mutate(X1B = H - (X2B + X3B + HR), WLKS = BB + HBP) %>%
  colSums() 

cronenSFG2022 <- data.frame(t(cronenSFG2022))

predict(runmodel, newdata = cronenSFG2022, type = "response")

# calculate updated win percentages with new players and without estrada

# Kolten Wong

# estimated win percentage for 2022 season with run model
712.3829 ^ 1.783 / (712.3829 ^ 1.783 + 697 ^ 1.783)
# calculate new estimated win percentage for 2022 season with Wong without Estrada
714.0494  ^ 1.783 / (714.0494  ^ 1.783 + 697 ^ 1.783)

# current predicted wins for 2022 season
(712.3829 ^ 1.783 / (712.3829 ^ 1.783 + 697 ^ 1.783)) * 162
# calculate predicted wins for 2022 season with Wong without Estrada
(714.0494 ^ 1.783 / (714.0494 ^ 1.783 + 697 ^ 1.783)) * 162

# Gleyber Torres

# estimated win percentage for 2022 season with run model
712.3829 ^ 1.783 / (712.3829 ^ 1.783 + 697 ^ 1.783)
# calculate new estimated win percentage for 2022 season with Torres without Estrada
725.0841  ^ 1.783 / (725.0841 ^ 1.783 + 697 ^ 1.783)

# current predicted wins for 2022 season
(712.3829 ^ 1.783 / (712.3829 ^ 1.783 + 697 ^ 1.783)) * 162
# calculate predicted wins for 2022 season with Torres without Estrada
(725.0841 ^ 1.783 / (725.0841 ^ 1.783 + 697 ^ 1.783)) * 162

# Jake Cronenworth 

# estimated win percentage for 2022 season with run model
712.3829 ^ 1.783 / (712.3829 ^ 1.783 + 697 ^ 1.783)
# calculate new estimated win percentage for 2022 season with Cronenworth without Estrada
734.2006  ^ 1.783 / (734.2006 ^ 1.783 + 697 ^ 1.783)

# current predicted wins for 2022 season
(712.3829 ^ 1.783 / (712.3829 ^ 1.783 + 697 ^ 1.783)) * 162
# calculate predicted wins for 2022 season with Cronenworth without Estrada
(734.2006 ^ 1.783 / (734.2006 ^ 1.783 + 697 ^ 1.783)) * 162
