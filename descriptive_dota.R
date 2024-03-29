#import libraries
library(lsr)
library(scales)
library(psych)
library(ggplot2)
library(dplyr)
library(plyr)
library(BSDA)
#Read a dataset in csv format
rtz <- read.csv("/Users/udeshhabaraduwa/Google Drive/Misc/Dota2data/rtz.csv")
heroes <- read.csv("/Users/udeshhabaraduwa/Google Drive/Misc/Dota2data/heroes.csv")

#drop the X column in the rtz data. It's simply counting the number of matches.
rtz <- subset(rtz,select =-c(X,Unnamed..0))
rtz$hero_id <- as.factor(rtz$hero_id)
#similarly, we will convert the "localized_name" and id column to a factors
heroes$localized_name <- as.factor(heroes$localized_name)
heroes$id <- as.factor(heroes$id)
rtz$match_id <- as.factor(rtz$match_id)
#convert the durtaion from seconds to minutes
rtz$duration <- rtz$duration/60
#rename player slot to reflect radiant/dire
rtz$player_slot <- ifelse(rtz$player_slot <= 127,"radiant","dire")
#change skill column name since its not being used, to win/lose status
colnames(rtz)[13] <- "win_lose"
# Win/lose status for each match
rtz$win_lose<- ifelse((rtz$player_slot == "radiant") & (rtz$radiant_win == 1) | (rtz$player_slot == "dire") & (rtz$radiant_win == 0),"win","lose")
rtz$win_lose <- as.factor(rtz$win_lose)
#drop rows without results
rtz <- rtz[!is.na(rtz$win_lose),]
#drop unparsed replays
rtz <-dplyr::filter(rtz, hero_id != 0)
#===============================================================================
############
# LESSON 1 # - Descriptive Statistics
############

# A plot of kills of each game with jitter and opacity added to visualize region of central
# tendency
plot(jitter(rtz$kills,2), main ="Arteezy Kills", xlab = "Match Number", ylab = "Kills", col = scales::alpha("black",0.4), pch = 16)
# The mean kill score
abline(h = mean(rtz$kills), col = "red")

#calculate the mean and median
mean(rtz$kills) # or to do so manually : sum(rtz$kills)/nrow(rtz)
median(rtz$kills) # or to do so manually : sort(rtz$kills)[1296], sort(rtz$kills)[1297],(7+7)/2 

#calculate the (unbiased) sample variance
var(rtz$kills) # or to do it manually sum((rtz$kills - mean(rtz$kills))^2)/(nrow(rtz)-1)
sd(rtz$kills) # or to do it manually sqrt(sum((rtz$kills - mean(rtz$kills))^2)/(nrow(rtz)-1))

#Plot a histogram of Arteezy's kills
hist(rtz$kills, xlab = "Number of Kills", main = "Distribution of Arteezy's Kills", breaks = 50)

#skewness of kills
skew(rtz$kills)

#kurtosis of kills
kurtosi(rtz$kills)


#===============================================================================
############
# LESSON 2 # - Basic graphing and exploration.
############

#Arteezy kill score vs match duration. I've added a jitter to the kill scores - this adds
#some noise to the kill scores because otherwise they are all discrete values and will
#be on top of each other (i.e many games with exactly 6 kills)
plot(rtz$duration,jitter(rtz$kills,5), col = scales::alpha("black",0.15), pch = 16, xlab = "Match Duration (minutes)", ylab = "Kill Score (kills)", main = "Arteezy Kill Score Vs. Match Duration")

# Side-by-side box plot for Rtz kills and game result
boxplot(
  formula = kills ~ win_lose, # y as a function of x
  data = rtz,
  xlab = "Result",
  ylab = "Kill Score (kills)",
  frame.plot = FALSE, #turn off the frame in the plot
  whisklty = 1, # solid lines for the whiskers
  main = "Kill Score by Game Result",
  boxwex = .75, #make the boxes a little narrower
  boxfill = "grey80",
  whiskcol = "grey70",
  staplecol = "grey70",
  boxcol = "grey70", #the box borders
  outcol = "grey70", #the outliers
  medlwd = "1" # median line thickness
)
# Filter out Anti-mage, Lifestealer and SF games
ls_am_sf <- dplyr::filter(rtz, hero_id == "Lifestealer" | hero_id == "Anti-Mage" | hero_id == "Shadow Fiend")
#drop unused levels in the dataframe
ls_am_sf <- droplevels(ls_am_sf)
#get the mean score for each AM and LS respectively
ls_am_sf_mean <- as.data.frame(ls_am_sf %>% dplyr::group_by(hero_id) %>% dplyr::summarise(mean(kills)))
colnames(ls_am_sf_mean)[2] <- "Mean"

# Bar plot of mean kill score
ggplot(data = ls_am_sf_mean, aes(x = hero_id, y = Mean)) + 
  geom_col(fill = "grey") +
  #remove grid background, darken axis lines
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  ggtitle("Arteezy Mean Kill Scores for AM, LS and SF") +
  xlab("Hero Name") + 
  theme(plot.title = element_text(hjust = 0.5))

#stacked bar plot of kills and win/lose 
ggplot(ls_am_sf, aes(x = hero_id, fill = win_lose)) + geom_bar() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Proportions of Wins to Losses for 3 Heroes") +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Hero Name")+
  ylab("Count")+
  labs(fill = "Game Result")

#===============================================================================
############
# LESSON 3 & 4 # Hypothesis Testing, z-test
############
#rename hero name colum in RTZ
colnames(rtz)[5] <- "localized_name"
rtz_heroes <- merge(rtz,heroes, by = "localized_name")

#Grab 'm' samples of size 'n' and store the mean of each sample
sample_means <- c()
m = 1
n = 30
for (v in seq(1,m)) {
  s <- sample(rtz$kills, size = n, replace = TRUE)
  sample_means <- append(sample_means, mean(s))
}
sample_means <- as.data.frame(sample_means)
mean(sample_means$sample_means)
hist(sample_means$sample_means, xlab = "Mean Kill Score Of A Sample", main = "Sampling Distribution Of The Sample Means", breaks = 15)
shapiro.test(sample_means$sample_means)
#A null distribution of 10000 samples with mean 5 and standard deviation 
# equal to the standard error : population sigma / sqrt(sample size of one sample)
SE <- sd(rtz$kills)/sqrt(length(s))
null_dist <- rnorm(10000,5,sd = SE)
# histogram of the null distribution
hist(null_dist, xlab = "Sample Mean", main = "Distribution Of Sample Means for H0")
abline(v = 5, col = "blue", lwd = 2)
abline(v = mean(s), col= "red", lwd = 2)
y <- dnorm(x = null_dist, mean = 5, sd = SE)
plot(null_dist,y,xlab = "Sample mean", col = scales::alpha("black",0.01 ), pch = 16)

# Get Arteezy's matches while on Team Secret
ts1 <- as.Date("2015-01-06")
ts2 <- as.Date("2015-08-14")
ts_matches <- rtz_heroes[rtz_heroes$start_time >= ts1 & rtz_heroes$start_time <= ts2,]

# Get Arteezy's matches while on EG (after playing on TS)
eg1 <- as.Date("2016-09-15")
eg2 <- as.Date("2020-08-14")
eg_matches <- rtz_heroes[rtz_heroes$start_time >= eg1 & rtz_heroes$start_time <= eg2,]
z_score = (7.633 - 7.982)/(4.673/32.78)
#plot a standard normal distribution
x <- seq(-4,4, by = 0.1)
y <- dnorm(x, mean = 0, sd = 1)
plot(x,y, type = 'l', main = "The Standard Normal (Z) Distribution",
     xlab = "z-statistic",
     ylab = "Probability Density")
#add the z-score and rejection regions
abline(v = z_score, col = "red")
text("-2.448", x = z_score, y = max(y), srt= 90, pos = 2, col = "red")
abline(v = 1.96, col = "blue")
text("1.96", x = 1.96, y = max(y), srt= 90, pos = 2, col = "blue")
abline(v = -1.96, col = "blue")
text("-1.96", x = -1.96, y = max(y), srt= 90, pos = 2, col = "blue")
# run the complete z.test
z.test(x = eg_matches$kills, mu = mean(ts_matches$kills), sigma.x = 4.673)
# run the complete t test
t.test(x = eg_matches$kills, mu = mean(ts_matches$kills))
