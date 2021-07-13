#import libraries
library(lsr)
library(scales)
library(psych)
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

# A plot of kills of each game with jitter and opacity added to visualize region of central
# tendency
plot(jitter(rtz$kills,2), main ="Arteezy Kills", xlab = "Match Number", ylab = "Kills", col = alpha("black",0.4), pch = 16)
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
