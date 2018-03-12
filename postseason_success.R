#Source for data: The Lahman Baseball Database
#2016 Version
#Release Date: February 25, 2017

library(ggplot2)
require(mgcv)

# Upload BattingPost dataset. File Path will be changed depending on the user.
batting_post <- read.csv("C:/Users/Benjamin Wong/Desktop/BattingPost.csv")

# Upload Master dataset. File Path will be changed depending on the user
master <- read.csv("C:/Users/Benjamin Wong/Desktop/Master.csv")

# Compactly display datasets defined above
str(batting_post)
str(master)

# Delete all observations that are not in the 2016 postseason year
batting_post_new <- batting_post[ which(batting_post$yearID == 2016), ]

# aggregate(list(batting_post_new$G, batting_post_new$AB, batting_post_new$R, batting_post_new$H, batting_post_new$X2B, batting_post_new$X3B, batting_post_new$HR, batting_post_new$RBI, batting_post_new$SB, batting_post_new$CS, batting_post_new$BB, batting_post_new$SO, batting_post_new$IBB, batting_post_new$HBP, batting_post_new$SH, batting_post_new$SF, batting_post_new$GIDP), by = list(batting_post_new$playerID), sum)

# Merge the master dataset with the postseason batting dataset
merge_set <- merge(batting_post_new, master, by="playerID")

# Create a new birth year variable that accounts for player's age during postseason play
merge_set$birthYear_new <- ifelse(merge_set$birthMonth > 10, merge_set$birthYear + 1, merge_set$birthYear)

# Create variable of the player's age during the 2016 postseason
merge_set$age <- 2016 - merge_set$birthYear_new

# Create variable of the player's years of experience during the 2016 postseason
merge_set$experience <- 2016 - (1898 + as.integer(merge_set$debutYear))

# Sum the stats for each player into one row
agg_merge_set <- aggregate(list(G = merge_set$G, AB = merge_set$AB, R = merge_set$R, H = merge_set$H, X2B = merge_set$X2B, X3B = merge_set$X3B, HR = merge_set$HR, RBI = merge_set$RBI, SB = merge_set$SB, CS = merge_set$CS, BB = merge_set$BB, SO = merge_set$SO, IBB = merge_set$IBB, HBP = merge_set$HBP, SH = merge_set$SH, SF = merge_set$SF, GIDP = merge_set$GIDP), by = list(playerID = merge_set$playerID, age = merge_set$age, experience = merge_set$experience), sum)

# Calculate the wOBA for each player during the 2016 postseason
# agg_merge_set$wOBA <- (0.691*agg_merge_set$BB + 0.721*agg_merge_set$HBP + 0.878*(agg_merge_set$H-(agg_merge_set$X2B+agg_merge_set$X3B+agg_merge_set$HR)) + 1.242*agg_merge_set$X2B + 1.569*agg_merge_set$X3B + 2.015*agg_merge_set$HR) / agg_merge_set$AB + agg_merge_set$BB - agg_merge_set$IBB + agg_merge_set$SF + agg_merge_set$HBP
agg_merge_set$wOBA_numerator <- (0.691*agg_merge_set$BB + 0.721*agg_merge_set$HBP + 0.878*(agg_merge_set$H-(agg_merge_set$X2B+agg_merge_set$X3B+agg_merge_set$HR)) + 1.242*agg_merge_set$X2B + 1.569*agg_merge_set$X3B + 2.015*agg_merge_set$HR)
agg_merge_set$wOBA_denominator <- agg_merge_set$AB + agg_merge_set$BB - agg_merge_set$IBB + agg_merge_set$SF + agg_merge_set$HBP
agg_merge_set$wOBA <- agg_merge_set$wOBA_numerator / agg_merge_set$wOBA_denominator

# Remove players who do not have a wOBA. These are likely pitchers.
agg_merge_set <- na.omit(agg_merge_set)

data("agg_merge_set", package = "ggplot2")

# Ggplot for scatter plot: age v. wOBA
ggplot(agg_merge_set, aes(x=age, y=wOBA)) + 
  geom_point() + 
  geom_smooth(method="lm") + # set se=FALSE to turnoff confidence bands
  
  # Delete the points outside the limits
  xlim(c(18, 48)) + ylim(c(0.05, 0.525)) +   # deletes points
  
  # Add Title and Labels
  labs(title="MLB: Age vs. wOBA", subtitle="2016 Postseason", y="wOBA", x="Player Age")

# Ggplot for scatter plot: experience vs. age
ggplot(agg_merge_set, aes(x=experience, y=wOBA)) + 
  geom_point() + 
  geom_smooth(method="lm") + # set se=FALSE to turnoff confidence bands
  
  # Delete the points outside the limits
  xlim(c(0, 20)) + ylim(c(0.05, 0.75)) +   # deletes points 
  
  # Add Title and Labels
  labs(title="MLB: Years of Experience vs. wOBA", subtitle="2016 Postseason", y="wOBA", x="Player's Experience (years)")

# Ggplot for scatter plot: age vs. mean wOBA for each age
fill <- "#4271AE"
line <- "#1F3552"
agg_merge_set$age <- as.factor(agg_merge_set$age)

ggplot(agg_merge_set, aes(x=age, y=wOBA)) + 
  geom_boxplot(fill = fill, colour = line) +
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(name = "wOBA",
                     breaks = seq(0, 0.75, 0.1),
                     limits=c(0,0.75)) +
  scale_x_discrete(name = "Player Age") +
  ggtitle("Boxplot of mean wOBA by age for 2016 Postseason")

# # Calculate the mean wOBA for each age
# agg_merge_set_mean <- aggregate(wOBA ~ age, data=agg_merge_set, mean)
# 
# # Ggplot for scatter plot: age vs. mean wOBA for each age
# ggplot(agg_merge_set_mean, aes(x=age, y=wOBA)) + 
#   geom_point() + 
#   geom_smooth(method="lm") + # set se=FALSE to turnoff confidence bands
#   
#   # Delete the points outside the limits
#   #xlim(c(19, 41)) + ylim(c(0.05, 0.6)) +   # deletes points 
#   
#   # Add Title and Labels
#   labs(title="MLB: Age vs. Mean wOBA", subtitle="2016 Postseason", y="wOBA", x="Player Age")

