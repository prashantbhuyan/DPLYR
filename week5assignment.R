# Prashant B. Bhuyan
# is607 Assignment 5

##### Problem 1 #####
# Question 1: 
# How likely is a Scottish person no matter which city he/she hails from to vote 
# for Cullen Skink over Partan Bree?
#
# Question 2: 
# How likely is a person from Edinburgh to vote for Cullen Skink over
# Partan Bree?  How likely is a person from Glasgow to vote for Cullen
# Skink over Partan Bree?
#
# Question 3: 
# What age group is most likely to vote for Cullen Skink over Partan Bree
# and what city are they most likely to come from?

##### Problem 2 #####
# Solution:

yes <- cbind(80100,143000,99400,150400)
no <- cbind(35900,214800,43000,207000)
df <- rbind(yes, no)
colnames(df) = c("Edinburg16-24", "EdinburgOver25", "Glasgow16-24", "GlasgowOver25")
rownames(df) = rbind("Yes", "No")
dfdataframe <- data.frame(df)

# Results: 
dfdataframe
# Edinburg16.24 EdinburgOver25 Glasgow16.24 GlasgowOver25
# Yes         80100         143000        99400        150400
# No          35900         214800        43000        207000

##### Problem 3 #####

yesdf <- dfdataframe[1,]
nodf <- dfdataframe[2,]
groupedby.yes <- gather(yesdf, Demographic, Votes)
groupedby.no <- gather(nodf, Demographic, Votes)
regrouped <- cbind(groupedby.yes, groupedby.no)
regrouped2 <- data.frame(regrouped[,1],regrouped[,2],regrouped[,4])
colnames(regrouped2) <- c("Demographic", "YesVotes", "NoVotes")
regrouped2df <- data.frame(regrouped2)

# Output 
# > regrouped2
#      Demographic YesVotes NoVotes
# 1  Edinburg16.24   80100   35900
# 2  EdinburgOver25  143000  214800
# 3  Glasgow16.24    99400   43000
# 4  GlasgowOver25   150400  207000


##### Problem 4 ##### 

# 1) Question 1 from 1st part:
# How likely is a Scottish person no matter which city he/she hails from to vote 
# for Cullen Skink over Partan Bree?
# 
# Solution: 
ans1 <- ddply(regrouped2df, .(yes,no,sum(yes)/(sum(yes)+sum(no))))
ans1df <- data.frame(ans1)
colnames(ans1df) <- c("Yes", "No", "probofyes", "demo", "yesvotes", "novotes")
probability.of.yesdf <- data.frame(ans1df)
allprob <- probability.of.yesdf$probofyes
allprobdf <- data.frame(allprob)
prob.allcitizens <- na.omit(allprobdf)

# Results
# The probability of any Scottish Citizen (assuming all citizens are either from
# Glasgow or from Edinburg) voting for Cullen Skink over Partan Bree is 
# 48.57%. 
prob.allcitizens
# Results
# allprob
# 1 0.4857231
# 
# 2) Question2 from first part:
# How likely is a person from Edinburg to vote for Cullen Skink? How 
# likely is a person from Glasgow to vote for Cullen Skink?
# 
ans2 <- ddply(regrouped2df, .(yes,no,yes/(yes+no)))
ans2df <- data.frame(ans2)
colnames(ans2df) <- c("yes", "no", "prob.by.demo", "demographic", "yesvotes", "novotes")
allvotes <- sum(ans2df$yes, ans2df$no)
# > allvotes
# [1] 973600
prob.by.city.edin <- (ans2df$yes[1]+ans2df$yes[3])/(allvotes)
# Results
# Probability that people from Edinburg prefer to vote for Cullen Skink is: 
# 22.91%
prob.by.city.edin
# [1] 0.2291495
prob.by.city.glas <- (ans2df$yes[2]+ans2df$yes[4])/(allvotes)
# Probability that people from Glasgow prefer to vote for Cullen Skink is 
# 25.65%
prob.by.city.glas
# [1] 0.2565735
#
# 3) What age group is most likely to vote for Cullen Skink and what city
# are they most likely to come from?
#
# Solution: 
prob.demo <- ans2df[which(ans2df$prob.by.demo == max(ans2df$prob.by.demo)),]
prob.demodf <- data.frame(prob.demo)
most.likelydf <- data.frame(prob.demodf$prob.by.demo, prob.demodf$demographic, prob.demodf$yesvotes, prob.demodf$novotes)
colnames(most.likelydf) <- c("ProbabilityFor", "Demographic", "YesVotes", "NoVotes")
most.likelydf
# Results:
#     ProbabilityFor  Demographic YesVotes NoVotes
# 1      0.6980337 Glasgow16.24    99400   43000
#
# Therefore, citizens ages 16-24 from Glasgow are most likely to vote for 
# Cullen Skink over Partan Bree. 





















