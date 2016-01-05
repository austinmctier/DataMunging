# DATA MUNGING
# R. Lawson, A. McTier

# set working directory
setwd("~/R");

# load packages
# N/A

# import datasets
lcdata <- read.csv(file="StudentCompiled.csv", header=T);
irdata <- read.csv(file="oire_student_07to15.csv", header=T);

# check names and dimensions
names(lcdata)
names(irdata)
dim(lcdata) # 11945 22
dim(irdata) # 110915 45

# visuals
hist(irdata$SAT_MATH)
hist(irdata$SAT_WRITING)
hist(irdata$EXPECTED_FAMILY_CONTRIBUTION)
hist(log(irdata$EXPECTED_FAMILY_CONTRIBUTION))
hist(lcdata$SUM)
hist(lcdata$SUM, col="lightblue")
hist(log(lcdata$SUM), col="lightblue")

# bind columns of a discrete student
