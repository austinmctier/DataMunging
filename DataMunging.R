# DATA MUNGING
# R. Lawson, A. McTier

# set working directory
setwd("~/R");

# load packages
# N/A

# import datasets
lcdata <- read.csv(file="StudentCompiled.csv", header=T)
irdata <- read.csv(file="oire_stu_0915.csv", header=T)

# check for duplicates
dupes = irdata[duplicated(irdata),]
dupes[1,]
length(dupes)


# remove duplicate rows
  #lcdata <- unique(lcdata)
  #irdata <- unique(irdata)

# check names and dimensions
names(lcdata)
names(irdata)
dim(lcdata)
dim(irdata)

# visuals
hist(irdata$SAT_MATH)
hist(irdata$SAT_WRITING)
hist(irdata$EXPECTED_FAMILY_CONTRIBUTION)
hist(log(irdata$EXPECTED_FAMILY_CONTRIBUTION))
hist(lcdata$SUM)
hist(lcdata$SUM, col="lightblue")
hist(log(lcdata$SUM), col="lightblue")

# merge sets (left join)
mergedData <- merge(irdata, lcdata, c("TERM_CODE", "COURSE_ACRONYM", "COURSE_NUMBER", 
                                      "LAST_NAME", "FIRST_NAME"), all.x=T)

