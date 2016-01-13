# DATA MUNGING
# R. Lawson, A. McTier
# GCSU OIR

# set working directory
setwd("~/R");

# load packages
# N/A

# import datasets
lcdata <- read.csv(file="StudentCompiled.csv", header=T)
irdata <- read.csv(file="oire_student_0915.csv", header=T)

# remove duplicate rows (if any)
dupes <- irdata[duplicated(irdata),]
irdata <- irdata[!duplicated(irdata),]

# coerce names to lowercase for consistency
irdata$FIRST_NAME <- tolower(irdata$FIRST_NAME)
irdata$LAST_NAME <- tolower(irdata$LAST_NAME)
lcdata$FIRST_NAME <- tolower(lcdata$FIRST_NAME)
lcdata$LAST_NAME <- tolower(lcdata$LAST_NAME)

# merge sets (left join)
mergedData <- merge(irdata, lcdata, by=c("TERM_CODE","COURSE_ACRONYM", 
                                         "COURSE_NUMBER", "LAST_NAME", 
                                         "FIRST_NAME"), all.x=T, sort=F)