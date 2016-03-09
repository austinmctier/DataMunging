setwd("~/R")

# load packages
library(Amelia)
library(pROC)
library(splines)
library(MatchIt)
library(arm)
library(ordinal)
library(car)
library(stats)
library(dplyr)

# import datasets
options(stringsAsFactors = FALSE) # don't coerce strings to factors
lcdata <- read.csv(file="StudentData.csv", header=T)
irdata <- read.csv(file="data_0708_1408.csv", header=T)
sidata <- read.csv(file="SiData.csv", header=T)

# drop transfer students
ftf_ids = unique(irdata$STU_INST_UID[irdata$FTF_IPEDS_IND=='Y'])
irdata <- irdata[irdata$STU_INST_UID %in% ftf_ids,]

# remove duplicate rows (if any)
irdata <- irdata[!duplicated(irdata),]

# coerce names to lowercase for consistency
irdata$FIRST_NAME <- tolower(irdata$FIRST_NAME)
irdata$LAST_NAME <- tolower(irdata$LAST_NAME)
lcdata$FIRST_NAME <- tolower(lcdata$FIRST_NAME)
lcdata$LAST_NAME <- tolower(lcdata$LAST_NAME)

# merge sets (left join)
dataData <- merge(sidata, lcdata, by=c("TERM_CODE","COURSE_ACRONYM", 
                                       "COURSE_NUMBER", "COURSE_SEC_IDENTIFIER", 
                                       "SI_LEADER"), all.y=T, sort=F)

mergedData <- merge(dataData, irdata, by=c("TERM_CODE","COURSE_ACRONYM", 
                                           "COURSE_NUMBER", "LAST_NAME", 
                                           "FIRST_NAME"), all.y=T, sort=F)

# clean out environment
remove(irdata)
remove(dataData)
remove(lcdata)
remove(sidata)

names(mergedData)

# drop unwanted variables
mergedData <- mergedData[,c(
  'TERM_CODE',
  'STU_INST_UID',
  'COURSE_ACRONYM',
  'COURSE_NUMBER',
  'STUDENT_LEVEL_NBR',
  'INST_TERM_HRS_ATTEMPTED',
  'INST_CUM_HRS_ATTEMPTED',
  'INST_CUM_GPA',
  'HS_GPA',
  'SAT_CRITICAL_READING',
  'SAT_MATH',
  'ACT_COMPOSITE',
  'BRIDGE_IND',
  'GENDER_CODE',
  'WHITE_IND',
  'NN_GRANT',
  'NR_GRANT',
  'LOAN',
  'MAJOR_DESC',
  'DEPENDENCY_CODE',
  'STUDENT_OR_PARENT_AGI',
  'FATHER_HIGHEST_GRADE_CODE',
  'MOTHER_HIGHEST_GRADE_CODE',
  'COURSE_ATTEMPTED_HRS',
  'INST_COURSE_GRADE',
  'SI_LEADER',
  'TOTAL',
  'MEDIAN',
  'MEAN',
  'MAJOR_IND',
  'pop_over25_bachelors',
  'pop_over25',
  'pop_over16',
  'pop_armedForces',
  'pop_over25_HSGrad',
  'AGE',
  'MAJOR_CHANGES',
  'landArea',
  'TERM_ORD',
  'INST_CUM_HRS_EARNED',
  'pop',
  'COURSE_SEC_IDENTIFIER.x'
)]

mergedData$COURSE_SEC_IDENTIFIER <- mergedData$COURSE_SEC_IDENTIFIER.x
mergedData$COURSE_SEC_IDENTIFIER.x <- NULL

# drop PHYS2211 and GEOL1121 from term 201208
mergedData <- mergedData[which(mergedData$TERM_CODE!=201208 & 
                                 mergedData$COURSE_ACRONYM!='PHYS' & 
                                 mergedData$COURSE_NUMBER!='2211'),]
mergedData <- mergedData[which(mergedData$TERM_CODE!=201208 & 
                                 mergedData$COURSE_ACRONYM!='GEOL' & 
                                 mergedData$COURSE_NUMBER!='1121'),]

# drop grades other than A, B, C, D, F
mergedData <- mergedData[which(mergedData$INST_COURSE_GRADE == 'A' | 
                                 mergedData$INST_COURSE_GRADE == 'B' | 
                                 mergedData$INST_COURSE_GRADE == 'C' | 
                                 mergedData$INST_COURSE_GRADE == 'D' | 
                                 mergedData$INST_COURSE_GRADE == 'F'),]

# create categorical variables
mergedData$COURSE_ACRONYM <- factor(mergedData$COURSE_ACRONYM, ordered=FALSE)
mergedData$COURSE_NUMBER <- factor(mergedData$COURSE_NUMBER, ordered=FALSE)
mergedData$BRIDGE_IND <- factor(mergedData$BRIDGE_IND, ordered=TRUE)
mergedData$GENDER_CODE <- factor(mergedData$GENDER_CODE, ordered=TRUE)
mergedData$WHITE_IND <- factor(mergedData$WHITE_IND, ordered=TRUE)
mergedData$DEPENDENCY_CODE <- factor(mergedData$DEPENDENCY_CODE, ordered=TRUE)
mergedData$MAJOR_DESC <- factor(mergedData$MAJOR_DESC, ordered=FALSE)
mergedData$MAJOR_IND <- factor(mergedData$MAJOR_IND, ordered=FALSE)
mergedData$MAJOR_IND <- relevel(mergedData$MAJOR_IND, ref="14")
for(i in 1:length(mergedData$SI_LEADER))
{
  if(is.na(mergedData$SI_LEADER[i]))
    mergedData$SI_LEADER[i]="NONE"
}
mergedData$SI_LEADER <- factor(mergedData$SI_LEADER, ordered=FALSE)
mergedData$SI_LEADER <- relevel(mergedData$SI_LEADER, ref="NONE")

# treat nas as zeros
mergedData$TOTAL[is.na(mergedData$TOTAL)] <- 0
mergedData$MEAN[is.na(mergedData$MEAN)] <- 0
mergedData$MEDIAN[is.na(mergedData$MEDIAN)] <- 0

# create ordinal variables
mergedData$STUDENT_LEVEL_NBR <- as.numeric(factor(mergedData$STUDENT_LEVEL_NBR, 
                                                  +                                                   levels=c(10,20,30,40), 
                                                  +                                                   ordered=TRUE))
mergedData$FATHER_HIGHEST_GRADE_CODE <- as.numeric(factor(mergedData$FATHER_HIGHEST_GRADE_CODE, 
                                                          +                                                           levels=c(NA,1,2,3,4), 
                                                          +                                                           ordered=TRUE))
mergedData$MOTHER_HIGHEST_GRADE_CODE <- as.numeric(factor(mergedData$MOTHER_HIGHEST_GRADE_CODE, 
                                                          +                                                           levels=c(NA,1,2,3,4), 
                                                          +                                                           ordered=TRUE))
mergedData$INST_COURSE_GRADE <- as.numeric(factor(mergedData$INST_COURSE_GRADE, 
                                                  +                                                   levels=c('F','D','C','B','A'), 
                                                  +                                                   ordered=TRUE))
imputedData <- amelia(
  x=mergedData,
  m=1,
  logs=c(
    'STUDENT_OR_PARENT_AGI',
    'TOTAL',
    'MEDIAN',
    'MEAN'
  ),
  sqrts=c(
    'INST_CUM_HRS_ATTEMPTED',
    'INST_CUM_HRS_EARNED'
  ),
  noms=c(
    'BRIDGE_IND',
    'GENDER_CODE',
    'WHITE_IND',
    'MAJOR_IND',
    'DEPENDENCY_CODE'
  ),
  ords=c(
    'STUDENT_LEVEL_NBR',
    'FATHER_HIGHEST_GRADE_CODE',
    'MOTHER_HIGHEST_GRADE_CODE',
    'INST_COURSE_GRADE'
  ),
  idvars=c(
    'TERM_CODE',
    'TERM_ORD',
    'COURSE_ACRONYM',
    'COURSE_NUMBER',
    'SI_LEADER',
    'MAJOR_DESC',
    'COURSE_SEC_IDENTIFIER',
    'STU_INST_UID'
  )
)
# get first imputation
mergedData <- imputedData$imputations[[1]]
remove(imputedData)
mergedData$TERM_ORD <- factor(mergedData$TERM_ORD, ordered=T)
mergedData$TERM_ORD <- ts(mergedData$TERM_ORD)

# lag term 
mergedData$INST_CUM_HRS_ATTEMPTED <- lag(mergedData$INST_CUM_HRS_ATTEMPTED, k=-1)
mergedData$INST_CUM_GPA <- lag(mergedData$INST_CUM_GPA, k=-1)
mergedData$INST_CUM_HRS_EARNED <- lag(mergedData$INST_CUM_HRS_EARNED, k=-1)

# create proportions for population variables
mergedData$pop_over25_HSGrad <- mergedData$pop_over25_HSGrad/mergedData$pop_over25
mergedData$pop_over25_bachelors <- mergedData$pop_over25_bachelors/mergedData$pop_over25
mergedData$pop_armedForces <- mergedData$pop_armedForces/mergedData$pop_over16
mergedData$pop <- mergedData$pop/mergedData$landArea
mergedData$MAJOR_CHANGES <- lag(mergedData$MAJOR_CHANGES, k=-1)


#mergedData$LAGGED_TERM_GPA = NA
#....

for(id in ids) {
  vars = newData[newData$STU_INST_UID == id,names(newData) %in% c("TERM_CODE", "INST_CUM_GPA", "INST_COURSE_GRADE")]
  vars = vars[order(vars$TERM_CODE),]
  vars$TERM_CODE = as.numeric(vars$TERM_CODE)
  vars$STU_INST_UID = as.numeric(vars$STU_INST_UID)
  for (i in 2:nrow(vars))
  {
    vars$COURSE_HRS[vars$TERM_CODE == vars$TERM_CODE[i-1]]
    vars$GRADE[vars$TERM_CODE == vars$TERM_CODE[i-1]]
    #mergedData$LAGGED_TERM_GPA[newData$TERM_CODE == vars$TERM_CODE[i] & newData$STUDENT_INST_ID == id] = calulatedLaggedVariable
    newData$LAGGED_VARIABLE[newData$TERM_CODE == vars$TERM_CODE[i] & newData$STUDENT_INST_ID == id]
  }
}

 mergedData$BIO_HRS = 0
 mergedData$CHEM_HRS = 0
 mergedData$MATH_HRS = 0
 mergedData$CSCI_HRS = 0
 mergedData$ASTR_HRS = 0
 mergedData$PREV_TERM_GPA = 0
 mergedData$SI_IN_CLASS = 0
 mergedData$NUM_SI_TERM = 0
 mergedData$HRS_SI_SUPPORTED_CLASS = 0
 mergedData$CUM_HRS_BY_DEPARTMENT = 0
 mergedData$INST_COURSE_GRADE <- mergedData$INST_COURSE_GRADE - 1
 #pracData$INST_COURSE_GRADE <- pracData$INST_COURSE_GRADE - 1
 pracData$Quality_Points = pracData$COURSE_ATTEMPTED_HRS*pracData$INST_COURSE_GRADE
 View(pracData)
 for(i in 1:nrow(pracData))
      pracData[i, 'uniq_stu_id'] = paste(pracData[i, 'STU_INST_UID'], pracData[i,'TERM_CODE'])
 remove(i)
 uniq_stu_ids <- unique(pracData$uniq_stu_id)
 pracData$NUM_CLASSES = 0
 pracData$TOTAL_QUALITY_POINTS = 0
 for(uniq_stu_id in uniq_stu_ids)
        {
                 pracData$TOTAL_QUALITY_POINTS[pracData$uniq_stu_id == uniq_stu_id] = sum(pracData$Quality_Points[pracData$uniq_stu_id == uniq_stu_id])
                  
                    }
 for(uniq_stu_id in uniq_stu_ids)
            {
                          pracData$TOTAL_CRSE_ATMP_HRS[pracData$uniq_stu_id == uniq_stu_id] = sum(pracData$COURSE_ATTEMPTED_HRS[pracData$uniq_stu_id == uniq_stu_id])}
 pracData$PREV_TERM_GPA = pracData$TOTAL_QUALITY_POINTS / pracData$TOTAL_CRSE_ATMP_HRS
 View(pracData)

 pracData$PREV_TERM_GPA <- lead(pracData$PREV_TERM_GPA, k= +1)
 View(mergedData)
