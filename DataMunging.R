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
library(survival)

# import datasets
options(stringsAsFactors = FALSE) # don't coerce strings to factors
lcdata <- read.csv(file="StudentCompiled.csv", header=T)
irdata <- read.csv(file="data_0902_1508.csv", header=T)
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
                                           "COURSE_NUMBER",'COURSE_SEC_IDENTIFIER', "LAST_NAME", 
                                           "FIRST_NAME"), all.y=T, sort=F)

# clean out environment
remove(irdata)
remove(dataData)
remove(lcdata)
remove(sidata)

names(mergedData)

# drop unwanted variables
names(mergedData)[names(mergedData) == 'income_househould_median'] = 'income_household_median'
names(mergedData)[names(mergedData) == 'Total'] = 'TOTAL'

mergedData$ACTIVITY =  0
mergedData$ACTIVITY[mergedData$ACTIVITY_NCAA_IND == 1 | mergedData$ACTIVITY_GREEK_IND == 1 | mergedData$ACTIVITY_ACADM_IND == 1] = 1
mergedData$IPEDS_RACE_CODE[mergedData$IPEDS_RACE_CODE %in% c('I','M','A','P','U','T','U','Z','N')] = 'OTHER'
# create proportions for population variables

select =  c(
    'pop_over25','pop_over16','pop','landArea',
    'GIVE_HRS',
    'income_household_median',
    'home_medianValue','pop_black',
    'ACTIVITY','SUMMER_CNT',
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
    'IPEDS_RACE_CODE',
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
    'pop_armedForces',
    'pop_over25_HSGrad',
    'AGE',
    'MAJOR_CHANGES',
    'TERM_ORD',
    'INST_CUM_HRS_EARNED',
    'pop',
    'COURSE_SEC_IDENTIFIER'
)

mergedData <- mergedData[,names(mergedData) %in% select]

# drop PHYS2211 and GEOL1121 from term 201208
mergedData <- mergedData[(mergedData$TERM_CODE==201208 & 
                              mergedData$COURSE_ACRONYM=='PHYS' & 
                              mergedData$COURSE_NUMBER=='2211')==FALSE,]
mergedData <- mergedData[(mergedData$TERM_CODE==201208 & 
                              mergedData$COURSE_ACRONYM=='GEOL' & 
                              mergedData$COURSE_NUMBER=='1121')==FALSE,]

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
mergedData$IPEDS_RACE_CODE <- factor(mergedData$IPEDS_RACE_CODE, ordered=FALSE)
mergedData$IPEDS_RACE_CODE = relevel(mergedData$IPEDS_RACE_CODE,ref='W')
mergedData$DEPENDENCY_CODE <- factor(mergedData$DEPENDENCY_CODE, ordered=TRUE)
mergedData$MAJOR_IND <- factor(mergedData$MAJOR_IND, ordered=FALSE)
mergedData$MAJOR_IND <- relevel(mergedData$MAJOR_IND, ref="11")

# change NA's in SI_LEADER to "NONE"
mergedData$SI_LEADER <- factor(mergedData$SI_LEADER, ordered=FALSE)

# treat nas as zeros
mergedData$TOTAL[is.na(mergedData$TOTAL)] <- 0
mergedData$MEAN[is.na(mergedData$MEAN)] <- 0
mergedData$MEDIAN[is.na(mergedData$MEDIAN)] <- 0
mergedData$MAJOR_IND[is.na(mergedData$MAJOR_IND)] <- 11


workData <- mergedData

# create ordinal variables
mergedData$FATHER_HIGHEST_GRADE_CODE <- as.numeric(factor(mergedData$FATHER_HIGHEST_GRADE_CODE, 
                                                          levels=c(NA,1,2,3,4), 
                                                          ordered=TRUE))
mergedData$MOTHER_HIGHEST_GRADE_CODE <- as.numeric(factor(mergedData$MOTHER_HIGHEST_GRADE_CODE, 
                                                          levels=c(NA,1,2,3,4), 
                                                          ordered=TRUE))
mergedData$INST_COURSE_GRADE <- as.numeric(factor(mergedData$INST_COURSE_GRADE, 
                                                  levels=c('F','D','C','B','A'), 
                                                  ordered=TRUE))
# create time series variable using TERM_ORD
mergedData$TERM_ORD <- factor(mergedData$TERM_ORD, ordered=T)
mergedData$TERM_ORD <- ts(mergedData$TERM_ORD)

ids = unique(mergedData$STU_INST_UID)
terms = unique(mergedData$TERM_CODE)

for(id in ids)
{
    terms = sort(unique(mergedData$TERM_CODE[mergedData$STU_INST_UID == id]))
    if(length(terms) > 1)
    {
        for(i in 2:length(terms))
        {
            if(nrow( mergedData[mergedData$TERM_CODE == terms[i] & mergedData$STU_INST_UID == id,]) > 0 & 
               nrow( mergedData[mergedData$TERM_CODE == terms[i-1] & mergedData$STU_INST_UID == id,]) > 0
            ) {
                mergedData$INST_CUM_GPA[mergedData$TERM_CODE == terms[i] & mergedData$STU_INST_UID == id] = 
                    unique(mergedData$INST_CUM_GPA[mergedData$TERM_CODE == terms[i-1] & mergedData$STU_INST_UID == id])
                
                mergedData$INST_CUM_HRS_ATTEMPTED[mergedData$TERM_CODE == terms[i] & mergedData$STU_INST_UID == id] = 
                    unique(mergedData$INST_CUM_HRS_ATTEMPTED[mergedData$TERM_CODE == terms[i-1] & mergedData$STU_INST_UID == id])
                
                mergedData$INST_CUM_HRS_EARNED[mergedData$TERM_CODE == terms[i] & mergedData$STU_INST_UID == id] = 
                    unique(mergedData$INST_CUM_HRS_EARNED[mergedData$TERM_CODE == terms[i-1] & mergedData$STU_INST_UID == id])
            }
        }
    }
}


mergedData <- mergedData[!is.na(mergedData$SI_LEADER),]



# create treatment variable

mergedData$treatment = 0
mergedData$treatment[mergedData$TOTAL > 0] = 1

# create class average variable
# create unique ids for sections/SI Leaders
mergedData$class_uniq_id = NA
for(i in 1:nrow(mergedData))
    mergedData[i,'class_uniq_id'] = paste(mergedData[i,'COURSE_SEC_IDENTIFIER'],
                                          mergedData[i,'SI_LEADER'])
# get unique ids from dataset
classes_uniq_ids <- unique(mergedData$class_uniq_id)

# shift course grade to 4-point scale
mergedData$INST_COURSE_GRADE <- mergedData$INST_COURSE_GRADE - 1

# create vector of class averages
mergedData$class_average = NA
for(class_uniq_id in classes_uniq_ids)
    mergedData$class_average[mergedData$class_uniq_id == class_uniq_id] = 
    mean(mergedData$INST_COURSE_GRADE[mergedData$class_uniq_id == class_uniq_id])

# convert factors to numeric
mergedData$BRIDGE_IND <- as.numeric(mergedData$BRIDGE_IND)
mergedData$GENDER_CODE <- as.numeric(mergedData$GENDER_CODE)
mergedData$IPEDS_RACE_CODE <- as.character(mergedData$IPEDS_RACE_CODE)
mergedData$DEPENDENCY_CODE <- as.numeric(mergedData$DEPENDENCY_CODE)

mergedData$income_household_median = as.numeric(mergedData$income_household_median)
mergedData$home_medianValue = as.numeric(mergedData$home_medianValue)

# run Amelia
imputedData <- amelia(
    x=mergedData,
    m=1,
    logs=c(
        'STUDENT_OR_PARENT_AGI',
        'income_household_median',
        'home_medianValue','pop','landArea','pop_over25','pop_over16'
    ),
    sqrts=c(
        'INST_CUM_HRS_ATTEMPTED',
        'INST_CUM_HRS_EARNED'
    ),
    nom=c(
        'BRIDGE_IND',
        'GENDER_CODE',
        'NR_GRANT',
        'NN_GRANT',
        'IPEDS_RACE_CODE',
        'DEPENDENCY_CODE',
        'ACTIVITY'
    ),
    ords=c(
        'FATHER_HIGHEST_GRADE_CODE',
        'MOTHER_HIGHEST_GRADE_CODE',
        'INST_COURSE_GRADE'
    ),
    idvars=c(
        'MAJOR_DESC',
        'TERM_CODE',
        'TERM_ORD',
        'COURSE_ACRONYM',
        'COURSE_NUMBER',
        'COURSE_SEC_IDENTIFIER',
        'treatment',
        'class_uniq_id',
        'STU_INST_UID',
        'SI_LEADER',
        'MAJOR_IND'
    )
)

#get first imputation
mergedData <- imputedData$imputations[[1]]


#some data adjustments
mergedData$COURSE_ACRONYM = as.character(mergedData$COURSE_ACRONYM)
mergedData$IPEDS_RACE_CODE <- factor(mergedData$IPEDS_RACE_CODE, ordered=FALSE)
mergedData$IPEDS_RACE_CODE = relevel(mergedData$IPEDS_RACE_CODE,ref='W')
mergedData$MAJOR_IND <- factor(mergedData$MAJOR_IND, ordered=FALSE)
mergedData$MAJOR_IND <- relevel(mergedData$MAJOR_IND, ref="11")

# create proportions for population variables
mergedData$pop_over25_HSGrad <- mergedData$pop_over25_HSGrad/mergedData$pop_over25
mergedData$pop_over25_bachelors <- mergedData$pop_over25_bachelors/mergedData$pop_over25
mergedData$pop_armedForces <- mergedData$pop_armedForces/mergedData$pop_over16
mergedData$pop_black <- mergedData$pop_black/ (mergedData$pop)
mergedData$pop <- mergedData$pop/mergedData$landArea*2.59e+6


# create variable testing sameness of major and course department
mergedData$MAJOR_COURSE_SAME = 0
mergedData$MAJOR_COURSE_SAME[grepl('computer science',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'CSCI'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('mathematics',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'MATH'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('biology',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'BIOL'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('chemistry',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'CHEM'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('economics',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'ECON'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('environmental',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'ENSC'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('geography',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'GEOG'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('physics',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'PHYS'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('french',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'FREN'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('spanish',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'SPAN'] = 1
mergedData$MAJOR_COURSE_SAME[grepl('psychology',mergedData$MAJOR_DESC,ignore.case=TRUE) & mergedData$COURSE_ACRONYM == 'PSYC'] = 1


# MatchIt

matchedFull <- matchit(
    formula = treatment ~ 
        INST_TERM_HRS_ATTEMPTED
    + INST_CUM_GPA
    + class_average
    #+ INST_CUM_HRS_ATTEMPTED
    + HS_GPA
    + I(scale(SAT_CRITICAL_READING + SAT_MATH)) 
    #+ BRIDGE_IND 
    + GENDER_CODE
    + IPEDS_RACE_CODE  
    + NN_GRANT
    + NR_GRANT
    #+ STUDENT_OR_PARENT_AGI  
    #+ MOTHER_HIGHEST_GRADE_CODE 
    #+ FATHER_HIGHEST_GRADE_CODE 
    #+ pop_over25_bachelors   
    #+ income_household_median
    + I(scale(home_medianValue))
    + SUMMER_CNT
    #+ MAJOR_COURSE_SAME
    #+ ACTIVITY
    #+ pop_armedForces  
    #+ pop_over25_HSGrad
    + AGE  
    #+ MAJOR_CHANGES 
    + pop  
    + class_average
    ,
    distance="logit",
    data=mergedData[,names(mergedData) %in% c('treatment','STU_INST_UID', 'INST_TERM_HRS_ATTEMPTED','GENDER_CODE','IPEDS_RACE_CODE','INST_CUM_GPA',
                                              'NN_GRANT','NR_GRANT','home_medianValue','SUMMER_CNT','AGE','pop','class_average',
                                              'MAJOR_IND','HS_GPA','SAT_CRITICAL_READING','SAT_MATH','INST_COURSE_GRADE',
                                              'ACTIVITY','pop_armedForces','pop_over25_HSGrad','MAJOR_CHANGES','pop_over25_bachelors',
                                              'BRIDGE_IND','STUDENT_OR_PARENT_AGI','MOTHER_HIGHEST_GRADE_CODE',
                                              'FATHER_HIGHEST_GRADE_CODE','INST_CUM_HOURS_ATTEMPTED','TOTAL','MEAN','MEDIAN',
                                              'MAJOR_COURSE_SAME','income_household_median','INST_CUM_HRS_ATTEMPTED')],
    method='nearest',reestimate = TRUE
)

mergedData <- match.data(matchedFull)

PSMData <- mergedData

backupData <- mergedData

PSM_ids = unique(PSMData$STU_INST_UID)
  
  survivalData = workData[workData$STU_INST_UID %in% PSM_ids,]
# create ordinal variables
survivalData$STUDENT_LEVEL_NBR <- as.numeric(factor(survivalData$STUDENT_LEVEL_NBR, 
                                                  levels=c(10,20,30,40), 
                                                  ordered=TRUE))
survivalData$FATHER_HIGHEST_GRADE_CODE <- as.numeric(factor(survivalData$FATHER_HIGHEST_GRADE_CODE, 
                                                          levels=c(NA,1,2,3,4), 
                                                          ordered=TRUE))
survivalData$MOTHER_HIGHEST_GRADE_CODE <- as.numeric(factor(survivalData$MOTHER_HIGHEST_GRADE_CODE, 
                                                          levels=c(NA,1,2,3,4), 
                                                          ordered=TRUE))
survivalData$INST_COURSE_GRADE <- as.numeric(factor(survivalData$INST_COURSE_GRADE, 
                                                  levels=c('F','D','C','B','A'), 
                                                  ordered=TRUE))

survivalData$pop_over25_HSGrad <- survivalData$pop_over25_HSGrad/survivalData$pop_over25
survivalData$pop_over25_bachelors <- survivalData$pop_over25_bachelors/survivalData$pop_over25
survivalData$pop_armedForces <- survivalData$pop_armedForces/survivalData$pop_over16
survivalData$pop_density <- survivalData$pop/survivalData$landArea


survivalData$BIOL_VISITS = 0
survivalData$CHEM_VISITS = 0
survivalData$MATH_VISITS = 0
survivalData$CSCI_VISITS = 0
survivalData$ASTR_VISITS = 0
survivalData$ECON_VISITS = 0
survivalData$ENSC_GEOL_VISITS = 0
survivalData$GC1Y_VISITS = 0
survivalData$KINS_VISITS = 0
survivalData$PHYS_VISITS = 0
survivalData$SPAN_FREN_VISITS = 0
survivalData$PSYC_VISITS = 0
survivalData$GEOG_VISITS = 0



survivalData$INST_COURSE_GRADE <- survivalData$INST_COURSE_GRADE - 1

survivalData$Quality_Points = survivalData$COURSE_ATTEMPTED_HRS*survivalData$INST_COURSE_GRADE

for(i in 1:nrow(survivalData))
  survivalData[i, 'uniq_stu_id'] = paste(survivalData[i, 'STU_INST_UID'], survivalData[i,'TERM_CODE'])
remove(i)
uniq_stu_ids <- unique(survivalData$uniq_stu_id)
survivalData$TOTAL_QUALITY_POINTS = 0
for(uniq_stu_id in uniq_stu_ids)
{
  survivalData$TOTAL_QUALITY_POINTS[survivalData$uniq_stu_id == uniq_stu_id] = sum(survivalData$Quality_Points
                                                                               [survivalData$uniq_stu_id == uniq_stu_id])
  
}
for(uniq_stu_id in uniq_stu_ids)
{
  survivalData$TOTAL_CRSE_ATMP_HRS[survivalData$uniq_stu_id == uniq_stu_id] = sum(survivalData$COURSE_ATTEMPTED_HRS[survivalData$uniq_stu_id == uniq_stu_id])
  }

survivalData$ANY_SI = 0 
survivalData$NUM_OF_SI_LEADERS = 0


for(i in 1:length(survivalData$SI))
{
  if(survivalData$SI_LEADER[i] != "NONE")
    survivalData$ANY_SI[i] = 1
}



for(uniq_stu_id in uniq_stu_ids)
{
  survivalData$NUM_OF_SI_LEADERS[survivalData$uniq_stu_id == uniq_stu_id] = sum(survivalData$ANY_SI[survivalData$uniq_stu_id == uniq_stu_id])
}

for(uniq_stu_id in uniq_stu_ids)
{
  survivalData$TOTAL_VISITS[survivalData$uniq_stu_id == uniq_stu_id] = sum(survivalData$TOTAL[survivalData$uniq_stu_id == uniq_stu_id])
}



survivalData$TERM_GPA = 0
  
survivalData$TERM_GPA = survivalData$TOTAL_QUALITY_POINTS / survivalData$TOTAL_CRSE_ATMP_HRS

pracData = survivalData
pracData$COURSE_ACRONYM = as.character(pracData$COURSE_ACRONYM)
for(i in 1:length(pracData$COURSE_ACRONYM))
{
  if(is.na(pracData$COURSE_ACRONYM[i]))
    pracData$COURSE_ACRONYM[i]="NONE"
}

pracData <- pracData[which(pracData$COURSE_ACRONYM!="NONE"),]
pracData$COURSE_ACRONYM[pracData$COURSE_ACRONYM == 'SPAN'] = 'SPAN_FREN'
pracData$COURSE_ACRONYM[pracData$COURSE_ACRONYM == 'FREN'] = 'SPAN_FREN'
pracData$COURSE_ACRONYM[pracData$COURSE_ACRONYM == 'CSCI GC1Y'] = 'CSCI'
pracData$COURSE_ACRONYM[pracData$COURSE_ACRONYM == 'ENSC'] = 'ENSC_GEOL'
pracData$COURSE_ACRONYM[pracData$COURSE_ACRONYM == 'GEOL'] = 'ENSC_GEOL'
pracData$COURSE_ACRONYM <- factor(pracData$COURSE_ACRONYM, ordered=FALSE)
table(pracData$COURSE_ACRONYM,useNA = 'ifany')
pracData$TERM_CODE = as.numeric(pracData$TERM_CODE)
pracData = pracData[order(pracData$TERM_ORD),]

treated_ids = unique(pracData$STU_INST_UID[pracData$SI_LEADER != 'NONE'])
pracData = pracData[pracData$STU_INST_UID %in% treated_ids,]

table(pracData$INST_CUM_HRS_ATTEMPTED, useNA = 'ifany')
table(pracData$INST_CUM_GPA, useNA = 'ifany')
table(pracData$MAJOR_CHANGES, useNA = 'ifany') 
table(pracData$INST_CUM_HRS_EARNED, useNA = 'ifany')



noFresData = pracData[which(pracData$INST_CUM_HRS_EARNED > 29),]

noFresData$TOTAL_SIs = 0
ids = unique(noFresData$STU_INST_UID)
terms = unique(noFresData$TERM_CODE)

for(id in ids)
{
  for(term in terms)
  {
    noFresData$BIOL_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "BIOL"])
    noFresData$CHEM_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "CHEM"])
    noFresData$MATH_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "MATH"])
    noFresData$CSCI_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "CSCI"])
    noFresData$ASTR_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "ASTR"])
    noFresData$ECON_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "ECON"])
    noFresData$ENSC_GEOL_VISITS[noFresData$STU_INST_UID == id 
                             & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                                    & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "ENSC_GEOL"])
    noFresData$GC1Y_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "GC1Y"])
    noFresData$KINS_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "KINS"])
    noFresData$PHYS_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "PHYS"])
    noFresData$PSYC_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "PSYC"])
    noFresData$GEOG_VISITS[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                               & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "GEOG"])
    noFresData$SPAN_FREN_VISITS[noFresData$STU_INST_UID == id 
                             & noFresData$TERM_CODE == term] = sum(noFresData$TOTAL[noFresData$STU_INST_UID == id 
                                                                                    & noFresData$TERM_CODE <= term & noFresData$COURSE_ACRONYM == "SPAN_FREN"])
    noFresData$TOTAL_SIs[noFresData$STU_INST_UID == id 
                        & noFresData$TERM_CODE == term] = sum(noFresData$ANY_SI[noFresData$STU_INST_UID == id 
                                                                                    & noFresData$TERM_CODE <= term])
    
  }
  
}

stu_ids = unique(noFresData$STU_INST_UID)
noFresData$survTime = noFresData$TERM_ORD
noFresData$T1 = 0

for (id in stu_ids) 
{
  noFresData$survTime[noFresData$STU_INST_UID == id] = noFresData$survTime[noFresData$STU_INST_UID == id] - min(noFresData$survTime[noFresData$STU_INST_UID == id])
}

noFresData$COURSE_ACRONYM <- NULL
noFresData$COURSE_NUMBER <- NULL
noFresData$COURSE_ATTEMPTED_HRS <- NULL
noFresData$INST_COURSE_GRADE <- NULL
noFresData$TOTAL <- NULL
noFresData$MEDIAN <- NULL
noFresData$MEAN <- NULL
noFresData$SI <- NULL
noFresData$uniq_stu_id <- NULL
noFresData$COURSE_SEC_IDENTIFIER <- NULL
noFresData$Quality_Points <- NULL
noFresData$SI_LEADER <- NULL
noFresData$ANY_SI <- NULL
noFresData <- noFresData[!duplicated(noFresData),]

ids = unique(noFresData$STU_INST_UID)
terms = unique(noFresData$TERM_CODE)

noFresData$INST_CUM_HRS_ATTEMPTED_LAGGED = 0
noFresData$INST_CUM_HRS_EARNED_LAGGED = 0
noFresData$INST_CUM_GPA_LAGGED = 0
noFresData$TERM_GPA_LAGGED = 0


for(id in ids)
{
  terms = sort(unique(noFresData$TERM_CODE[noFresData$STU_INST_UID == id]))
  if(length(terms) > 1)
  {
    for(i in 2:length(terms))
    {
      noFresData$INST_CUM_HRS_ATTEMPTED_LAGGED[noFresData$TERM_CODE == terms[i] & noFresData$STU_INST_UID == id] = noFresData$INST_CUM_HRS_ATTEMPTED[noFresData$TERM_CODE == terms[i-1] & noFresData$STU_INST_UID == id]
    }
  }
  if(length(terms) > 1)
  {
    for(i in 2:length(terms))
    {
      noFresData$INST_CUM_HRS_EARNED_LAGGED[noFresData$TERM_CODE == terms[i] & noFresData$STU_INST_UID == id] = noFresData$INST_CUM_HRS_EARNED[noFresData$TERM_CODE == terms[i-1] & noFresData$STU_INST_UID == id]
    }
  }
  if(length(terms) > 1)
  {
    for(i in 2:length(terms))
    {
      noFresData$INST_CUM_GPA_LAGGED[noFresData$TERM_CODE == terms[i] & noFresData$STU_INST_UID == id] = noFresData$INST_CUM_GPA[noFresData$TERM_CODE == terms[i-1] & noFresData$STU_INST_UID == id]
    }
  }
  if(length(terms) > 1)
  {
    for(i in 2:length(terms))
    {
      noFresData$TERM_GPA_LAGGED[noFresData$TERM_CODE == terms[i] & noFresData$STU_INST_UID == id] = noFresData$TERM_GPA[noFresData$TERM_CODE == terms[i-1] & noFresData$STU_INST_UID == id]
    }
  }
  
}


thisData = noFresData

stem_majors = c(
  "Environmental Science"  ,        "Computer Science"      ,         "Research and Experimental Psyc"
  ,"Physics, General" ,  "Biology, General" , "Psychology, General" ,   "Chemistry, General",  "Mathematics"     
  ,"Web/Multimedia Mgmt & Webmstr" ,"Computer/Information Sci, Gen" 
)

stem_ids = unique(thisData$STU_INST_UID[thisData$MAJOR_DESC %in% stem_majors])
ids = unique(thisData$STU_INST_UID)

for(id in ids) 
{
  terms = sort(unique(thisData$TERM_CODE[thisData$STU_INST_UID == id]))

  
  if (length(terms) == 1) 
    {  
    thisData$dropped[thisData$TERM_CODE == terms & thisData$STU_INST_UID == id] = 0
    next
    }
  decided = 0
  for(i in 2:length(terms)) 
  {
    last_major =  unique(thisData$MAJOR_DESC[thisData$TERM_CODE == terms[i-1] & thisData$STU_INST_UID == id])
    cur_major =  unique(thisData$MAJOR_DESC[thisData$TERM_CODE == terms[i] & thisData$STU_INST_UID == id])
    if (decided == 0 & last_major %in% stem_majors) 
    {
      decided = 1
    }
    else if (decided == 1 & last_major != cur_major & last_major %in% stem_majors ) 
    {
      thisData$dropped[thisData$TERM_CODE == terms[i] & thisData$STU_INST_UID == id] = 1
      thisData = thisData[(thisData$TERM_CODE > terms[i] & thisData$STU_INST_UID == id)== FALSE, ]
      break
    }
  }
  if (decided == 0) 
  {
    thisData = thisData[thisData$STU_INST_UID != id,]
  }
  
}

abc_ids = unique(stemData$STU_INST_UID)

stemData <- thisData[which(thisData$INST_CUM_HRS_ATTEMPTED_LAGGED != 0),]

for (id in abc_ids) 
{
  stemData$T1[stemData$STU_INST_UID == id] = stemData$TERM_ORD[stemData$STU_INST_UID == id] - min(stemData$TERM_ORD[noFresData$STU_INST_UID == id])
}

stemData$CHANGED_MAJOR = 1

for(id in ids)
{
 sorted = sort(unique(stemData$TERM_ORD[stemData$STU_INST_UID == id], decreasing = TRUE))
 max = sorted[1]
 almost_max = sorted[2]
 lastMajor = stemData$MAJOR_DESC[stemData$STU_INST_UID == id & stemData$TERM_ORD == max]
 almostLM = stemData$MAJOR_DESC[stemData$STU_INST_UID == id & stemData$TERM_ORD == almost_max]
 if ((grepl(almostLM, lastMajor)) | (grepl(almostLM, 'Psychology, General') & grepl(lastMajor, 'Research and Experimental Psyc')) == TRUE)
 {
  stemData$CHANGED_MAJOR[stemData$STU_INST_UID == id & stemData$TERM_ORD == max] = 0
 }
 else
 {
 stemData$CHANGED_MAJOR[stemData$STU_INST_UID == id & stemData$TERM_ORD == max] = 1
 }

}


stemData$dropped <- NULL
 stemData$INST_CUM_HRS_EARNED <- NULL
stemData$INST_CUM_GPA <- NULL
 stemData$INST_CUM_HRS_ATTEMPTED <- NULL
 stemData$BRIDGE_IND <- NULL

imputedData <- amelia(
    x=mergedData,
    m=1,
    logs=c(
        'STUDENT_OR_PARENT_AGI',
        'income_household_median',
        'home_medianValue','pop','landArea','pop_over25','pop_over16'
    ),
    sqrts=c(
        'INST_CUM_HRS_ATTEMPTED',
        'INST_CUM_HRS_EARNED'
    ),
    nom=c(
        'BRIDGE_IND',
        'GENDER_CODE',
        'NR_GRANT',
        'NN_GRANT',
        'IPEDS_RACE_CODE',
        'DEPENDENCY_CODE',
        'ACTIVITY'
    ),
    ords=c(
        'FATHER_HIGHEST_GRADE_CODE',
        'MOTHER_HIGHEST_GRADE_CODE',
        'INST_COURSE_GRADE'
    ),
    idvars=c(
        'MAJOR_DESC',
        'TERM_CODE',
        'TERM_ORD',
        'COURSE_ACRONYM',
        'COURSE_NUMBER',
        'COURSE_SEC_IDENTIFIER',
        'STU_INST_UID',
       
    )
)

imputedData <- amelia(
              x=stemData,
              m=1,
              logs=c(
                          'STUDENT_OR_PARENT_AGI',
        'income_household_median',
        'home_medianValue','pop','landArea','pop_over25','pop_over16'
                      ),
        sqrts=c(
                          'INST_CUM_HRS_ATTEMPTED_LAGGED',
                          'INST_CUM_HRS_EARNED_LAGGED'
                      ),
              noms=c(
                          'GENDER_CODE',
                          'IPEDS_RACE_CODE',
                          'DEPENDENCY_CODE'
                      ),
          ords=c(
                          'STUDENT_LEVEL_NBR',
                          'FATHER_HIGHEST_GRADE_CODE',
                          'MOTHER_HIGHEST_GRADE_CODE'
                          
                              ),
              idvars=c(
                          'TERM_CODE',
                          'TERM_ORD',
                          'MAJOR_DESC',
                          'MAJOR_IND',
                      'STU_INST_UID'
                      )
          )
          
           stemData <- imputedData$imputations[[1]]
  remove(imputedData)
  
  PSM_ids = unique(PSMData$STU_INST_UID)
  
  workData = stemData[stemData$STU_INST_UID %in% PSM_ids,]
  
  
  
#attach(stemData)
#my.surv <- Surv(stemData$survTime, stemData$MAJOR_CHANGES)
#coxph.fit <- coxph(my.surv ~ , method = "breslow", data=stemData)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
