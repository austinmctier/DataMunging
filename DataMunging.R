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
irdata <- read.csv(file="data_0708_1505.csv", header=T)
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

mergedData$MAJOR_IND[mergedData$MAJOR_DESC == "Research and Experimental Psyc"] = 4

missing_majs = unique(mergedData$STU_INST_UID[is.na(mergedData$MAJOR_DESC) | is.na(mergedData$MAJOR_IND)])

for (id in missing_majs) {
  majs = mergedData$MAJOR_DESC[mergedData$STU_INST_UID == id & is.na(mergedData$MAJOR_DESC) == FALSE]
  majs = majs[order(mergedData$TERM_CODE[mergedData$STU_INST_UID == id & is.na(mergedData$MAJOR_DESC) == FALSE ],decreasing=TRUE)]
  
  maj_inds = mergedData$MAJOR_DESC[mergedData$STU_INST_UID == id & is.na(mergedData$MAJOR_IND) == FALSE]
  maj_inds = maj_inds[order(mergedData$TERM_CODE[mergedData$STU_INST_UID == id & is.na(mergedData$MAJOR_IND) == FALSE ],decreasing=TRUE)]
  
  mergedData$MAJOR_IND[mergedData$STU_INST_UID == id & is.na(mergedData$MAJOR_IND)] =  maj_inds[1]
  mergedData$MAJOR_DESC[mergedData$STU_INST_UID == id & is.na(mergedData$MAJOR_DESC)] =  majs[1]
}

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
mergedData <- mergedData[(mergedData$TERM_CODE == 201208 & 
                              mergedData$COURSE_ACRONYM =='PHYS' & 
                              mergedData$COURSE_NUMBER =='2211') == FALSE,]
 mergedData <- mergedData[(mergedData$TERM_CODE == 201208 & 
                               mergedData$COURSE_ACRONYM == 'GEOL' & 
                               mergedData$COURSE_NUMBER == '1121') == FALSE,]
 

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
                                                  levels=c(10,20,30,40), 
                                                  ordered=TRUE))
mergedData$FATHER_HIGHEST_GRADE_CODE <- as.numeric(factor(mergedData$FATHER_HIGHEST_GRADE_CODE, 
                                                          levels=c(NA,1,2,3,4), 
                                                          ordered=TRUE))
mergedData$MOTHER_HIGHEST_GRADE_CODE <- as.numeric(factor(mergedData$MOTHER_HIGHEST_GRADE_CODE, 
                                                          levels=c(NA,1,2,3,4), 
                                                          ordered=TRUE))
mergedData$INST_COURSE_GRADE <- as.numeric(factor(mergedData$INST_COURSE_GRADE, 
                                                  levels=c('F','D','C','B','A'), 
                                                  ordered=TRUE))
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
     'MAJOR_IND',
    'COURSE_SEC_IDENTIFIER',
    'STU_INST_UID'
  )
)
# get first imputation
mergedData <- imputedData$imputations[[1]]
remove(imputedData)
mergedData$TERM_ORD <- factor(mergedData$TERM_ORD, ordered=T)
mergedData$TERM_ORD <- ts(mergedData$TERM_ORD)


ids = unique(pracData$STU_INST_UID)
terms = unique(pracData$TERM_CODE)

for(id in ids)
       {
          terms = sort(unique(testData$TERM_CODE[testData$STU_INST_UID == id]))
                 if(length(terms) > 1)
               {
       for(i in 2:length(terms))
       {
                  testData$MAJOR_CHANGES_LAGGED[testData$TERM_CODE == terms[i] & testData$STU_INST_UID == id] = testData$MAJOR_CHANGES[testData$TERM_CODE == terms[i-1] & testData$STU_INST_UID == id]
                  }
             }
  
         }


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




mergedData$BIOL_HRS = 0
mergedData$CHEM_HRS = 0
mergedData$MATH_HRS = 0
mergedData$CSCI_HRS = 0
mergedData$ASTR_HRS = 0
mergedData$ECON_HRS = 0
mergedData$ENSC_GEOL_HRS = 0
mergedData$GC1Y_HRS = 0
mergedData$KINS_HRS = 0
mergedData$PHYS_HRS = 0
mergedData$SPAN_FREN_HRS = 0
mergedData$PSYC_HRS = 0
mergedData$GEOG_HRS = 0

mergedData$PREV_TERM_GPA = 0


mergedData$INST_COURSE_GRADE <- mergedData$INST_COURSE_GRADE - 1
#pracData$INST_COURSE_GRADE <- pracData$INST_COURSE_GRADE - 1
mergedData$Quality_Points = mergedData$COURSE_ATTEMPTED_HRS*mergedData$INST_COURSE_GRADE
View(mergedData)
for(i in 1:nrow(mergedData))
  mergedData[i, 'uniq_stu_id'] = paste(mergedData[i, 'STU_INST_UID'], mergedData[i,'TERM_CODE'])
remove(i)
uniq_stu_ids <- unique(mergedData$uniq_stu_id)
mergedData$TOTAL_QUALITY_POINTS = 0
for(uniq_stu_id in uniq_stu_ids)
{
  mergedData$TOTAL_QUALITY_POINTS[mergedData$uniq_stu_id == uniq_stu_id] = sum(mergedData$Quality_Points
  [mergedData$uniq_stu_id == uniq_stu_id])
  
}
for(uniq_stu_id in uniq_stu_ids)
{
  mergedData$TOTAL_CRSE_ATMP_HRS[mergedData$uniq_stu_id == uniq_stu_id] = sum(mergedData$COURSE_ATTEMPTED_HRS[mergedData$uniq_stu_id == uniq_stu_id])}
mergedData$PREV_TERM_GPA = mergedData$TOTAL_QUALITY_POINTS / mergedData$TOTAL_CRSE_ATMP_HRS


mergedData$PREV_TERM_GPA <- lead(mergedData$PREV_TERM_GPA, k= +1)
pracData = mergedData
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

stem_majors = c(
  "Environmental Science"  ,        "Computer Science"      ,         "Research and Experimental Psyc"
  ,"Physics, General" ,  "Biology, General" , "Psychology, General" ,   "Chemistry, General",  "Mathematics"     
  ,"Web/Multimedia Mgmt & Webmstr" ,"Computer/Information Sci, Gen" 
)

stem_ids = unique(pracData$STU_INST_UID[pracData$MAJOR_DESC %in% stem_majors])

for(id in stem_ids) {
  terms = sort(unique(pracData$TERM_CODE[pracData$STU_INST_UID == id]))
  
  if (length(terms) == 1) {  
    pracData$dropped[pracData$TERM_CODE == terms & pracData$STU_INST_UID == id] = 1
    next
  }
  decided = 0
  for(i in 2:length(terms)) {
    last_major =  unique(pracData$MAJOR_DESC[pracData$TERM_CODE == terms[i-1] & pracData$STU_INST_UID == id])
    cur_major =  unique(pracData$MAJOR_DESC[pracData$TERM_CODE == terms[i] & pracData$STU_INST_UID == id])
    if (decided == 0 & last_major %in% stem_majors) {
      decided = 1
    }
    else if (decided == 1 & last_major != cur_major ) {
      pracData$dropped[pracData$TERM_CODE == terms[i] & pracData$STU_INST_UID == id] = 1
      pracData = pracData[(pracData$TERM_CODE > terms[i] & pracData$STU_INST_UID == id)== FALSE, ]
      break
    }
  }
  if (decided == 0) {
    pracData = pracData[pracData$STU_INST_UID != id,]
  }
}

 for(id in ids)
{
  for(term in terms)
  {
    pracData$BIOL_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "BIOL"])
    pracData$CHEM_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "CHEM"])
    pracData$MATH_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "MATH"])
    pracData$CSCI_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "CSCI"])
    pracData$ASTR_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "ASTR"])
    pracData$ECON_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "ECON"])
    pracData$ENSC_GEOL_HRS[pracData$STU_INST_UID == id 
                           & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                              & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "ENSC_GEOL"])
    pracData$GC1Y_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "GC1Y"])
    pracData$KINS_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "KINS"])
    pracData$PHYS_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "PHYS"])
    pracData$PSYC_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "PSYC"])
    pracData$GEOG_HRS[pracData$STU_INST_UID == id 
                      & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                         & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "GEOG"])
    pracData$SPAN_FREN_HRS[pracData$STU_INST_UID == id 
                           & pracData$TERM_CODE == term] = sum(pracData$TOTAL[pracData$STU_INST_UID == id 
                                                                              & pracData$TERM_CODE <= term & pracData$COURSE_ACRONYM == "SPAN_FREN"])
    
  }
  
}

noFresData = pracData[which(pracData$INST_CUM_HRS_ATTEMPTED > 29),]

table(pracData$INST_CUM_HRS_ATTEMPTED, useNA = 'ifany')
table(pracData$INST_CUM_GPA, useNA = 'ifany')
table(pracData$MAJOR_CHANGES, useNA = 'ifany')
table(pracData$INST_CUM_HRS_EARNED, useNA = 'ifany')

stu_ids = unique(pracData$STU_INST_UID)
pracData$survTime = pracData$TERM_ORD

for (id in stu_ids) {
    pracData$survTime[pracData$STU_INST_UID == id] = pracData$survTime[pracData$STU_INST_UID == id] - min(pracData$survTime[pracData$STU_INST_UID == id])
}
