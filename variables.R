source('~/Github/DataMunging/DataMunging.R', echo=T)

attach(mergedData)
names(mergedData)

hist(SAT_CRITICAL_READING)                # Normal
hist(SAT_MATH)                            # Normal
hist(SAT_WRITING)                         # Normal
hist(log(EXPECTED_FAMILY_CONTRIBUTION))   # Log -> Normal
hist(HS_GPA)                              # Left Skewed
hist(SALARY)                              # Right Skewed
hist(USG_CUM_GPA)                         # Left Skewed
hist(INST_CUM_GPA)                        # Left Skewed
hist(USG_CUM_HRS_EARNED)                  # Interesting
hist(USG_CUM_HRS_ATTEMPTED)               # Interesting
hist(as.numeric(log(MEAN)))               # Log -> Better
hist(log(as.numeric(MODE)))               # Log -> Better
hist(log(as.numeric(NONZERO_MODE)))       # Log -> Better

