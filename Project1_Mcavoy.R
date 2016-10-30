## EDA Project_1 - Education
## Intend on answering if the data can assess university return on investment
## Will explore quality of data, important and not-so-important variables, and
## ways to normalize data to elucidate interesting findings.

## install.packages("dplyr")
## install.packages("tidyr")
## install.packages("reshape")
library(dplyr)
library(tidyr)
library(reshape)
library(sqldf)

## Will need to set working directory locally
setwd("C:/Users/homur/OneDrive/New College/EDA/Project1")
setwd("C:/Users/homur/OneDrive/New College/EDA/Project1/NCF-EDA-project-1")



## ---- Possibly important variables gleaned from documentation.pdf ----
## Most variables arn't complete will need to refer back to documentation to find
## the appropriate column names.

## Use OPE ID 6 for institution level. OPE ID 8 is for campus level.
## Location - CITY, STABBR, ZIP or LATITUDE, LONGITUDE
## Degree Type - HIGHDEG (highest degree offered), PREDDEG (Predominant degree)
## CONTROL - identifies public, private, nonprofit, private for-profit
## PCIP01-54 - Percentage for Classification of Instructional Programs
## CIPCERT.. - if program offered
## ADM_RATE_ALL - total undergrad admitted/total undergrad applied
## SAT or ACT VR, WR, MT, CM _25 or _75 - SAT 25 or 77 percentile VR reading, WR Writing, MT Math, CM composite score
## COSTT4_A or COSTT4_P - average annual cost for academic year or program-year institutions. Not reported prior to 2009.
## TUITIONFEE_IN, TUITIONFEE_OUT, TUITIONFEE_PROG - tution fee for in-state, out-of-state, and program-year institutions for full-time, first year students. Data not reported prior to 2000.
## NPT4_PUB, _PRIV. Total price minus federal aid. Not reported prior to 2009.
## UGDS - number of degree/certificate seeking undergrads UG is not degree seeking?
## UGDS_WOMEN, WHITE, AIAN.. - Self reported race and gender. Prior to 2008-09 are in seperate variables. Later in single variable as float representing 
## INC_PCT_LO, _M1, _M2, _H1, H2 - percentage of title-IV recieving students with family income data from FAFSA. Not available prior to 2009.
## RET_FT4 - retention rate of first-year students from the previous year 
## PAR_ED_PCT_1STGEN, PAR_ED_PCT_MS, _HS, PS - percent first-gen, and highest education for parents.
## PCTFLOAN - share of students who recieve fed loans. 'Can provide important context to figures related to debt, repayment, and non-repayment..many community colleges are sufficiently low-cost to have low federal loan bororwing rates, and may be difficult to compare borrowing behaviors for those institutions'. Data not avaiable prior to 2009.
## PCTPELL - share of students who recieve pell grants. 'May not capture all low-income students' Undocumented and foreign nationals not eligible for Pell Grants
## DEBT_MON, LO-INC_DEBT_MON - median debt load (eg after graduating) at the institution. Also separated on GRAD_, WDRAW_ for graduated or whithdrawn, low, med, hi income family, pell grant, famale, male, firstgen students
## ** C150_4, C150_L4 - completion rate within 150% of expected completion time for 4-year institution or less than (L4) institutions. Also disaggregated by race (C150_4_* _ASIAN, NRA (non-resident alien). C200_4 for completed within 200% of expected time (like 8 years)
## COMP_ORIG_YR*_RT, WDRAW_ORIG_YR*_RT - share completed at original school, whidrawn from original school
## ** - Earnings. Aggregate for institution; noted STEM students have higher earnings. Only for Title-IV students, and so not representative for low proportion of title-IV receiving students. Also alumni in graduate school are excluded.
## ** MN_EARN_WNE_P*, MD_EARN_WNE_P*, - Inflation adjusted (all?) wage earnings for employed students.
## ** gt_25K_P* *=1-12? - Corresponds to pooled cohort earnings P* years later. Eg. in 2011-2012 gt_25K_p10 corresponds to earnings of students that graduated in 2000-2001 and 2001-2002.
## ** CDR2 - cohort default rate for 2 years or 3 years after graduation.
## ** (RPY_*YR_RT *-1,3,5,7; COMPL_RPY_*, NONCOM_RPY_*- fraction of borrowers not in default. COMPL_RPY_* disaggregated for completed/non-completed students, and by income


## Variables to Remove
## CCBASIC, CCUGPROF, CCSIZSET - carnegie classifications of schools starting 2015
## HBCU, PBI, ANNHI, AANAPII, TRIBAL, HSI, NANTI for special mission or religious affiliation
## PPTUG_EF - proportion of part time students
## APPL_SCHL_PCT_GE2 - Number of schools applied for FAFSA aid
## MARRIED, VETERAN, FIRST_GEN, PCT_BLACK, PCT_ASIAN, POVERTY_RATE - all demographic data, likely won't correspond to success of school
## MALE_WDRAW_ORIG_YR*_RT, CLO_WDRAW_ORIG_YR*_RT - share completed based on some factor. Gender, swiched school or not. Too many disaggregated data to make much sense atm. Might try re-aggregating in normalized (tidy) form.



## ---------------- 2 ---------------- ##

## Will start with a few, then add more as needed.
## OPEID6 - 6-digit OPE ID for institution. UNITID and OPEID8 are other options.
## INSTNM - Institution name

## ---- Earning variables ---- ##
## COUNT_NWNE_P6 - Number of students not working and not enrolled 6 years after entry
## COUNT_WNE_P6 - Number of students working and not enrolled 6 years after entry
## MN_EARN_WNE_P6 - Mean earnings of students working and not enrolled 6 years after entry
## MD_EARN_WNE_P6 - Median earnings of students working and not enrolled 6 years after entry
## PCT10_EARN_WNE_P6 - 10th percentile of earnings of students working and not enrolled 6 years after entry
## PCT25_EARN_WNE_P6 - 25th percentile of earnings of students working and not enrolled 6 years after entry	earnings	6_yrs_after_entry.working_not_enrolled.earnings_percentile.25	integer	
## PCT75_EARN_WNE_P6 - 75th percentile of earnings of students working and not enrolled 6 years after entry	earnings	6_yrs_after_entry.working_not_enrolled.earnings_percentile.75	integer	
## PCT90_EARN_WNE_P6 - 90th percentile of earnings of students working and not enrolled 6 years after entry	earnings	6_yrs_after_entry.working_not_enrolled.earnings_percentile.90	integer	

## ---- Cost and loan variables ---- ##
## DEBT_MDN - The median original amount of the loan principal upon entering repayment
## DEBT_N - The number of students in the median debt cohort
## COSTT4_A - Average cost of attendance for academic year schools. Compare with NPT4_PUB and NPT4_PRIV
## COSTT4_P - Average cost of attendance for program year schools.
## CDR2 - Default rate for 2 years after graduation
## CDR3 - Default rate for 3 years after graduation


## Additional of interest - 
## CONTROL - 1 = public. 2 = private-non-profit. 3 = private-for-profit 
## MN_EARN_WNE_P10 - Mean earnings of students working and not enrolled 10 years after entry
## MD_EARN_WNE_P10 - Median earnings of students working and not enrolled 10 years after entry

## Additional Notes 
## 	Mean and median earnings pooled between 1995-2007.


## Subset data based on desired variables. Might want to clear memory ocasionally
## Clear memory
rm(list=ls()) 


## 1996 - bad data, many variables of interest are NULL.
data_select1 <- select(data_1996_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(data_select1)
dim(data_select1)
str(data_select1)
head(data_select1$MN_EARN_WNE_P6, 20)

## 1997 - Bad data.
data_1997_df <- tbl_df(read.csv("Data/MERGED1997_98_PP.csv"))
dsub_97 <- select(data_1997_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_97)
str(dsub_97)

## 1998 - Bad data.
data_1998_df <- tbl_df(read.csv("Data/MERGED1998_99_PP.csv"))
dsub_98 <- select(data_1998_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_98)
str(dsub_98)

## 1999 - Bad data.
data_1999_df <- tbl_df(read.csv("Data/MERGED1999_00_PP.csv"))
dsub_99 <- select(data_1999_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_99)
str(dsub_99)

## 2000 - Bad data.
data_2000_df <- tbl_df(read.csv("Data/MERGED2000_01_PP.csv"))
dsub_00 <- select(data_2000_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_00)
str(dsub_00)

## 2001 - Bad data.
data_2001_df <- tbl_df(read.csv("Data/MERGED2001_02_PP.csv"))
dsub_01 <- select(data_2001_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_01)
str(dsub_01)

## 2002 - Bad data
data_2002_df <- tbl_df(read.csv("Data/MERGED2002_03_PP.csv"))
dsub_02 <- select(data_2002_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_02)
str(dsub_02)

## 2003 - Has good data. IE. values other than null in COUNT_WNE_p6
data_2003_df <- tbl_df(read.csv("Data/MERGED2003_04_PP.csv"))
data_2003_df <- read.csv("Data/MERGED2003_04_PP.csv")
dsub_03 <- select(data_2003_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_03)
str(dsub_03)

## 2004 - bad data.
data_2004_df <- tbl_df(read.csv("Data/MERGED2004_05_PP.csv"))
dsub_04 <- select(data_2004_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_04)
str(dsub_04)

## 2005 - good data.
data_2005_df <- tbl_df(read.csv("Data/MERGED2005_06_PP.csv"))
dsub_05 <- select(data_2005_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_05)
str(dsub_05)

## 2006 - bad data.
data_2006_df <- tbl_df(read.csv("Data/MERGED2006_07_PP.csv"))
dsub_06 <- select(data_2006_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_06)
str(dsub_06)

## 2007 - good data.
data_2007_df <- tbl_df(read.csv("Data/MERGED2007_08_PP.csv"))
dsub_07 <- select(data_2007_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_07)
str(dsub_07)

## 2008 - bad data.
data_2008_df <- tbl_df(read.csv("Data/MERGED2008_09_PP.csv"))
dsub_08 <- select(data_2008_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_08)
str(dsub_08)

## 2009 - good data.
data_2009_df <- tbl_df(read.csv("Data/MERGED2009_10_PP.csv"))
dsub_09 <- select(data_2009_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_09)
str(dsub_09)

## 2010 - bad data.
data_2010_df <- tbl_df(read.csv("Data/MERGED2010_11_PP.csv"))
dsub_10 <- select(data_2010_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_10)
str(dsub_10)

## 2011 - good data.
data_2011_df <- tbl_df(read.csv("Data/MERGED2011_12_PP.csv"))
dsub_11 <- select(data_2011_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_11)
str(dsub_11)

## 2012 - somewhat data.
data_2012_df <- tbl_df(read.csv("Data/MERGED2012_13_PP.csv"))
dsub_12 <- select(data_2012_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_12)
str(dsub_12)

## 2013 - bad data.
data_2013_df <- tbl_df(read.csv("Data/MERGED2013_14_PP.csv"))
dsub_13 <- select(data_2013_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_13)
str(dsub_13)

## 2014 - bad data.
data_2014_df <- tbl_df(read.csv("Data/MERGED2014_15_PP.csv"))
dsub_14 <- select(data_2013_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(dsub_14)
str(dsub_14)


## -- Startings with 2003, then every other year through 2011
## load data for desired year from above
head(dsub_03[,c(1:3)])
head(dsub_03[,c(4:6)])
head(dsub_03[,c(7:9)])
head(dsub_03[,c(10:12)])
head(dsub_03[,c(13:15)])
head(dsub_03[,c(16:17)])

sum(complete.cases(dsub_03[,2]))
dim(dsub_03)
names(dsub_03)
str(dsub_03) 


## Dropped columns 14, 15, 17 since bad data
dsub_03 <- select(dsub_03, -COSTT4_A, -COSTT4_P, -CDR3)

## Can't immediately convert all to numeric, columns have funny items in them

## -- Cleaning Column 1 OPEID6 -- ##  Used sql statements to find improper data

## Shows some schools are duplicated, will try another id.
## Tried OPEID corresponding to 8 digit number. Still have a couple duplicates, but more importantly,
## differentiates schools like University if Pheonix branches as different.
## Will simply use OPEID6 and keep unique.

sqldf("SELECT OPEID6 AS uniql, count(OPEID6) as co FROM dsub_03 GROUP BY uniql ORDER BY co ASC")

## code dims to see removal of duplicates
dsub_03_clean_01 <- filter(dsub_03, !is.na(OPEID6), (1)!="NULL")
dsub_03_clean_01[,1] <- as.numeric(as.character(dsub_03[,1]))
sqldf("SELECT OPEID6 AS uniql, count(OPEID6) as co FROM dsub_03_clean_01 GROUP BY uniql ORDER BY co ASC")
unique_01 <- duplicated(dsub_03_clean_01$OPEID6)
sum(unique_01)
dim(dsub_03_clean_01)
dsub_03_clean_01 <- dsub_03_clean_01[!unique_01,]
dim(dsub_03_clean_01)


## -- Cleaning Column 2 INSTNM -- ##
## only saw duplicates, might not need to filter if important
sqldf("SELECT INSTNM as s1 FROM dsub_03 ORDER BY s1")

dsub_03_clean_02 <- filter(dsub_03, !is.na(INSTNM))
unique_02 <- duplicated(dsub_03_clean_02$INSTNM)
sum(unique_02)
dim(dsub_03_clean_02)
dsub_03_clean_02 <- dsub_03_clean_02[!unique_02,]
dim(dsub_03_clean_02)
sqldf("SELECT INSTNM as s1 FROM dsub_03_clean_02 ORDER BY s1")


## -- Cleaning Column 3 CONTROL -- ##
sqldf("SELECT CONTROL as s1 FROM dsub_03 GROUP BY s1 ORDER BY s1")

## data is clean, only need to convert to numeric
dsub_03_clean_03 <- dsub_03
dsub_03_clean_03[,3] <- as.numeric(as.character(dsub_03_clean_03[,3]))
class(dsub_03_clean_03[,3])


## -- Cleaning Column 4 COUNT_NWNE_P6 -- ##
## sqldf("SELECT COUNT_NWNE_P6 AS p6 FROM dsub_03_clean GROUP BY p6 ORDER BY p6 DESC")

## Found two strings in column 4: "NULL" and "PrivacySuppressed"
## used filter to remove NA's, and the strings. Then converted column to numeric
 
dsub_03_clean_04 <- filter(data.frame(dsub_03), !is.na(COUNT_NWNE_P6), (COUNT_NWNE_P6)!="NULL", (COUNT_NWNE_P6)!="PrivacySuppressed")
dsub_03_clean_04[,4] <- as.numeric(as.character(dsub_03_clean_04[,4]))
dim(dsub_03_clean_04)


## -- Cleaning Column 5 COUNT_WNE_P6 -- ##
sqldf("SELECT COUNT_WNE_P6 AS p6 FROM dsub_03_clean_05 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_05 <- filter(data.frame(dsub_03), !is.na(COUNT_WNE_P6), (COUNT_WNE_P6)!="NULL", (COUNT_WNE_P6)!="PrivacySuppressed")
dsub_03_clean_05[,5] <- as.numeric(as.character(dsub_03_clean_05[,5]))
dim(dsub_03_clean_05)


## -- Cleaning Column 6 MN_EARN_WNE_P6 -- ##
sqldf("SELECT MN_EARN_WNE_P6 AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_06 <- filter(data.frame(dsub_03), !is.na(MN_EARN_WNE_P6), (MN_EARN_WNE_P6)!="NULL", (MN_EARN_WNE_P6)!="PrivacySuppressed")
dsub_03_clean_06[,6] <- as.numeric(as.character(dsub_03_clean_06[,6]))
dim(dsub_03_clean_06)


## -- Cleaning Column 7 MD_EARN_WNE_P6 -- ##
sqldf("SELECT MD_EARN_WNE_P6 AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_07 <- filter(data.frame(dsub_03), !is.na(MD_EARN_WNE_P6), (MD_EARN_WNE_P6)!="NULL", (MD_EARN_WNE_P6)!="PrivacySuppressed")
dsub_03_clean_07[,7] <- as.numeric(as.character(dsub_03_clean_07[,7]))
dim(dsub_03_clean_07)



## -- Cleaning Column 8 PCT10_EARN_WNE_P6 -- ##
sqldf("SELECT PCT10_EARN_WNE_P6 AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_08 <- filter(data.frame(dsub_03), !is.na(PCT10_EARN_WNE_P6), (PCT10_EARN_WNE_P6)!="NULL", (PCT10_EARN_WNE_P6)!="PrivacySuppressed")
dsub_03_clean_08[,8] <- as.numeric(as.character(dsub_03_clean_08[,8]))
dim(dsub_03_clean_08)


## -- Cleaning Column 9 PCT25_EARN_WNE_P6 -- ##
sqldf("SELECT PCT25_EARN_WNE_P6 AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_09 <- filter(data.frame(dsub_03), !is.na(PCT25_EARN_WNE_P6), (PCT25_EARN_WNE_P6)!="NULL", (PCT25_EARN_WNE_P6)!="PrivacySuppressed")
dsub_03_clean_09[,9] <- as.numeric(as.character(dsub_03_clean_09[,9]))
dim(dsub_03_clean_09)


## -- Cleaning Column 10 PCT75_EARN_WNE_P6 -- ##
sqldf("SELECT PCT75_EARN_WNE_P6 AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_10 <- filter(data.frame(dsub_03), !is.na(PCT75_EARN_WNE_P6), (PCT75_EARN_WNE_P6)!="NULL", (PCT75_EARN_WNE_P6)!="PrivacySuppressed")
dsub_03_clean_10[,10] <- as.numeric(as.character(dsub_03_clean_10[,10]))
dim(dsub_03_clean_10)


## -- Cleaning Column 11 PCT90_EARN_WNE_P6 -- ##
sqldf("SELECT PCT90_EARN_WNE_P6 AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_11 <- filter(data.frame(dsub_03), !is.na(PCT90_EARN_WNE_P6), (PCT90_EARN_WNE_P6)!="NULL", (PCT90_EARN_WNE_P6)!="PrivacySuppressed")
dsub_03_clean_11[,11] <- as.numeric(as.character(dsub_03_clean_11[,11]))
dim(dsub_03_clean_11)


## -- Cleaning Column 12 DEBT_MDN -- ##
sqldf("SELECT DEBT_MDN AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_12 <- filter(data.frame(dsub_03), !is.na(DEBT_MDN), (DEBT_MDN)!="NULL", (DEBT_MDN)!="PrivacySuppressed")
dsub_03_clean_12[,12] <- as.numeric(as.character(dsub_03_clean_12[,12]))
dim(dsub_03_clean_12)


## -- Cleaning Column 13 DEBT_N -- ##
sqldf("SELECT DEBT_N AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is PrivacySuppressed and NULL, will filter out
dsub_03_clean_13 <- filter(data.frame(dsub_03), !is.na(DEBT_N), (DEBT_N)!="NULL", (DEBT_N)!="PrivacySuppressed")
dsub_03_clean_13[,13] <- as.numeric(as.character(dsub_03_clean_13[,13]))
dim(dsub_03_clean_13)


## -- Cleaning Column 14 CDR2 -- ##
sqldf("SELECT CDR2 AS p6 FROM dsub_03 GROUP BY p6 ORDER BY p6 DESC")
## There is NULL, will filter out
dsub_03_clean_14 <- filter(data.frame(dsub_03), !is.na(CDR2), (CDR2)!="NULL")
dsub_03_clean_14[,14] <- as.numeric(as.character(dsub_03_clean_14[,14]))
dim(dsub_03_clean_14)


## -- Now that each column has been looked over, need to clean all together and convert to numeric
dsub_03_clean <- filter(data.frame(dsub_03), 
	!is.na(OPEID6), (1)!="NULL",
	!is.na(INSTNM), 
	!is.na(COUNT_NWNE_P6), (COUNT_NWNE_P6)!="NULL", (COUNT_NWNE_P6)!="PrivacySuppressed", 
	!is.na(COUNT_WNE_P6), (COUNT_WNE_P6)!="NULL", (COUNT_WNE_P6)!="PrivacySuppressed",
	!is.na(MN_EARN_WNE_P6), (MN_EARN_WNE_P6)!="NULL", (MN_EARN_WNE_P6)!="PrivacySuppressed", 
	!is.na(MD_EARN_WNE_P6), (MD_EARN_WNE_P6)!="NULL", (MD_EARN_WNE_P6)!="PrivacySuppressed",
	!is.na(PCT10_EARN_WNE_P6), (PCT10_EARN_WNE_P6)!="NULL", (PCT10_EARN_WNE_P6)!="PrivacySuppressed", 
	!is.na(PCT25_EARN_WNE_P6), (PCT25_EARN_WNE_P6)!="NULL", (PCT25_EARN_WNE_P6)!="PrivacySuppressed",
	!is.na(PCT75_EARN_WNE_P6), (PCT75_EARN_WNE_P6)!="NULL", (PCT75_EARN_WNE_P6)!="PrivacySuppressed", 
	!is.na(PCT90_EARN_WNE_P6), (PCT90_EARN_WNE_P6)!="NULL", (PCT90_EARN_WNE_P6)!="PrivacySuppressed",
	!is.na(DEBT_MDN), (DEBT_MDN)!="NULL", (DEBT_MDN)!="PrivacySuppressed",
	!is.na(DEBT_N), (DEBT_N)!="NULL", (DEBT_N)!="PrivacySuppressed",
	!is.na(CDR2), (CDR2)!="NULL"
)

dsub_03_clean[,2] <- as.character(dsub_03_clean[,2])
dsub_03_clean[,3] <- as.numeric(as.character(dsub_03_clean[,3]))
dsub_03_clean[,4] <- as.numeric(as.character(dsub_03_clean[,4]))
dsub_03_clean[,5] <- as.numeric(as.character(dsub_03_clean[,5]))
dsub_03_clean[,6] <- as.numeric(as.character(dsub_03_clean[,6]))
dsub_03_clean[,7] <- as.numeric(as.character(dsub_03_clean[,7]))
dsub_03_clean[,8] <- as.numeric(as.character(dsub_03_clean[,8]))
dsub_03_clean[,9] <- as.numeric(as.character(dsub_03_clean[,9]))
dsub_03_clean[,10] <- as.numeric(as.character(dsub_03_clean[,10]))
dsub_03_clean[,11] <- as.numeric(as.character(dsub_03_clean[,11]))
dsub_03_clean[,12] <- as.numeric(as.character(dsub_03_clean[,12]))
dsub_03_clean[,13] <- as.numeric(as.character(dsub_03_clean[,13]))
dsub_03_clean[,14] <- as.numeric(as.character(dsub_03_clean[,14]))

unique_01 <- duplicated(dsub_03_clean$OPEID6)
dsub_03_clean <- dsub_03_clean[!unique_01,]
unique_02 <- duplicated(dsub_03_clean$INSTNM)
dsub_03_clean <- dsub_03_clean[!unique_02,]

## Will change column names to easier to read
colnames(dsub_03_clean) <- c("OPEID6", "INSTNM", "CONTROL", "NOTWORKING", "WORKING", "MEAN_EARNINGS", "MEDIAN_EARNINGS", "PERCENT10_EARNINGS", "PERCENT25_EARNINGS", "PERCENT75_EARNINGS", "PERCENT90_EARNINGS", "MEDIAN_DEBT", "NUM_DEBT", "DEFAULT2")

dim(dsub_03_clean)
head(dsub_03_clean)

## We are left with 3176 x 14 data.

## ---------------- 3 ---------------- ##
## Want to look at debt and earnings to begin with.

## making copy so can easily break and recover
s2_data <- dsub_03_clean
str(s2_data)

## Want to find highest debt and lowest debt schools
s2_debt <- select(s2_data, INSTNM, MEDIAN_DEBT) %>% arrange(desc(MEDIAN_DEBT))
head(s2_debt); tail(s2_debt)

## Want to find highest median earnings schools
s2_earn <- select(s2_data, INSTNM, MEDIAN_EARNINGS, MEAN_EARNINGS) %>% arrange(desc(MEDIAN_EARNINGS))
head(s2_earn); tail(s2_earn)


