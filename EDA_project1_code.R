## EDA Project_1 - Education

## install.packages("dplyr")
## install.packages("tidyr")
## install.packages("reshape")
library(dplyr)
library(tidyr)
library(reshape)

setwd("C:/Users/homur/OneDrive/New College/EDA/Project1")

rawdata_1996 <- read.csv("Data/MERGED1996_97_PP.csv")
rawdata_1997 <- read.csv("Data/MERGED1997_98_PP.csv")

## Notes. Possibly important variables gleaned from documentation.pdf
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
## --- gt_25K_P* *=1-12? - Corresponds to pooled cohort earnings P* years later. Eg. in 2011-2012 gt_25K_p10 corresponds to earnings of students that graduated in 2000-2001 and 2001-2002.
## ** CDR2 - cohort default rate for 2 years or 3 years after graduation.
## ** (RPY_*YR_RT *-1,3,5,7; COMPL_RPY_*, NONCOM_RPY_*- fraction of borrowers not in default. COMPL_RPY_* disaggregated for completed/non-completed students, and by income


## Variables to Remove
## CCBASIC, CCUGPROF, CCSIZSET - carnegie classifications of schools starting 2015
## HBCU, PBI, ANNHI, AANAPII, TRIBAL, HSI, NANTI for special mission or religious affiliation
## PPTUG_EF - proportion of part time students
## APPL_SCHL_PCT_GE2 - Number of schools applied for FAFSA aid
## MARRIED, VETERAN, FIRST_GEN, PCT_BLACK, PCT_ASIAN, POVERTY_RATE - all demographic data, likely won't correspond to success of school
## MALE_WDRAW_ORIG_YR*_RT, CLO_WDRAW_ORIG_YR*_RT - share completed based on some factor. Gender, swiched school or not. Too many disaggregated data to make much sense atm. Might try re-aggregating in normalized (tidy) form.



data_1996_df <- tbl_df(rawdata_1996)
data_1996_df
str(data_1996_df)
names(data_1996_df)


## ---------------- 2 ---------------- ##
## Want to normalize data.
## Most interesting variables are
## 	..
head(data_1996_df$CDR2)
names(data_1996_df)

## always keep OPEID6

## ONLY PCIP. Melt into single column. Currelty only < PCIP10
## This allows us to group data based on the classification program
data_pcip <- as.data.frame(select(data_1996_df, OPEID6, matches("PCIP0.")))
data_pcip_melt <- melt(data_pcip, id=c("OPEID6"))
names(data_pcip_melt)

filter out na's and null's
dim(data_pcip_melt)
data_pcip_melt <- data_pcip_melt %>% na.omit() %>% filter(value!="NULL")
dim(data_pcip_melt)
head(data_pcip_melt); tail(data_pcip_melt)

## Matches CIP01.. BACHL, CERT1, etc. ^ matches start of name
data_cip <- as.data.frame(select(data_1996_df, OPEID6, matches("^CIP0.")))
names(data_cip)
data_cip_melt <- melt(data_cip, id=c("OPEID6"))
dim(data_cip_melt)
names(data_cip_melt)
data_cip_melt <- data_cip_melt %>% na.omit() %>% filter(value!="NULL")
dim(data_cip_melt)
head(data_cip_melt); tail(data_cip_melt)

data_cpip_pip <- inner_join(data_pcip_melt, data_cip_melt, by="OPEID6")
dim(data_cpip_pip)
head(data_cpip_pip); tails(data_cpip_pip)


## Will start with a few, then add more as needed.
## OPEID6 - 6-digit OPE ID for institution
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


## Additional - 
## CONTROL - 1 = public. 2 = private-non-profit. 3 = private-for-profit 
## MN_EARN_WNE_P10 - Mean earnings of students working and not enrolled 10 years after entry
## MD_EARN_WNE_P10 - Median earnings of students working and not enrolled 10 years after entry


## Subset data based on desired varaibles
data_select1 <- select(data_1996_df, OPEID6, INSTNM, CONTROL, COUNT_NWNE_P6, COUNT_WNE_P6, 
			MN_EARN_WNE_P6, MD_EARN_WNE_P6, PCT10_EARN_WNE_P6, PCT25_EARN_WNE_P6, 
			PCT75_EARN_WNE_P6, PCT90_EARN_WNE_P6, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2, CDR3)
names(data_select1)
dim(data_select1)
str(data_select1)
head(data_select1$MN_EARN_WNE_P6, 20)

## Many are null, will try 1997-2015 years

## Note. Mean and median earnings pooled between 1995-2007.

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

## Clear memory
rm(list=ls()) 

## -- Startings with 2003, then every other year through 2011
## load data from above
head(dsub_03[,c(1:3)])
head(dsub_03[,c(4:6)])
head(dsub_03[,c(7:9)])
head(dsub_03[,c(10:12)])
head(dsub_03[,c(13:15)])
head(dsub_03[,c(16:17)])









