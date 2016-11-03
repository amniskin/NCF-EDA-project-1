## mcavoy_code_compiled.r
## EDA Project_1 - Education

## install.packages("ggplot2")
## install.packages("ggrepel")
library(dplyr)
library(ggplot2)
library(ggrepel)

## Data produced for two year pooled cohorts. 2007-08 includes data for 2006-07 and 2007-08
## COSTT4_A and COSTT4_P not reported prior to 2009
## Thesis: How does cost, debt, percent completion, and predominant degree type affect median income after ten years.


## Will need to set working directory locally
setwd("C:/Users/homur/OneDrive/New College/EDA/Project1/NCF-EDA-project-1")

data_2003_df <- read.csv("../Data/MERGED2003_04_PP.csv", na.string=c("NULL",""))
dsub_03 <- select(data_2003_df, OPEID6, INSTNM, STABBR, CONTROL, COUNT_NWNE_P10, COUNT_WNE_P10, 
			MN_EARN_WNE_P10, MD_EARN_WNE_P10, PCT10_EARN_WNE_P10, PCT25_EARN_WNE_P10, 
			PCT75_EARN_WNE_P10, PCT90_EARN_WNE_P10, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2)
dsub_03 <- mutate(dsub_03, year=2003)

data_2005_df <- read.csv("../Data/MERGED2005_06_PP.csv", na.string=c("NULL",""))
dsub_05 <- select(data_2005_df, OPEID6, INSTNM, STABBR, CONTROL, COUNT_NWNE_P10, COUNT_WNE_P10, 
			MN_EARN_WNE_P10, MD_EARN_WNE_P10, PCT10_EARN_WNE_P10, PCT25_EARN_WNE_P10, 
			PCT75_EARN_WNE_P10, PCT90_EARN_WNE_P10, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2)
dsub_05 <- mutate(dsub_05, year=2005)

data_2007_df <- read.csv("../Data/MERGED2007_08_PP.csv", na.string=c("NULL",""))
dsub_07 <- select(data_2007_df, OPEID6, INSTNM, STABBR, CONTROL, COUNT_NWNE_P10, COUNT_WNE_P10, 
			MN_EARN_WNE_P10, MD_EARN_WNE_P10, PCT10_EARN_WNE_P10, PCT25_EARN_WNE_P10, 
			PCT75_EARN_WNE_P10, PCT90_EARN_WNE_P10, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2)
dsub_07 <- mutate(dsub_07, year=2007)

data_2009_df <- read.csv("../Data/MERGED2009_10_PP.csv", na.string=c("NULL",""))
dsub_09 <- select(data_2009_df, OPEID6, INSTNM, STABBR, CONTROL, COUNT_NWNE_P10, COUNT_WNE_P10, 
			MN_EARN_WNE_P10, MD_EARN_WNE_P10, PCT10_EARN_WNE_P10, PCT25_EARN_WNE_P10, 
			PCT75_EARN_WNE_P10, PCT90_EARN_WNE_P10, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2)
dsub_09 <- mutate(dsub_09, year=2009)

data_2011_df <- read.csv("../Data/MERGED2011_12_PP.csv", na.string=c("NULL",""))
dsub_11 <- select(data_2011_df, OPEID6, INSTNM, STABBR, CONTROL, COUNT_NWNE_P10, COUNT_WNE_P10, 
			MN_EARN_WNE_P10, MD_EARN_WNE_P10, PCT10_EARN_WNE_P10, PCT25_EARN_WNE_P10, 
			PCT75_EARN_WNE_P10, PCT90_EARN_WNE_P10, DEBT_MDN, DEBT_N, COSTT4_A, COSTT4_P, 
			CDR2)
dsub_11 <- mutate(dsub_11, year=2011)

bind_years <- bind_rows  (dsub_03, dsub_05)
bind_years <- bind_rows (bind_years, dsub_07)
bind_years <- bind_rows (bind_years, dsub_09)
bind_years <- bind_rows (bind_years, dsub_11)

## Save bound years
write.csv(bind_years, file = "Education_costs_years.csv")

## Start here with education year data for cost variables
years <- read.csv("Education_costs_years.csv")

years <- filter(data.frame(years), 
	!is.na(c(1)), (1)!="NULL",
	!is.na(INSTNM), 
	!is.na(COUNT_NWNE_P10), (COUNT_NWNE_P10)!="NULL", (COUNT_NWNE_P10)!="PrivacySuppressed", 
	!is.na(COUNT_WNE_P10), (COUNT_WNE_P10)!="NULL", (COUNT_WNE_P10)!="PrivacySuppressed",
	!is.na(MN_EARN_WNE_P10), (MN_EARN_WNE_P10)!="NULL", (MN_EARN_WNE_P10)!="PrivacySuppressed", 
	!is.na(MD_EARN_WNE_P10), (MD_EARN_WNE_P10)!="NULL", (MD_EARN_WNE_P10)!="PrivacySuppressed",
	!is.na(PCT10_EARN_WNE_P10), (PCT10_EARN_WNE_P10)!="NULL", (PCT10_EARN_WNE_P10)!="PrivacySuppressed", 
	!is.na(PCT25_EARN_WNE_P10), (PCT25_EARN_WNE_P10)!="NULL", (PCT25_EARN_WNE_P10)!="PrivacySuppressed",
	!is.na(PCT75_EARN_WNE_P10), (PCT75_EARN_WNE_P10)!="NULL", (PCT75_EARN_WNE_P10)!="PrivacySuppressed", 
	!is.na(PCT90_EARN_WNE_P10), (PCT90_EARN_WNE_P10)!="NULL", (PCT90_EARN_WNE_P10)!="PrivacySuppressed",
	!is.na(DEBT_MDN), (DEBT_MDN)!="NULL", (DEBT_MDN)!="PrivacySuppressed",
	!is.na(DEBT_N), (DEBT_N)!="NULL", (DEBT_N)!="PrivacySuppressed",
	!is.na(CDR2), (CDR2)!="NULL"
)

years <- filter(years, 
	!is.na(INSTNM), 
	COUNT_NWNE_P10 != "PrivacySuppressed", 
	COUNT_WNE_P10 != "PrivacySuppressed",
	MN_EARN_WNE_P10 != "PrivacySuppressed", 
	MD_EARN_WNE_P10 != "PrivacySuppressed",
	PCT10_EARN_WNE_P10 != "PrivacySuppressed", 
	PCT25_EARN_WNE_P10 != "PrivacySuppressed",
	PCT75_EARN_WNE_P10 != "PrivacySuppressed", 
	PCT90_EARN_WNE_P10 != "PrivacySuppressed",
	DEBT_MDN != "PrivacySuppressed",
	DEBT_N != "PrivacySuppressed"
)

## Removing default3 years since pretty empty along with tuition of program schools
years <- select(years, -X, -COSTT4_P)
colnames(years) <- c("OPEID6", "INSTNM", "STATE", "CONTROL", "NOTWORKING", "WORKING", "MEAN_EARNINGS", "MEDIAN_EARNINGS", "PERCENT10_EARNINGS", "PERCENT25_EARNINGS", "PERCENT75_EARNINGS", "PERCENT90_EARNINGS", "MEDIAN_DEBT", "NUM_DEBT", "TUITION", "DEFAULT2", "YEAR")
str(years)

## Convert factor variables to numeric
years_first <- select(years, 1:4)
year_date <- select(years, 17)
years_num <- select(years, 5:16) %>% mutate_each(funs(as.character)) %>% mutate_each(funs(as.numeric))
years <- bind_cols(years_first, years_num)
years <- bind_cols(years, year_date)

write.csv(bind_years, file = "Cleaned_education_costs_years.csv")

## --- Start Analysis --- ##
years <- read.csv("Cleaned_education_costs_years.csv")

## total average debt
years %>% summarise(total_avg_debt=mean(MEDIAN_DEBT))

## Average debt and earnings by year
debt_to_earnings <- years %>% group_by(YEAR) %>% 
	summarise(avg_debt = mean(MEDIAN_DEBT), avg_earnings = mean(MEDIAN_EARNINGS))
debt_to_earnings

## Smooth function for earnings and debt over year
y01 <- ggplot(debt_to_earnings, aes(x=YEAR)) + 
	geom_smooth(aes(y=avg_debt, size=1.3), method="lm", formula = y ~ splines::bs(x,3), se=FALSE, colour = "#FF9999") + 
	geom_smooth(aes(y=avg_earnings, size=1.3), method="lm", formula = y ~ splines::bs(x,3), se=FALSE, colour = "darkred") + 
	ylab("Dollars") + xlab("Year") + ggtitle("Earnings and debt by year")
y01
##dev.copy(png, 'Images/mcavoy_project1_year_earnings_debt.png')  ## saves image
##dev.off()

## Line function for earnings and debt over year
##y01 <- ggplot(debt_to_earnings, aes(x=YEAR)) + 
##	geom_line(aes(y=avg_debt), colour = "blue") + 
##	geom_line(aes(y=avg_earnings), colour = "darkred") + 
##	ylab("Dollars") + xlab("Year") + ggtitle("Earnings and debt by year")
##y01


## Relationship between debt and earnings
## Shows, as average debt increases, your earnings will increase as well, no matter what bracket you are in
y02 <- ggplot(data=years, aes(x=MEDIAN_DEBT)) + 
	geom_smooth(aes(y=PERCENT10_EARNINGS), method="lm", se=FALSE, colour="darkgreen") + 
	geom_smooth(aes(y=PERCENT25_EARNINGS), method="lm", se=FALSE, colour="green") + 
	geom_smooth(aes(y=MEDIAN_EARNINGS), method="lm", se=FALSE, colour="lightgreen") + 
	geom_smooth(aes(y=PERCENT75_EARNINGS), method="lm", se=FALSE, colour="pink") +
	geom_smooth(aes(y=PERCENT90_EARNINGS), method="lm", se=FALSE, colour="purple") + 
	ylab("Earnings") + xlab("Debt") + ggtitle("Relationship between debt and earnings")
y02
##dev.copy(png, 'Images/mcavoy_project1_debt_to_earnings.png')
##dev.off()

## Plot of debt vs earnings by state
state_debt <- years %>% group_by(STATE) %>% summarise(avg_debt=mean(MEDIAN_DEBT), avg_earnings=mean(MEAN_EARNINGS)) %>% arrange(avg_debt)
y03 <- ggplot(data=state_debt, aes(x=avg_debt, y=avg_earnings)) + 
	geom_point(shape = 1, size = 2.5) + geom_text_repel(aes(label=STATE), size = 4) +
	xlab("Debt") + ylab("Earnings") + ggtitle("Relationship between average state debt and earnings")
y03
##dev.copy(png, 'Images/mcavoy_project1_plot_state_debt_earnings.png')
##dev.off()

## Average debt (2007,2009,2011), only showing top and bottom three
end_states <- state_debt %>% slice(c(1:3,52:54)) %>% arrange(avg_debt)
y04 <- ggplot(data=end_states, aes(x=STATE, y=avg_debt)) + 
	geom_bar(aes(fill=STATE), stat="identity") + 
	scale_x_discrete(limits=c("PR","VI","WY","PA","VT","RI")) + 
	ylab("Debt") + xlab("State") + ggtitle("Lowest and highest debt states")
y04
##dev.copy(png, 'Images/mcavoy_project1_top_bottom_debt_states.png')
##dev.off()





