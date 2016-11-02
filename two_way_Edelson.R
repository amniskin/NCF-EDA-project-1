# two way anova using the full csv
setwd("~/Desktop/Data_Science/Group_EDA/NCF-EDA-project-1/ignore/")
library(tidyverse)
df_full <- read_csv("complete_data.csv", na = c("", "NA", "NULL", "PrivacySurpressed"))
df <- read_csv("two_way_anova.csv", na=c("", "NA", "NULL", "PrivacySurpressed"))
# convert to correct types for ANOVA
df[c(1,2,6)] <- df %>% select(c(1,2,6)) %>% mutate_each(funs(as.factor))
df[c(3,4,5)] <- df %>% select(c(3,4,5)) %>% mutate_each(funs(as.numeric))

# create aov object
fit <- aov(MD_EARN_WNE_P6 ~ HIGHDEG*year_ID , data=df[complete.cases(df),])

# to look at all of them
TukeyHSD(fit)

# to look at some of them
TukeyHSD(fit, "HIGHDEG")