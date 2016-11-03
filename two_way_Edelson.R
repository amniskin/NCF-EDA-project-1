# two way anova using the full csv
# How does cost, completion, and highest and predominate degree type affect median income 10 years out?
setwd("~/Desktop/Data_Science/Group_EDA/NCF-EDA-project-1/ignore/CollegeScorecard_Raw_Data")
library(tidyverse)
df <- read_csv("anova_median.csv", na=c("","NULL","NA","PrivacySurppressed"))

# enforce proper types
df[c(5:7)] <- df %>% select(c(5:7)) %>% mutate_each(funs(as.factor))
df[c(2:4)] <- df %>% select(c(2:4)) %>% mutate_each(funs(as.numeric))
str(df)

# look at number of complete cases for all columns
nccase <- df %>% complete.cases() %>% sum()
ratio <- nccase/nrow(df)
ratio*100

# look at MD alonge
df %>% select(c(2)) %>% complete.cases() %>% sum()
df %>% select(c(3)) %>% complete.cases() %>% sum()
df %>% select(c(4)) %>% complete.cases() %>% sum()/nrow(df) * 100


# fit an anova with HIGHDEG
fit <- aov(MD_EARN_WNE_P6 ~ HIGHDEG * year_ID , data=df[complete.cases(df),])
summary(fit)
# plot(fit)
# TukeyHSD(fit)

# now with PREDDEG
fit2 <- aov(MD_EARN_WNE_P6 ~ PREDDEG * year_ID, data=df[complete.cases(df),])
summary(fit2)
# plot(fit2)
# TukeyHSD(fit2)

# plot these new results
p1 <- ggplot(data=df[complete.cases(df),], aes(MD_EARN_WNE_P10))
p1 + geom_histogram(binwidth=1000, col="blue") + facet_grid(HIGHDEG ~.)

p2 <- ggplot(data=df[complete.cases(df),], aes(MD_EARN_WNE_P10))
p2 + geom_histogram(binwidth=1000, col="lightblue") + facet_grid(PREDDEG ~.)
