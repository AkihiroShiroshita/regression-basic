#multilevel analysis basic
df <- read.csv("C:/Users/akihi/Downloads/pneumocopd_analysis_dryad.csv")
df <- na.omit(df)
#level: id, hospital
#ICC
install.packages("ICC")
library(ICC)
ICCest(as.factor(hospital),hospitalization, data=df, alpha=0.05, CI.type="Smith")
#centering
df$age.m <- ave(df$age, df$hospital)
df$age.cwc <- df$age - df$age.m
round(cor(df[,c("age.m","age.cwc")]),3)
#random intercept model
install.packages("lmerTest")
library(lmerTest)
model1 <- lmer(hospitalization ~ age.cwc+(1|hospital), data=df, REML=FALSE)
summary(model1)
#random slope model
model2 <- lmer(hospitalization ~ age.cwc+(1+age.cwc|hospital), data=df, REML=FALSE)
summary(model2)
#corr=0
model2 <- lmer(hospitalization ~ age.cwc+(1+age.cwc||hospital), data=df, REML=FALSE)
summary(model2)
#hospital effect
df$age.cgm <- df$age - mean(df$age) 
df.age.cm <- df$age.m - mean(df$age)
model3 <- lmer(hospitalization ~ age.m + age.cgm + (1+age.cgm||hospital), data=df, REML=FALSE)
summary(model3)
