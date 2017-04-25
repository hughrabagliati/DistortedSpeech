library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
## for bootstrapping 95% confidence intervals -- from Mike Frank https://github.com/langcog/KTE/blob/master/mcf.useful.R
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #- mean(x,na.rm=na.rm)}
csv <- dir(path = "./data",pattern='*.csv$', recursive = T,full.names = T)
word <- bind_rows(lapply(csv, read.csv))

word$DOT <- parse_date_time(word$date_stamp, "%m/%d/%Y")
word$DOB <- parse_date_time(word$date_of_birth,c("%Y-%m-%d","%d/%m.%y"))
word$age_weeks <- difftime(word$DOT,word$DOB,units = "weeks")
word$age <- as.numeric(floor(word$age_weeks/52))
word$age <- ordered(word$age)

word <- subset(word, age > 1 & age < 9)

word$agegroup <- ifelse(word$age <= 4,"younger than 4","older than 5")
word$agegroup <- ordered(word$agegroup, levels = c("younger than 4","older than 5"))
word <- subset(word, trial_type == "critical" )
word.acc.graph <- word %>%
	select(rt,accuracy,subject_id,age) %>%
	group_by(subject_id,age) %>%
	summarise(acc.m = mean(accuracy,na.rm = T),rt.m = mean(rt,na.rm = T)) %>% 
	group_by(age) %>%
	select(acc.m,rt.m,subject_id,age) %>%
	summarise(acc.mean = mean(acc.m,na.rm = T),acc.sd = sd(acc.m,na.rm=T),rt.mean = mean(rt.m, na.rm = T), rt.sd = sd(rt.m,na.rm = T),acc.low = ci.low(acc.m),acc.high = ci.high(acc.m),rt.low = ci.low(rt.m),rt.high = ci.high(rt.m)) 
dodge <- position_dodge(width=0.9)
ggplot(word.acc.graph, aes(age,acc.mean, fill = age)) +
  geom_bar(stat = "identity",  position = dodge) +
  geom_errorbar(aes(ymax = word.acc.graph$acc.high, ymin = word.acc.graph$acc.low), width=0.25, position = dodge) +
  theme(axis.text.x = element_text(colour = "black", size = 12)) +
  ylab("Accuracy") +
  xlab("Age (years)") + 
  ylim(c(0,1))+guides(fill=FALSE)