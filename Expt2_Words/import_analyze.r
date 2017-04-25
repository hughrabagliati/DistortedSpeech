library(lme4)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(doBy)
## for bootstrapping 95% confidence intervals -- from Mike Frank https://github.com/langcog/KTE/blob/master/mcf.useful.R
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #- mean(x,na.rm=na.rm)}



read_TD_data <- function(csv,type){
  word <- bind_rows(lapply(csv, read.csv, colClasses = c("num_siblings"="character","birth_order" = "character","subject_id" = "character")))
  
  word$DOT <- parse_date_time(word$date_stamp, "%m/%d/%Y")
  word$DOB <- parse_date_time(word$date_of_birth,c("%Y-%m-%d","%d/%m.%y"))
  #word$age_weeks <- difftime(word$DOT,word$DOB,units = "weeks")
  #word$age <- as.numeric(floor(word$age_weeks/26)/2)
  #word <- subset(word, age > 1 & age <= 9)
  #word$age <- ordered(word$age)
  word$Experiment = type
  word$trial_type <- as.factor(word$trial_type)
  #word <- subset(word, trial_type == "critical" )
  word$resp <- ifelse(word$accuracy == "true",1,0)
  word$speaker_match <- as.factor(ifelse((word$plain == "fem_plain" & word$sws == "fem_sws")|(word$plain == "mal_plain" & word$sws == "mal_sws"),"Speaker Match","Speaker Mismatch"))
  return(word)
}

process_sine_data <- function(path,subject_list){
  
  sine_words_csv <- dir(path = path,pattern='*.csv$', recursive = T,full.names = T)
  
  sine_words <- read_TD_data(sine_words_csv,"Sine Words")
  sine_words$testing_loc <- as.factor(sine_words$testing_loc)
  sine_words<- subset(sine_words,trial_type == "critical")
  sine_words <- subset(sine_words, testing_loc != "pilot")
  sine_words$clarity <- as.factor(ifelse(sine_words$instruction_number %in% c(1,2),"clear","distorted"))
  sine_words$novelty <- as.factor(ifelse(sine_words$distance %in% c("different"),"novel","familiar"))
  sine_words$age <- NA
  sine_words$lang <- NA
  
  sine_words$choose_unmentioned <- ifelse(sine_words$chosen_pic == sine_words$sound1_item,0,ifelse(sine_words$chosen_pic == sine_words$sound2_item,0,1))
  subj_list <- read.csv(subject_list, header =  T)
  sine_words <- merge(sine_words,subj_list, by = "subject_id")
  }

children <- process_sine_data("./Expt2_Words/CHILDREN/","./Expt2_Words/children_subject_list.csv")
children$Group <- "Children"
adults <- process_sine_data("./Expt2_Words/ADULTS/","./Expt2_Words/adult_subject_list.csv")
adults$Group <- "Adults: 1st Lang"
italians <- process_sine_data("./Expt2_Words/ITALIAN/","./Expt2_Words/italian_subject_list.csv")
italians$Group <- "Adults: 2nd Lang"
expt2 <- rbind(children,adults,italians)



summaryBy(resp ~ speaker_match + clarity + distance,data = children)
summaryBy(resp + choose_unmentioned ~ age_years + distance , data =subset(children, instruction_number == 3))
summary(glmer(resp ~  distance*scale(age_years) +(1+distance|subject_id), data = subset(children, instruction_number == 3 & distance != "different"), family = "binomial"))