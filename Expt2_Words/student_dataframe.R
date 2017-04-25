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
  word <- bind_rows(lapply(csv, read.csv, colClasses = c("birth_order"="character", "num_siblings"="character", "subject_id"="character")))
  
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


student_data <- function(path,subject_list){

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
# Gradually Build a Wide dataframe for students to analyze 
# Strategy is to build a series of by-participant data frames, and then merge them together



clarity <- sine_words %>% 
  select(subject_id,speaker_match,clarity,resp) %>%
  group_by(subject_id,speaker_match, clarity) %>%
  summarize(resp.mean = mean(resp)) %>%
  spread(clarity,resp.mean)

novelty <- subset(sine_words, clarity == "distorted") %>% 
  select(subject_id,speaker_match,novelty,resp) %>%
  group_by(subject_id,speaker_match, novelty) %>%
  summarize(resp.mean = mean(resp)) %>%
  spread(novelty,resp.mean)

unmentioned <- subset(sine_words, clarity == "distorted") %>% 
  select(subject_id,speaker_match,novelty,choose_unmentioned) %>%
  group_by(subject_id,speaker_match, novelty) %>%
  summarize(unmentioned.mean = mean(choose_unmentioned)) %>%
  spread(novelty,unmentioned.mean)


distance <- subset(sine_words, clarity == "distorted" & novelty == "familiar") %>% 
  select(subject_id,speaker_match,distance,resp) %>%
  group_by(subject_id,speaker_match, distance) %>%
  summarize(resp.mean = mean(resp)) %>%
  spread(distance,resp.mean)



dataframe <- merge(clarity, novelty, by = c("subject_id", "speaker_match"))
dataframe <- merge(dataframe, distance, by = c("subject_id", "speaker_match"))
dataframe <- merge(dataframe, unmentioned, by = c("subject_id", "speaker_match"), suffixes = c("",".unmentioned"))
subj_list <- read.csv(subject_list, header =  T)
dataframe <- merge(dataframe,subj_list, by = "subject_id")
return(dataframe)
}

italian <- student_data("./ITALIAN/","italian_subject_list.csv")


adults <- student_data("./ADULTS/","adult_subject_list.csv")

children <- student_data("./CHILDREN/","children_subject_list.csv")
