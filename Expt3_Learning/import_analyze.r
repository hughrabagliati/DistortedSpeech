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
  word <- bind_rows(lapply(csv, read.csv, colClasses = c("num_siblings"="character", "accuracy" = "character",
                                                         "birth_order" = "character")))
  
  word$DOT <- parse_date_time(word$date_stamp, "%m/%d/%Y")
  word$DOB <- parse_date_time(word$date_of_birth,c("%Y-%m-%d","%d/%m.%y"))
  word$age_weeks <- as.numeric(difftime(word$DOT,word$DOB,units = "weeks"))
  word$age <- floor(word$age_weeks/52)
  #word <- subset(word, age > 1 & age <= 9)
  word$age <- ordered(word$age)
  word$Experiment = type
  word$trial_type <- as.factor(word$trial_type)
  #word <- subset(word, trial_type == "critical" )
  word$resp <- ifelse(word$accuracy == "true",1,0)
  word$speaker_match <- as.factor(ifelse((word$plain == "fem_plain" & word$sws == "fem_sws")|(word$plain == "mal_plain" & word$sws == "mal_sws"),"Speaker Match","Speaker Mismatch"))
  return(word)
}

process_sine_data <- function(path){
  
  sine_words_csv <- dir(path = path,pattern='*.csv$', recursive = T,full.names = T)
  
  sine_words <- read_TD_data(sine_words_csv,"Sine Words")
  sine_words$testing_loc <- as.factor(sine_words$testing_loc)
  sine_words<- subset(sine_words,trial_type == "critical")
  sine_words <- subset(sine_words, testing_loc != "pilot")
  sine_words$clarity <- NA
  sine_words[sine_words$test_lang == "Clear_After",]$clarity <- as.factor(ifelse(sine_words[sine_words$test_lang == "Clear_After",]$instruction_number %in% c(2,3),"clear","distorted"))
  sine_words[sine_words$test_lang == "Clear_Before" & sine_words$trial_number <=17,]$clarity <- as.factor(ifelse(sine_words[sine_words$test_lang == "Clear_Before" & sine_words$trial_number <=17,]$instruction_number %in% c(1,2),"clear","distorted"))
  sine_words[sine_words$test_lang == "Clear_Before" & sine_words$trial_number >17,]$clarity <- as.factor(ifelse(sine_words[sine_words$test_lang == "Clear_Before" & sine_words$trial_number >17,]$instruction_number %in% c(2,3),"clear","distorted"))
  
  sine_words$novelty <- as.factor(ifelse(sine_words$distance %in% c("different"),"novel","familiar"))
  #sine_words$age <- NA
  #sine_words$lang <- NA
  
  #sine_words$choose_unmentioned <- ifelse(sine_words$chosen_pic == sine_words$sound1_item,0,ifelse(sine_words$chosen_pic == sine_words$sound2_item,0,1))
  #subj_list <- read.csv(subject_list, header =  T)
  #sine_words <- merge(sine_words,subj_list, by = "subject_id")
  return(sine_words)
  }

children <- process_sine_data("./Data")

########
# There was a slight error in the original js script, such that a couple of columns got transposed for
# one condition. So, we need to fix that and then adjust the accuracy scores to account for it
children$re_pic1 = children$sound1_item
children$re_pic2 = children$sound2_item
children$re_pic3 = children$sound3_item
children[children$test_lang == "Clear_After" & children$order %in% c("1_2_2_1","2_1_1_1"),]$re_pic1 <- children[children$test_lang == "Clear_After" & children$order %in% c("1_2_2_1","2_1_1_1"),]$sound2_item 
children[children$test_lang == "Clear_After" & children$order %in% c("1_2_2_1","2_1_1_1"),]$re_pic2 <- children[children$test_lang == "Clear_After" & children$order %in% c("1_2_2_1","2_1_1_1"),]$sound1_item 
children[children$test_lang == "Clear_After" & children$order %in% c("2_1_2_1","1_2_1_1"),]$re_pic2 <- children[children$test_lang == "Clear_After" & children$order %in% c("2_1_2_1","1_2_1_1"),]$sound3_item 
children[children$test_lang == "Clear_After" & children$order %in% c("2_1_2_1","1_2_1_1"),]$re_pic3 <- children[children$test_lang == "Clear_After" & children$order %in% c("2_1_2_1","1_2_1_1"),]$sound2_item 

children[children$trial_number > 17 & children$test_lang == "Clear_Before" & children$order %in% c("1_2_2_1","2_1_1_1"),]$re_pic1 <- children[children$trial_number > 17 & children$test_lang == "Clear_Before" & children$order %in% c("1_2_2_1","2_1_1_1"),]$sound2_item 
children[children$trial_number > 17 & children$test_lang == "Clear_Before" & children$order %in% c("1_2_2_1","2_1_1_1"),]$re_pic2 <- children[children$trial_number > 17 & children$test_lang == "Clear_Before" & children$order %in% c("1_2_2_1","2_1_1_1"),]$sound1_item 
children[children$trial_number > 17 & children$test_lang == "Clear_Before" & children$order %in% c("2_1_2_1","1_2_1_1"),]$re_pic2 <- children[children$trial_number > 17 & children$test_lang == "Clear_Before" & children$order %in% c("2_1_2_1","1_2_1_1"),]$sound3_item 
children[children$trial_number > 17 & children$test_lang == "Clear_Before" & children$order %in% c("2_1_2_1","1_2_1_1"),]$re_pic3 <- children[children$trial_number > 17 & children$test_lang == "Clear_Before" & children$order %in% c("2_1_2_1","1_2_1_1"),]$sound2_item 

children$acc <- NA
children[children$instruction_number == "1",]$acc <- ifelse(children[children$instruction_number == "1",]$chosen_pic == children[children$instruction_number == "1",]$re_pic1,1,0 )
children[children$instruction_number == "2",]$acc <- ifelse(children[children$instruction_number == "2",]$chosen_pic == children[children$instruction_number == "2",]$re_pic2,1,0 )
children[children$instruction_number == "3",]$acc <- ifelse(children[children$instruction_number == "3",]$chosen_pic == children[children$instruction_number == "3",]$re_pic3,1,0 )

# End the script fix
###########################


# Produce an item column, and merge with the item names used in the mcdi (i.e., add american names for)
# querying wordbank
children$item <- children$re_pic1
children[children$instruction_number == "2",]$item <- children[children$instruction_number == "2",]$re_pic2
children[children$instruction_number == "3",]$item <- children[children$instruction_number == "3",]$re_pic3
children <- merge(children, read.csv("tested_words.csv"), by = "item")


children$clarity <- "clear"
children[children$test_lang == "Clear_After",]$clarity <- ifelse(children[children$test_lang == "Clear_After",]$instruction_number %in% c(2,3),"clear","distorted")
children[children$test_lang == "Clear_Before" & children$trial_number <=17,]$clarity <- ifelse(children[children$test_lang == "Clear_Before" & children$trial_number <=17,]$instruction_number %in% c(1,2),"clear","distorted")
children[children$test_lang == "Clear_Before" & children$trial_number >17,]$clarity <- ifelse(children[children$test_lang == "Clear_Before" & children$trial_number >17,]$instruction_number %in% c(2,3),"clear","distorted")
children$clarity <- as.factor(children$clarity)
children$block <- as.factor(ifelse(children$trial_number > 17, "Test","Training"))
children$test_lang <- as.factor(children$test_lang)


# Find out which children didn't complete and exclude them
# We include "extra" participants
num_test_trials <- summaryBy(acc ~ subject_id, 
                             data = subset(children, clarity == "distorted" 
                                           & block == "Test"), FUN = length) 
# exclude non-completers
noncomplete1 <- subset(num_test_trials, acc.length <8)
noncomplete2 <- setdiff(as.vector(unique(as.factor(children$subject_id))),
        as.vector(unique(as.factor(num_test_trials$subject_id))))
# Full set of excluded children based on fussiness etc from Hanna's subject sheet + our noncompleters
exclusion = c("tiY94","hEXBd","e6ael","O27zZ","FRI6o","uiBXd","9sb31","C9Nio",
              "UICHK",noncomplete1,noncomplete2)
children <- subset(children, !subject_id %in% exclusion)

summaryBy(acc ~ test_lang + clarity + block, data = children)

#######
# Exclude trials where they got a clear word wrong
wrong_clear <- summaryBy(acc ~ subject_id + trial_id, data = subset(children, clarity != "distorted")) 
wrong_clear <- subset(wrong_clear, acc.mean <1)
wrong_clear$subj_trial <- paste(wrong_clear$subject_id, wrong_clear$trial_id)
children$subj_trial <- paste(children$subject_id,children$trial_id)
children <- children[!children$subj_trial %in% wrong_clear$subj_trial,]

ggplot(children, aes(x = trial_number, y = acc, color = test_lang))+geom_smooth(method = "glm", method.args = list(family = "binomial"))+facet_wrap(~clarity,ncol = 2) + ylim(c(0,1.05))

ggplot(children, aes(x = block, y = acc, color = test_lang))+facet_wrap(~age+clarity,ncol = 2) + ylim(c(0,1))+stat_summary_bin(fun.data = mean_cl_boot)

summary(glmer(acc ~ scale(as.numeric(as.character(age_weeks))) * test_lang + (1|subject_id) +(1 + test_lang||item), 
              data = subset(children, 
                            clarity == "distorted" & block == "Test" ), 
              family = "binomial"))


# Check if this is explained by lexical knowledge
# Import AoA from Wordbank and see if it correlates with accuracy
library(wordbankr)
wordbank_stats <- get_item_data(c("English"),"WS") %>%
     filter(definition %in% unique(children$cdi_item))
english_ws_admins <- get_administration_data("English", "WS")

wordbank_items <- get_instrument_data(instrument_language = c("English"),
                                         instrument_form = "WS",
                                         items = unique(wordbank_stats$item_id),
                                         administrations = english_ws_admins)

wordbank_data <- wordbank_items %>%
  dplyr::mutate(produces = value == "produces") %>%
  dplyr::group_by(age, data_id,num_item_id) %>%
  dplyr::summarise(num_produce = sum(produces, na.rm = TRUE)) %>%
  dplyr::group_by(age,num_item_id) %>%
  dplyr::summarise(mean_num_produce = mean(num_produce, na.rm = TRUE))

wordbank_stats <- merge(wordbank_stats,subset(wordbank_data, age == "24"), by = "num_item_id")

children <- merge(children, wordbank_stats[,c("definition","mean_num_produce")], by.x = "cdi_item",by.y = "definition", all = TRUE)

summary(glmer(acc ~ mean_num_produce + (1+mean_num_produce|subject_id) + (1|item), data = subset(children, clarity == "distorted"), family = "binomial"))
