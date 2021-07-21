rm(list=ls())
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
###################read the 21 files 
setwd("/Users/mihaela/Dropbox/Legoworld/Reports/Analyses")
mydir= "Merged_BORIS_PUPIL_data"
myfiles=list.files(path = mydir, pattern = "*csv", full.names = TRUE)
myfiles
#Build the master file
dat_csv = ldply(myfiles, read.csv)
dat_csv
colnames(dat_csv)
#delete some of the unused columns, change a column name
dat_csv[,c(1:18)]<-list(NULL)
dat_csv[,c('Part') ] <-list(NULL)
tmp<-na.omit(dat_csv)

###############Factor the categorical variables############################ 
tmp <- as.data.frame(tmp)
tmp$Participant <- as.factor(tmp$Participant)
tmp$Build <- as.factor(tmp$Build)
tmp$Behavior_boris <- as.factor(tmp$Behavior_boris)
#tmp$Behavioral.category <- as.character(tmp$Behavioral.category)
tmp$Behavioral.category <- as.factor(tmp$Behavioral.category)

#Time as numeric (time is expressed in seconds)
as.numeric(tmp$Time_boris)
as.numeric(tmp$Time_N)

#### specify levels 
levels(tmp$Part) <- c('Part4', 'Part5', 'Part12','Part16', 'Part18', 'Part19', 'Part24')
levels(tmp$Build) <- c('Build1', 'Build2',"Build3")
levels(tmp$Behavioral.category) <- c("1transparent", "2DarkRed", "3Orange","4Red","4YellowRound","Accidents", "Base", "BrickClick","Configuration","Remove", "Minibase")

#######Recode Behavior into Type: look, use, other, by looking at the first two characters of Behavior
library(tidyverse)
tmp <- tmp %>%  
  mutate(first_two = str_extract(tmp$Behavior_boris, ".."), 
         Type = case_when( 
           first_two == "l_" ~ "look", 
           first_two == "u_" ~ "use", 
           TRUE ~ "other"
         ))  

levels(tmp$Type) <- c('look', 'use',"other")

#get the looks at 
tmp2<-tmp %>% filter(Type != 'use' & Type != 'other') %>% 
  filter(Status != "STOP")

# get the first row of each behav_category to analyse the first looks times for each brick
tmp3<-tmp %>% filter(Type != 'use' & Type != 'other') %>% 
  filter(Status != "STOP" & Time_N == '0')
###############FIRST TIME LOOKED AT, what bricks? Normalize by the number of bricks in each category

#Get the first looked at for each part, build, behav category
FirstLU<-tmp3 %>%  
  group_by(Part, Build, Behavioral.category) %>%  
  arrange(Time_boris, Build, Part) %>% 
  slice(1)  # pick the first one 

#factors in FirstLU
FirstLU<-data.frame(FirstLU)
FirstLU$Time_N <- as.numeric(FirstLU$Time_N)
FirstLU$Part <- as.factor(FirstLU$Part)
FirstLU$Build <- as.factor(FirstLU$Build)
FirstLU$Type <- as.factor(FirstLU$Type)

#Filter only the first looked at bricks where the value 0 (the starting time, normalized)
FirstLU.0<-FirstLU %>% filter(Time_N == '0')
# Get frequency data for which bricks were seen firstly

library(plyr)
summary<-FirstLU.0 %>% dplyr:::count(Behavioral.category, Build)
as.data.frame(summary)
colnames(summary) <- c("Behavioral.category", "Build","Frequency")

#normalize the data by how many bricks it has.

summary$NrBrick<-ifelse(summary$Behavioral.category== '1transparent', "1",
                        ifelse(summary$Behavioral.category=='2DarkRed', "2",
                               ifelse(summary$Behavioral.category=="3Orange", "3",
                                      ifelse(summary$Behavioral.category=="4Red", "4",
                                             ifelse(summary$Behavioral.category=="4YellowRound","4",
                                                    NA)))))
summary$NrTotal_N<- as.numeric(summary$Frequency) / as.numeric(summary$NrBrick)
#plot the data for the frequency of first looked
library(ggplot2)
plot1<-ggplot(data = summary)+
  geom_bar(mapping= aes(x = Behavioral.category, y =NrTotal_N, fill = Behavioral.category), stat = "identity", position = "dodge") + facet_grid(~Build)
plot1+theme(legend.position="right", axis.text.x = element_blank(), panel.background = element_blank()) + scale_y_continuous(name="First time looked at: How many times?")+ scale_x_discrete(name ="Category of bricks")+ theme(panel.spacing = unit(3, "lines")) + labs(fill = "Brick Category")


##############################EYE TRACKING MEASURES OVER ALL PARTICIPANTS################

#############################Duration first fixation###################################
D1stF<-tmp2 %>%  
  group_by(Part, Build, Behavioral.category) %>%  
  arrange(Time_boris, Build, Part) %>% 
  slice(1)  # pick the first one 
library(dplyr)
D1stF.s<-D1stF %>% group_by(Build, Behavioral.category) %>% 
  summarize(avg = mean(Duration_of_1st_Fixation_in_look),
            sd = sd(Duration_of_1st_Fixation_in_look),
            n = n(),  
            se = sd/sqrt(n()))

D1stF.s<-D1stF.s %>% filter(Behavioral.category != "Remove")
D1stF.s<-D1stF.s %>% filter(Behavioral.category != "Base" & Behavioral.category != "Configuration")

# plot Duration of first fixation
library(ggplot2)
dur.first.fixation<-ggplot(data = D1stF.s)+
  geom_bar(mapping= aes(x = Behavioral.category, y=avg, fill = Behavioral.category), stat = "identity", position = "dodge") + facet_grid(~Build) + geom_errorbar(aes(x= Behavioral.category, ymin=avg-se , ymax=avg+se ),width=.2, position=position_dodge(.9))
dur.first.fixation+theme(legend.position="right", axis.text.x = element_blank(), panel.background = element_blank()) + scale_y_continuous(name="First fixation duration")+scale_x_discrete(name ="Category of bricks") + labs(fill = "Brick Category")+ theme(panel.spacing = unit(3, "lines"))

#########calculate frequencies for MOST USED BRICKS!

Dt<-tmp %>% filter(Status != 'STOP' & Type != "look") %>% 
  filter(Type != "other" & Behavioral.category != "Base" & Behavioral.category != "Configuration") 

MostUsed<-Dt %>%
  group_by(Behavioral.category, Build, Participant) %>%
  summarise(NrTotal= n())
as.data.frame(MostUsed)
MostUsed<-MostUsed %>% filter(Behavioral.category != "Remove")

#create an empty, delete a column. MostUsed$NrBricks<-NA/ NULL

MostUsed$NrBrick<-ifelse(MostUsed$Behavioral.category== '1transparent', "1",
                         ifelse(MostUsed$Behavioral.category=='2DarkRed', "2",
                                ifelse(MostUsed$Behavioral.category=="3Orange", "3",
                                       ifelse(MostUsed$Behavioral.category=="4Red", "4",
                                              ifelse(MostUsed$Behavioral.category=="4YellowRound", "4",
                                                     NA)))))
require(dplyr)
MostUsed$NrTotal_N<- as.numeric(MostUsed$NrTotal) / as.numeric(MostUsed$NrBrick)
#MostUsed$NrTotal_N<-MostUsed$NrTotal_N/14


#install.packages("doBy")
library(doBy)
MostUsed.s<-summaryBy(NrTotal_N~Build*Behavioral.category, data = MostUsed, 
                      FUN = function(x) { c(m = mean(x), s = sd(x), sem= sd(x)/sqrt(sum(!is.na(x)))) } )
library(ggplot2)
plot.used <-ggplot(MostUsed.s, aes(x = Behavioral.category, y = NrTotal_N.m, color = Behavioral.category)) + geom_boxplot(size=0.4, notch = FALSE) + theme(legend.position = "right", panel.grid = element_blank(),axis.text.x = element_blank()) + ggtitle("Most Used Bricks")+ scale_fill_brewer(palette = "Set1") + facet_wrap(~Build)+guides(color=guide_legend("Brick Category"))
plot.used+geom_errorbar(aes(ymin=NrTotal_N.m-NrTotal_N.sem, ymax=NrTotal_N.m+NrTotal_N.sem),width=.2, position=position_dodge(.9)) + 
  geom_text(aes(label= round(NrTotal_N.m,2)),position=position_dodge(width=0.9), vjust=-0.25)+scale_y_continuous(name="Normalized use", limits=c(0, 4))+scale_x_discrete(name ="Category of bricks") 


#calculate frequencies for most LOOKED bricks!
#################################################################
################################################################
#calculate frequencies for most LOOKED bricks!
MostLooked<-tmp2 %>% 
  group_by(Behavioral.category, Build, Participant) %>% 
  summarise(NrTotal= n())
MostLooked<-MostLooked %>% filter(Behavioral.category != "Remove" & Behavioral.category != "Base" & Behavioral.category != "Configuration") 

#create an empty, delete a column. MostLooked$NrBricks<-NA/ NULL

MostLooked$NrBrick<-ifelse(MostLooked$Behavioral.category== '1transparent', "1",
                           ifelse(MostLooked$Behavioral.category=='2DarkRed', "2",
                                  ifelse(MostLooked$Behavioral.category=="3Orange", "3",
                                         ifelse(MostLooked$Behavioral.category=="4Red", "4",
                                                ifelse(MostLooked$Behavioral.category=="4YellowRound", "4",
                                                       NA)))))
require(dplyr)
MostLooked$NrTotal_N<- as.numeric(MostLooked$NrTotal) / as.numeric(MostLooked$NrBrick)
#MostLooked$NrTotal_N<-MostLooked$NrTotal_N/14


#install.packages("doBy")
library(doBy)
MostLooked.s<-summaryBy(NrTotal_N~Build*Behavioral.category, data = MostLooked, 
                        FUN = function(x) { c(m = mean(x), s = sd(x), sem= sd(x)/sqrt(sum(!is.na(x)))) } )
library(ggplot2)
plot.looked <-ggplot(MostLooked.s, aes(x = Behavioral.category, y = NrTotal_N.m, color = Behavioral.category)) + geom_boxplot(size=0.4, notch = FALSE) + theme(legend.position = "right", panel.grid = element_blank(),axis.text.x = element_blank()) + ggtitle("Most LOOKED at Bricks")+ scale_fill_brewer(palette = "Set1") + facet_wrap(~Build)+guides(color=guide_legend("Brick Category"))
plot.looked+geom_errorbar(aes(ymin=NrTotal_N.m-NrTotal_N.sem, ymax=NrTotal_N.m+NrTotal_N.sem),width=.2, position=position_dodge(.9)) + 
  geom_text(aes(label= round(NrTotal_N.m,2)),position=position_dodge(width=0.9), vjust=-0.25)+scale_y_continuous(name="Normalized use", limits=c(0, 7))+scale_x_discrete(name ="Category of bricks")


################NUMBER OF FIXATIONS#########################################################
#######this measure hasn't been used because it is considered unreliable
library(dplyr)
library(tidyverse)
tmp2<-as.data.frame(tmp2)
fix.nr<-tmp2 %>% group_by(Participant, Behavioral.category, Build) %>% summarize(Fixations_per_look = sum(Fixations_per_look))
fix.nr.s<-fix.nr %>% group_by(Behavioral.category, Build) %>% summarize(Fixations_per_look = mean(Fixations_per_look))
fix.nr.s<-fix.nr.s %>% filter(Behavioral.category != "Base" & Behavioral.category != "Configuration")
fix.nr.s<-fix.nr.s %>% filter(Behavioral.category != "Remove")
# plot total number of fixations
library(ggplot2)
plot5<-ggplot(data = fix.nr.s)+
  geom_bar(mapping= aes(x = Behavioral.category, y=Fixations_per_look, fill = Behavioral.category), stat = "identity", position = "dodge") + facet_grid(~Build)
plot5 + theme(legend.position="right", axis.text.x = element_blank(), panel.background = element_blank()) + scale_y_continuous(name="Total number of fixations")+ scale_x_discrete(name ="Category of bricks") 

##########################Time to first fixation#############################
#############################this one is also not a good measure##########
look.duration<-tmp2 %>% group_by(Participant, Behavioral.category, Build) %>% summarize(Look_duration = sum(Look_duration))
look.duration.s<-look.duration %>% group_by(Behavioral.category, Build) %>% summarize(Look_duration = mean(Look_duration))
look.duration.s<-look.duration.s %>% filter(Behavioral.category != "Remove")
look.duration.s<-look.duration.s %>% filter(Behavioral.category != "Base" & Behavioral.category != "Configuration")
# plot LOOK DURATION 
library(ggplot2)
plot6<-ggplot(data = look.duration.s)+
  geom_bar(mapping= aes(x = Behavioral.category, y=Look_duration, fill = Behavioral.category), stat = "identity", position = "dodge") + facet_grid(~Build)
plot6+theme(legend.position="right", axis.text.x = element_blank(), panel.background = element_blank())+ scale_y_continuous(name="Look duration/Total fixation time (ms)")+scale_x_discrete(name ="Category of bricks")

#differences between experts and children's ratings of creativity.
#nrTotal.Adj = c(29, 33, 29.66, 25.75, 29.5, 33, 46, 4)
#MostUsed<-cbind(MostUsed, nrTotal.Adj)

setwd("/Users/mihaela/Dropbox/Legoworld")
ratings<-read_csv('ExpertVsChildren.csv')
as.data.frame(ratings)

ratings$Build<-as.factor(ratings$Build)
ratings$Frequency<-as.numeric(ratings$Frequency)
ratings$Expert_vs_Children<-as.factor(ratings$Expert_vs_Children)
levels(ratings$Expert_vs_Children) <- c('Expert raters', 'Children')

plot.ratings<-ggplot(data = ratings)+
  geom_bar(mapping= aes(x = Build, y=Frequency, fill = Build), stat = "identity", position = "dodge") +facet_grid(~Expert_vs_Children)
plot.ratings+theme(legend.position="right", axis.text.x = element_blank(), panel.background = element_blank()) #+ scale_x_discrete(name ='Builds') + scale_y_discrete(name = 'Frequency')
