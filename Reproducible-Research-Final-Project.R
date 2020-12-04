#Librarys to be used
library(dplyr)
library(ggplot2)
library(reshape2)
# check & set Working Directory
if(getwd() != "~/RWD/Reproducible-Research-Final-Project")
    setwd("~/RWD/Reproducible-Research-Final-Project")

# Download file
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!file.exists("Storm Data.bz2"))
    download.file(url,"Storm Data.bz2")

#read data
if(!exists("a")) a <- read.csv("Storm Data.bz2")

#data Understanding
dim(a)
oldsize <- format(object.size(a), "MB")
names(a)
str(a$BGN_DATE)

#fixing the data (Cleaning and pre-processing)

# 1- dataset dimensionality reduction
b <- a %>% select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
    filter(as.Date(BGN_DATE, "%m/%d/%Y %H:%M:%S") > "1996-01-01")

c <- b %>% mutate(PROPDMGEXP2 = case_when(PROPDMGEXP == "B" ~ 100000000,
                                          PROPDMGEXP == "M" ~ 1000000,
                                          PROPDMGEXP == "K" ~ 1000,
                                          TRUE ~ 1)) %>%
    mutate(CROPDMGEXP2 = case_when(CROPDMGEXP == "B" ~ 100000000,
                                   CROPDMGEXP == "M" ~ 1000000,
                                   CROPDMGEXP == "K" ~ 1000,
                                   TRUE ~ 1)) %>%
    mutate(PROPDMG2 = PROPDMG* PROPDMGEXP2) %>% 
    mutate(CROPDMG2 = CROPDMG* CROPDMGEXP2)


newsize <- format(object.size(b), "MB")
format(object.size(a) - object.size(b), "MB")
unclass(object.size(b)/object.size(a)*100)


#Analysis
# 1- Across the United States, which types of events are most harmful with respect to population health?
aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, b, sum) %>%
    mutate(TOTAL = FATALITIES + INJURIES) %>%
    top_n(5, TOTAL) %>% 
    melt(id = "EVTYPE", measure.vars = c("FATALITIES", "INJURIES", "TOTAL")) %>%
    ggplot(aes(value, reorder(x = EVTYPE, X = (value)),  fill=EVTYPE)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_grid(variable~.) +
    xlab(label = "Death / Injury Count") +
    ylab(label = "") +    
    theme_light()

# 2- Across the United States, which types of events have the greatest economic consequences?
aggregate(cbind(PROPDMG2, CROPDMG2) ~ EVTYPE, c, sum) %>%
    mutate(TOTAL = PROPDMG2 + CROPDMG2) %>%
    top_n(5, TOTAL) %>% 
    melt(id = "EVTYPE", measure.vars = c("PROPDMG2", "CROPDMG2", "TOTAL")) %>%
    ggplot(aes(value, reorder(x = EVTYPE, X = (value)),  fill=EVTYPE)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_grid(variable~.) +
    xlab(label = "Property / Crop Damage Value") +
    ylab(label = "") +    
    theme_light()