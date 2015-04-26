# RepData_PeerAssessment2

library(dplyr)
data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))

## reduce the data set
# select only relevant rows & columns
d <- data %>% select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>% filter(FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)
# normalize EVTYPE, PROPDMGEXP and CROPDMGEXP and group by EVTYPE
d <- d %>% mutate(EVTYPE = as.factor(toupper(as.character(EVTYPE))), PROPDMGEXP = as.factor(toupper(as.character(PROPDMGEXP))), CROPDMGEXP = as.factor(toupper(as.character(CROPDMGEXP)))) %>% group_by(EVTYPE)

# calculate damages
propdmgexp <- data.frame(PROPDMGEXP=c("", "K", "M", "B"), propexp=c(1, 1e3, 1e6, 1e9))
cropdmgexp <- data.frame(CROPDMGEXP=c("", "K", "M", "B"), cropexp=c(1, 1e3, 1e6, 1e9))
d <- d %>% inner_join(propdmgexp) %>% inner_join(cropdmgexp) %>% mutate(TOTALDMG = PROPDMG*propexp + CROPDMG*cropexp)

# find events causing most damages
dmg <- summarize(d, damages=sum(TOTALDMG))
dmg <- head(arrange(dmg, desc(damages)))

barplot(dmg$damages/1e9, xlab = "Events", ylab = "Damage (in Bn US$)", legend.text = dmg$EVTYPE, col=c("red","orange","yellow","green","blue","white"), main="Events with Highest Damages")

# find events causing most fatalities
fat <- summarize(d, fatalities=sum(FATALITIES))
fat <- head(arrange(fat, desc(fatalities)))

barplot(fat$fatalities, xlab = "Events", ylab = "Fatalities", legend.text = fat$EVTYPE, col=c("red","orange","yellow","green","blue","white"), main="Events with Highest Fatalities")
