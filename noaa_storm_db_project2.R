# Document Layout
# Language: Your document should be written in English.
# Title: Your document should have a title that briefly summarizes your data analysis
# Synopsis: Immediately after the title, there should be a synopsis which describes and summarizes your analysis 
#           in at most 10 complete sentences.
#
# There should be a section titled Data Processing which describes (in words and code) how the data were loaded 
# into R and processed for analysis. In particular, your analysis must start from the raw CSV file containing the 
# data. You cannot do any preprocessing outside the document. If preprocessing is time-consuming you may consider 
# using the cache = TRUE option for certain code chunks.
#
# There should be a section titled Results in which your results are presented.
# You may have other sections in your analysis, but Data Processing and Results are required.
# The analysis document must have at least one figure containing a plot.
# 
# Your analysis must have no more than three figures. Figures may have multiple plots in them (i.e. panel plots), 
# but there cannot be more than three figures total.
# You must show all your code for the work in your analysis document. This may make the document a bit verbose, 
# but that is okay. In general, you should ensure that echo = TRUE for every code chunk (this is the default 
# setting in knitr).


# Your data analysis must address the following questions:
#  
# Q1) Across the United States, which types of events (as indicated in the EVTYPE variable) 
#     are most harmful with respect to population health?
# 
# Q2) Across the United States, which types of events have the greatest economic consequences?
#
# Consider writing your report as if it were to be read by a government or municipal manager who might be 
# responsible for preparing for severe weather events and will need to prioritize resources for different types 
# of events. However, there is no need to make any specific recommendations in your report.

# NOTE: Load dplyr to manipulate data. Additional information can be found here:
# https://dplyr.tidyverse.org/
library(plyr)
library(dplyr)

# NOTE: load ggplot2 to create the charts. Additinal information can be found here:
# https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
library(ggplot2)

# Data Processing
# NOTE: The original NOAA DB file can be found here:
# https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
noaa_db_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(noaa_db_url, dest = "StormData.csv.bz2")

# NOTE: additional info on read.table:
# https://stat.ethz.ch/R-manual/R-devel/library/utils/html/read.table.html 
noaa_table <- read.table("StormData.csv.bz2", 
                         sep = ",", 
                         header = TRUE, 
                         quote = "\"'", 
                         dec = ".",
                         numerals = c("allow.loss", 
                                      "warn.loss", 
                                      "no.loss"))

# Find the names of the columns in the dataset
names(noaa_table)

# We don't need all columns. Pick the necessary columns for further analysis
accidents <- noaa_table[,c(8,23:24)] # include EVTYPE, FATALITIES, INJURIES
property_damages <- noaa_table[,c(8,25:28)] # include EVTYPE, PROPFMG, PROPDMGEXP, CROPDMG, CROPDMGEXP

#NOTE: used only for debugging, too much information otherwise
print(accidents)
print(property_damages)

# Identify the highest incidence of events, sorted by fatalities and injuries that did not result in a fatality
top_display_events = 10
top.accidents <- aggregate(cbind(FATALITIES,INJURIES) ~ EVTYPE, 
                           data = accidents, 
                           sum, 
                           na.rm=TRUE)

top.accidents <- arrange(top.accidents, 
                         desc(FATALITIES + INJURIES))

# pick the top 15 event types since after 15 the injuries and fatalities go drastically down
top.accidents <- top.accidents[1:top_display_events,]
top.accidents

# Results
# Q1) Across the United States, which types of events (as indicated in the EVTYPE variable) 
#     are most harmful with respect to population health?
fatalities_chart = ddply(top.accidents, 
                         .(EVTYPE), 
                         summarize, 
                         sum_fatalities = sum(FATALITIES,na.rm=TRUE))

fatalities_chart = fatalities_chart[order(fatalities_chart$sum_fatalities, decreasing = TRUE), ]
head(fatalities_chart,top_display_events)

ggplot(fatalities_chart[1:top_display_events, ], 
       aes(EVTYPE,
           x = reorder(top.accidents$EVTYPE, sum_fatalities),
           y = sum_fatalities,
           fill=EVTYPE,
           alpha=0.5
          )
) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = fatalities_chart$sum_fatalities), 
            size = 5, 
            hjust = 0.5, 
            vjust = 1, 
            position = "stack") +
  ggtitle("Events = Fatalities") +
  guides(color = "none") +
  coord_flip() +
  xlab("") + 
  ylab("") +
  scale_fill_grey() + 
  theme_classic()

ggsave("fatalities-1.png")
dev.off()

injuries_chart = ddply(top.accidents, 
                       .(EVTYPE), 
                       summarize, 
                       sum_injuries = sum(INJURIES,na.rm=TRUE))
injuries_chart = injuries_chart[order(injuries_chart$sum_injuries, decreasing = TRUE), ]
head(injuries_chart,top_display_events)

ggplot(injuries_chart[1:top_display_events, ], 
       aes(EVTYPE,
           x = reorder(top.accidents$EVTYPE, sum_injuries),
           y = sum_injuries,
           fill=EVTYPE,
           alpha=0.5,
           label = sum_injuries
       )
) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = fatalities_chart$sum_fatalities), 
            size = 5, 
            hjust = 0.5, 
            vjust = 1, 
            position = "stack") +
  ggtitle("Events = Injuries") +
  guides(color = "none") +
  coord_flip()+
  xlab("") + 
  ylab("") +
  scale_fill_grey() + 
  theme_classic()

ggsave("injuries-1.png")
dev.off()

# Q2) Across the United States, which types of events have the greatest economic consequences?
# Find out property damage and crop damages
top.damages <- aggregate(cbind(PROPDMG,CROPDMG) ~ EVTYPE, 
                           data = property_damages, 
                           sum, 
                           na.rm=TRUE)

top.damages <- arrange(top.damages, 
                       desc(PROPDMG + CROPDMG))

top.damages <- top.damages[1:top_display_events,]
top.damages

# Find out how to apply the multiplier for PROPDMGEXP and CROPDMGEXP
table(property_damages$PROPDMGEXP)
table(property_damages$CROPDMGEXP)

property_damages$PROPDMGCALC [property_damages$PROPDMG==0] <- 0  
property_damages$CROPDMGCALC [property_damages$CROPDMG==0] <- 0 

# property multipliers
property_damages$PROPDMGCALC [property_damages$PROPDMGEXP=="H" | 
                                property_damages$PROPDMGEXP=="h"] <- property_damages$PROPDMG[property_damages$PROPDMGEXP=="H" |
                                                                                                property_damages$PROPDMGEXP=="h"]*100
property_damages$PROPDMGCALC [property_damages$PROPDMGEXP=="K" | 
                                property_damages$PROPDMGEXP=="k"] <- property_damages$PROPDMG[property_damages$PROPDMGEXP=="K" | 
                                                                                                property_damages$PROPDMGEXP=="k"]*1000

property_damages$PROPDMGCALC [property_damages$PROPDMGEXP=="M" | 
                                property_damages$PROPDMGEXP=="m"] <- property_damages$PROPDMG[property_damages$PROPDMGEXP=="M" |
                                                                                                property_damages$PROPDMGEXP=="m"]*1000000

property_damages$PROPDMGCALC [property_damages$PROPDMGEXP=="B" | 
                                property_damages$PROPDMGEXP=="b"] <- property_damages$PROPDMG[property_damages$PROPDMGEXP=="B" | 
                                                                                                property_damages$PROPDMGEXP=="b"]*1000000000

# CROP multipliers
property_damages$CROPDMGCALC [property_damages$CROPDMGEXP=="H" | 
                                property_damages$CROPDMGEXP=="h"] <- property_damages$CROPDMG[property_damages$CROPDMGEXP=="H" | 
                                                                                                property_damages$CROPDMGEXP=="h"]*100
property_damages$CROPDMGCALC [property_damages$CROPDMGEXP=="K" | 
                                property_damages$CROPDMGEXP=="k"] <- property_damages$CROPDMG[property_damages$CROPDMGEXP=="K" | 
                                                                                                property_damages$CROPDMGEXP=="k"]*1000

property_damages$CROPDMGCALC [property_damages$CROPDMGEXP=="M" | 
                                property_damages$CROPDMGEXP=="m"] <- property_damages$CROPDMG[property_damages$CROPDMGEXP=="M" | 
                                                                                                property_damages$CROPDMGEXP=="m"]*1000000
property_damages$CROPDMGCALC [property_damages$CROPDMGEXP=="B"| 
                                property_damages$CROPDMGEXP=="b"] <- property_damages$CROPDMG[property_damages$CROPDMGEXP=="B" |
                                                                                                property_damages$CROPDMGEXP=="b"]*1000000000

# find total damage
total_damages <- aggregate(cbind(PROPDMGCALC,CROPDMGCALC) ~ EVTYPE, 
                         data = property_damages, 
                         sum, 
                         na.rm=TRUE)

total_damages <- arrange(total_damages, 
                       desc(PROPDMGCALC + CROPDMGCALC))

total_damages <- total_damages[1:top_display_events,]
total_damages

total_damages_sum = total_damages$PROPDMGCALC + total_damages$CROPDMGCALC
total_damages_sum

total_damages_table = as.data.frame(cbind(total_damages, total_damages_sum))
total_damages_table

ggplot(total_damages[1:top_display_events, ], 
       aes(EVTYPE,
           x = reorder(total_damages$EVTYPE, total_damages_sum),
           y = total_damages_sum,
           fill=EVTYPE,
           alpha=0.5,
           label = total_damages_sum
       )
) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = total_damages_sum), 
            size = 5, 
            hjust = 0.5, 
            vjust = 1, 
            position = "stack") +
  ggtitle("Total Damages = Property + Crops ($)") +
  guides(color = "none") +
  coord_flip()+
  xlab("") + 
  ylab("") +
  scale_fill_grey() + 
  theme_classic()

ggsave("damages_costs-1.png")
dev.off()
