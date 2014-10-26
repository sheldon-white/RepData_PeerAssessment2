# RepData Project 2: Damage and Health Effects of Weather Events

******
# Synopsis
******

It's well known that severe weather events can have a major impact on municipal well-being; if a tornado touches down in your city, you're screwed. This report extracts and presents the primary weather types contributing to agricultural, physical and health damage in the US.

This source data was a dump of NOAA storm events database (http://www.ncdc.noaa.gov/stormevents/) from 1950 to 2011. Data processing was assisted by use of a reference document: "￼NATIONAL WEATHER SERVICE INSTRUCTION 10-1605", included here as the file "repdata-peer2_doc-pd01016005curr.pdf". This documents groups all weather events into a set of 48 categories, but many of the EVTYPE records in the database did not match this set.

Much of the work preparing this report was creating a set of patterns that could be applied to coerce all records into these 48 categories. After assignment to events types, the data corresponding to events with significant impact (here defined as contributing greater than two percent to the total damage value) are extracted and plotted by event type.

All supporting data files, documents and source code can be found at: https://github.com/sheldon-white/RepData_PeerAssessment2

******
# Challenges and Assumptions

* **Unclear data description:** There isn't a "proper" databook supplied for this dataset. The included PDF file provides general descriptions of the data but in the end it was necessary to make assumptions in interpreting the data. 
* **Incomplete records:** There is very little data before 1993; before 1980 only tornado events are "complete" by the other criteria listed here. These older records were ultimately discarded because their presence would greatly distort the damage totals towards tornadoes. The report's remaining data document almost 20 years of the recent past, which is good enough for its purposes.
* **Damage exponents:** These columns (PROPDMGEXP and CROPDMGEXP) represent multipliers for the dollar amounts represented by the PROPDMG (property damage) and CROPDMG (crop damage) columns. In the end, only "H", "K", "M", "B" were used because their meaning was fairly clear (after internet research) and seemed to be consistent with a sampling of the REMARKS column descriptions. The rows containing numeric PROPDMGEXP and CROPDMGEXP values were ultimately rejected, because inspection of their REMARKS values showed many descriptions that had no relation to the calculated damage values. For instance, damage exponents of 12 or 14 (which imply catastrophic dollar amounts) could not be reconciled with the corresponding event descriptions.
* **Irregular EVTYPE values:** EVTYPE values not matching the 48 official categories were mapped to these categories through inspection, trial and error, and creation of pattern-matching rules. The set of patterns listed here results in the assignment of %99.97 of the data to these 48 values.
* **Inflation:** Damage values were inflation-adjusted to 2011 values using conversion factors extracted from data compiled at the Oregon State Political Science department: http://oregonstate.edu/cla/polisci/sites/default/files/faculty-research/sahr/inflation-conversion/excel/infcf17742014.xls. Values A(210) - A(271) and T(210) - T(271) were extracted from this Excel file to create inflationFactors.csv. These numbers represent the number of old dollars equivalent to a 2011 dollar. (For example: 0.107 1950 dollars == one 2011 dollar.) The reported weather damage values are divided by these conversion values to obtain the equivalent 2011 damage values.

******
# Data Processing

Load the raw storm data

```r
library(stringr)
library(plyr)
library(reshape)
```

```
## 
## Attaching package: 'reshape'
## 
## The following objects are masked from 'package:plyr':
## 
##     rename, round_any
```

```r
library(ggplot2)
stormData = read.csv(bzfile("repdata-data-StormData.csv.bz2"))
```
Only retain the rows that contain damage, injuries or fatalities; damage exponents must be in the accepted set. We can also discard most of the columns, but we'll also need the 'year' value

```r
damageData = stormData[,c("FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "BGN_DATE", "EVTYPE")]
damageData = damageData[damageData$INJURIES > 0 |
                        damageData$FATALITIES > 0 |
                        damageData$PROPDMGEXP %in% c("H", "M", "K", "B") |
                        damageData$CROPDMGEXP %in% c("H", "M", "K", "B"),]
damageData$year = strptime(damageData$BGN_DATE, "%m/%d/%Y %H:%M:%S")$year + 1900
class(damageData$year) = "integer"
```
Load the standard set of event types, convert to patterns for exact matching. For convenience, all patterns are matched using regular expressions in a single pass.

```r
evTypes = read.csv("eventtypes.csv", header = FALSE)
evTypes$V1 = str_trim(tolower(evTypes$V1))
evTypes$evtype = evTypes$V1
colnames(evTypes) = c("pattern", "evtype")
evTypes$pattern = paste("^", evTypes$pattern, "$", sep="")
```

Add the additional matching regular expressions. These are pairs of (pattern, standard-event) strings.

```r
rules = c(  
    "(typhoon|hurricane)", "hurricane (typhoon)",
    "(wall cloud|micoburst|microburst|downburst|turbulence|thunder|tstm)", "thunderstorm wind",
    "chill", "extreme cold/wind chill",  
    "wind", "high wind",   
    "fog and cold temperatures", "freezing fog",
    "fog", "dense fog",
    "gustnado", "high wind",
    "(tornado|torndao)", "tornado",
    "(ligntning|lighting)", "lightning",
    "(heavy|hvy|excessive|torrential|record).+(rain|rainfall|rainstorm|precipatation|precipitation)", "heavy rain",
    "(shower|rainstorm|rain damage|prolonged rain)", "heavy rain",
    "mixed precipitation", "heavy rain",   
    "(heavy|hvy|excessive|torrential|record).+snow", "heavy snow",
    "snow", "winter weather",
    "hail", "hail",
    "(sleet|freezing)", "sleet",
    "(swell|surf)", "high surf",
    "rip currents", "rip currents",
    "spout", "waterspout",
    "(coastal storm|surge)", "storm surge/tide",
    "(extreme|severe|heavy|hvy|excessive|torrential|record|unusual).+cold", "extreme cold/wind chill",
    "hypothermia", "extreme cold/wind chill",
    "cold", "cold/wind chill",
    "low temperature", "cold/wind chill",
    "(frost|freeze)", "frost/freeze",
    "(tide)", "storm surge/tide",
    "(wave| seas)", "high surf",
    "fire", "wildfire",
    "wint", "winter weather",
    "extreme heat", "heat",
    "(hot|warm)", "heat",
    "(slide|mud|landslump)", "debris flow",
    "avalance", "avalanche",
    "flood", "flash flood",
    "(high water|fld|rapidly rising water)", "flood",
    "dust", "dust storm",
    "tropical storm", "tropical storm",
    "(ice|icy|glaze)", "frost/freeze"
)

ruleset = data.frame(matrix(rules, ncol=2, byrow=TRUE))
colnames(ruleset) = c("pattern", "evtype")
allRules = rbind(evTypes, ruleset)
```

Then assign all possible records into the official categories.

```r
damageData$normalizedEvtype = str_trim(tolower(damageData$EVTYPE))
damageData$event = NA
a = apply(allRules, 1, function(row) {
    pattern = row[1]   
    evtype = row[2]
    matches = grepl(pattern, damageData$normalizedEvtype) & is.na(damageData$event)  
    damageData[matches, "event"] <<- evtype
})
```
Some samples are taken to see how the event types are reported in the past.
"Complete" events reported in or before 1980:

```r
paste(unique(damageData$event[damageData$year <= 1980 & !is.na(damageData$event)]), collapse = ",")
```

```
## [1] "tornado"
```
"Complete" events reported in or before 1980:

```r
paste(unique(damageData$event[damageData$year <= 1990 & !is.na(damageData$event)]), collapse = ",")
```

```
## [1] "tornado,thunderstorm wind,hail"
```
"Complete" events reported in or before 1980:

```r
paste(unique(damageData$event[damageData$year <= 1993 & !is.na(damageData$event)]), collapse = ",")
```

```
## [1] "tornado,thunderstorm wind,hail,winter storm,lightning,high wind,high surf,coastal flood,heavy rain,flash flood,flood,heavy snow,ice storm,wildfire,winter weather,debris flow,dense fog,blizzard,cold/wind chill,extreme cold/wind chill,waterspout,storm surge/tide,hurricane (typhoon),avalanche,sleet,frost/freeze,heat,dust devil,drought,funnel cloud,dust storm,tropical storm"
```
So we don't have a good representation of event types in the older records cleaned and filtered dataset. Tornadoes and thunderstorms could be heavily overrepresented in the totals, especially taking inflation into account. **We continue processing with records from 1993 onward.**

```r
damageData = damageData[damageData$year >= 1993,]
```
Now load the inflation adjustment factors; add inflation data to the damageData table.

```r
inflationFactors = read.csv("inflationFactors.csv", header = FALSE)
colnames(inflationFactors) = c("year", "factor")
class(inflationFactors$year) = "integer"

inflation = function(y) {
    inflationFactors$factor[inflationFactors$year == y]
}

damageData$inflationFactor = as.numeric(lapply(damageData$year, inflation))
```
Now convert the damage cost records into a usable form, scaling them with the damage exponents and the inflation factor.

```r
damageData$PROPDMGEXP = toupper(damageData$PROPDMGEXP)
damageData$CROPDMGEXP = toupper(damageData$CROPDMGEXP)

damageData$propMultiplier = 1
damageData$propMultiplier[damageData$PROPDMGEXP == "H"] = 100
damageData$propMultiplier[damageData$PROPDMGEXP == "K"] = 1000
damageData$propMultiplier[damageData$PROPDMGEXP == "M"] = 1000000
damageData$propMultiplier[damageData$PROPDMGEXP == "B"] = 1000000000
damageData$cropMultiplier = 1
damageData$cropMultiplier[damageData$CROPDMGEXP == "H"] = 100
damageData$cropMultiplier[damageData$CROPDMGEXP == "K"] = 1000
damageData$cropMultiplier[damageData$CROPDMGEXP == "M"] = 1000000
damageData$cropMultiplier[damageData$CROPDMGEXP == "B"] = 1000000000

damageData$propDmgDollars = damageData$propMultiplier * damageData$PROPDMG / damageData$inflationFactor
damageData$cropDmgDollars = damageData$cropMultiplier * damageData$CROPDMG / damageData$inflationFactor
```
Extract the minimum set of columns for data visualization, reshape the data into a final format. We separate damage values from health values at this point.

```r
damage = damageData[!is.na(damageData$event), c("year", "FATALITIES", "INJURIES", "propDmgDollars", "cropDmgDollars", "event")]
damage$event = factor(damage$event)

totals = ddply(damage, .(event), summarize,
               injuries = sum(INJURIES),
               fatalities = sum(FATALITIES),
               cropDamage = sum(cropDmgDollars),
               propDamage = sum(propDmgDollars))
```
We calculate the relative contributions of each event type to it's category (needed a little later).

```r
totals$propDamagePct = 100 * totals$propDamage / sum(totals$propDamage)
totals$cropDamagePct = 100 * totals$cropDamage / sum(totals$cropDamage)
totals$injuriesPct = 100 * totals$injuries / sum(totals$injuries)
totals$fatalitiesPct = 100 * totals$fatalities / sum(totals$fatalities)
totals = ddply(damage, .(event), summarize,
               injuries = sum(INJURIES),
               fatalities = sum(FATALITIES),
               cropDamage = sum(cropDmgDollars),
               propDamage = sum(propDmgDollars))
totals$propDamagePct = 100 * totals$propDamage / sum(totals$propDamage)
totals$cropDamagePct = 100 * totals$cropDamage / sum(totals$cropDamage)
totals$injuriesPct = 100 * totals$injuries / sum(totals$injuries)
totals$fatalitiesPct = 100 * totals$fatalities / sum(totals$fatalities)

damageEvents = totals[,c("event","propDamage", "propDamagePct", "cropDamage", "cropDamagePct")]
healthEvents = totals[,c("event","injuries", "injuriesPct", "fatalities", "fatalitiesPct")]
```
Gather some final statistics about the data processing.

```r
uncategorized = damageData[is.na(damageData$event),]
message("uncategorized row count: ", nrow(uncategorized))
```

```
## uncategorized row count: 95
```

```r
message("uncategorized damage: ", sum(uncategorized$propDmgDollars) + sum(uncategorized$cropDmgDollars))
```

```
## uncategorized damage: 246438232.472558
```

```r
message("categorized damage: ", sum(damage$propDmgDollars) + sum(damage$cropDmgDollars))
```

```
## categorized damage: 533869672318.179
```
For the first two plots we only retain events that are a significant contributor (> %2) to the damage and health totals. 

```r
damageEvents = totals[,c("event","propDamage", "propDamagePct", "cropDamage", "cropDamagePct")]
healthEvents = totals[,c("event","injuries", "injuriesPct", "fatalities", "fatalitiesPct")]
worstDamageEvents = damageEvents[damageEvents$propDamagePct > 2 | damageEvents$cropDamagePct > 2, c("event","propDamage", "cropDamage")]
colnames(worstDamageEvents)[2:3] = c("Property Damage", "Crop Damage")
worstHealthEvents = healthEvents[healthEvents$injuriesPct > 2 | healthEvents$fatalitiesPct > 2, c("event","injuries", "fatalities")]
colnames(worstHealthEvents)[2:3] = c("Injuries", "Fatalities")

damageCollated = melt(worstDamageEvents, id=c("event"))
healthCollated = melt(worstHealthEvents, id=c("event"))
colnames(damageCollated)[2] = "category"
colnames(healthCollated)[2] = "category"
damageCollated$value = damageCollated$value / 1000000
```

******
# Results

The first plot shows the total damage due to event types with a major impact:

```r
ggplot(damageCollated, aes(event, value)) +
    geom_bar(aes(fill = category), position = "dodge", stat = "identity") + coord_flip() +
    xlab("Event Type") + ylab("Total Damage (Millons of Dollars)") + ggtitle("Damage Totals in US (1993 - 2011)\n")
```

![plot of chunk event-damage](./RepData_PeerAssessment2_files/figure-html/event-damage.png) 

Major points revealed by this plot:

* **Flooding and hurricanes cause far more damage than other weather events.**
* Property damage values are far higher than crop damage values.

The second plot shows the injuries and fatalities due to event types with a major impact:

```r
ggplot(healthCollated, aes(event, value)) +
    geom_bar(aes(fill = category), position = "dodge", stat="identity") + coord_flip() +
    xlab("Event Type") + ylab("Number of Affected People") + ggtitle("Injury and Fatality Totals in US (1993 - 2011)\n")
```

![plot of chunk event-injury](./RepData_PeerAssessment2_files/figure-html/event-injury.png) 

Major points revealed by this plot:

* **Tornadoes are by far the biggest cause of injuries and deaths.**
* As expected, injuries far exceed fatalities.

It is interesting to see the year-by-year trends in reported damages. We can plot the annual costs due to the most destructive weather types:


```r
subset = damage[damage$event %in% c("tornado", "flood", "hurricane (typhoon)"),]
totals = ddply(subset, c("event", "year"), summarize,
               injuries = sum(INJURIES),
               fatalities = sum(FATALITIES),
               cropDamage = sum(cropDmgDollars),
               propDamage = sum(propDmgDollars))
totalsMelted = melt(totals, id=c("event", "year"))
ggplot(data=totalsMelted, aes(x=year, y = value, colour = event, group = event)) + 
    geom_line() + facet_wrap(~variable, scales = "free")
```

![plot of chunk major-events-by-year](./RepData_PeerAssessment2_files/figure-html/major-events-by-year.png) 
