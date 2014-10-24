# RepData Project 2: The Impact of Weather Events on Property and Crop Damage in the US


# Synopsis
******
It's well known that severe weather events can have a major impact on municipal well-being; if a tornado touches down in your city, you're screwed. This report describes the health, material and agricultural damage due to different categories of weather events.

(describe data source)
Data processing was assisted by use of a reference document: "￼NATIONAL WEATHER SERVICE INSTRUCTION 10-1605", included here as the file "repdata-peer2_doc-pd01016005curr.pdf".
(describe events types, without listing them)
(briefly describe challenges and assumptions)
(damage numbers adjusted for inflation to 2011 dollar values)
(briefly describe results?)
http://oregonstate.edu/cla/polisci/sites/default/files/faculty-research/sahr/inflation-conversion/excel/infcf17742014.xls

# Challenges
* Damage Exponents: These columns (PROPDMGEXP and CROPDMGEXP) represent multipliers for the dollar amounts represnted by the PROPDMG (property damage) and CROPDMG (crop damage) columns. In the end, only "H", "K", "M", "B" were used because their meaning was clear and seemed to be consistent with a sampling of the REMARKS column descriptions. The rows containing numeric PROPDMGEXP and CROPDMGEXP values were ultimately rejected, because inspection of their REMARKS values showed many descriptions that had no relation to the calculated damage values. Also, damage exponents of 12 or 14 (which imply catastrophic dollar amounts) could not be reconciled with the corresponding event descriptions.
* Irregular EVTYPE values: EVTYPE values not matching the 48 baseline values were mapped to the baslie set through inspection and creation of pattern-matching rules. The set of patterns listed here results in the assignment of %99.97 of the data to these 48 values.

# Data Processing
******
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
#stormData = read.csv(bzfile("repdata-data-StormData.csv.bz2"))
```
Only retain the records that contain damage, injuries or fatalities.

```r
#damageData = stormData[stormData$FATALITIES > 0 | stormData$INJURIES > 0 | stormData$PROPDMG > 0 | stormData$CROPDMG > 0,]
damageData = read.csv("meaningful.csv")
```
Load the standard set of event types, prepare for record matching.

```r
evTypes = read.csv("eventtypes.csv", header = FALSE)
evTypes$V1 = str_trim(tolower(evTypes$V1))
evTypes$evtype = evTypes$V1
colnames(evTypes) = c("pattern", "evtype")
evTypes$pattern = paste("^", evTypes$pattern, "$", sep="")
```

Add the additional matching rules.

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

Then assign all records into the 48 docuented categories.

```r
damageData$normalizedEvtype = str_trim(tolower(damageData$EVTYPE))
damageData$event = NA
apply(allRules, 1, function(row) {
    pattern = row[1]   
    evtype = row[2]
    matches = grepl(pattern, damageData$normalizedEvtype) & is.na(damageData$event)  
    damageData[matches, "event"] <<- evtype
})
```

```
##                          1                          2 
##    "astronomical low tide"                "avalanche" 
##                          3                          4 
##                 "blizzard"            "coastal flood" 
##                          5                          6 
##          "cold/wind chill"              "debris flow" 
##                          7                          8 
##                "dense fog"              "dense smoke" 
##                          9                         10 
##                  "drought"               "dust devil" 
##                         11                         12 
##               "dust storm"           "excessive heat" 
##                         13                         14 
##  "extreme cold/wind chill"              "flash flood" 
##                         15                         16 
##                    "flood"             "frost/freeze" 
##                         17                         18 
##             "funnel cloud"             "freezing fog" 
##                         19                         20 
##                     "hail"                     "heat" 
##                         21                         22 
##               "heavy rain"               "heavy snow" 
##                         23                         24 
##                "high surf"                "high wind" 
##                         25                         26 
##      "hurricane (typhoon)"                "ice storm" 
##                         27                         28 
##         "lake-effect snow"          "lakeshore flood" 
##                         29                         30 
##                "lightning"              "marine hail" 
##                         31                         32 
##         "marine high wind"       "marine strong wind" 
##                         33                         34 
## "marine thunderstorm wind"              "rip current" 
##                         35                         36 
##                   "seiche"                    "sleet" 
##                         37                         38 
##         "storm surge/tide"              "strong wind" 
##                         39                         40 
##        "thunderstorm wind"                  "tornado" 
##                         41                         42 
##      "tropical depression"           "tropical storm" 
##                         43                         44 
##                  "tsunami"             "volcanic ash" 
##                         45                         46 
##               "waterspout"                 "wildfire" 
##                         47                         48 
##             "winter storm"           "winter weather" 
##                         49                         50 
##      "hurricane (typhoon)"        "thunderstorm wind" 
##                         51                         52 
##  "extreme cold/wind chill"                "high wind" 
##                         53                         54 
##             "freezing fog"                "dense fog" 
##                         55                         56 
##                "high wind"                  "tornado" 
##                         57                         58 
##                "lightning"               "heavy rain" 
##                         59                         60 
##               "heavy rain"               "heavy rain" 
##                         61                         62 
##               "heavy snow"           "winter weather" 
##                         63                         64 
##                     "hail"                    "sleet" 
##                         65                         66 
##                "high surf"               "waterspout" 
##                         67                         68 
##         "storm surge/tide"  "extreme cold/wind chill" 
##                         69                         70 
##  "extreme cold/wind chill"          "cold/wind chill" 
##                         71                         72 
##          "cold/wind chill"             "frost/freeze" 
##                         73                         74 
##         "storm surge/tide"                "high surf" 
##                         75                         76 
##                 "wildfire"           "winter weather" 
##                         77                         78 
##                     "heat"              "debris flow" 
##                         79                         80 
##                "avalanche"              "flash flood" 
##                         81                         82 
##                    "flood"               "dust storm" 
##                         83                         84 
##           "tropical storm"             "frost/freeze"
```
Load the inflation adjustment factors; add inflation data to the damageData table.

```r
inflationFactors = read.csv("inflationFactors.csv", header = FALSE)
colnames(inflationFactors) = c("year", "factor")
class(inflationFactors$year) = "integer"

inflation = function(y) {
    inflationFactors$factor[inflationFactors$year == y]
}

damageData$year = strptime(damageData$BGN_DATE, "%m/%d/%Y %H:%M:%S")$year + 1900
class(damageData$year) = "integer"
damageData$inflationFactor = as.numeric(lapply(damageData$year, inflation))
```


```r
damageData$PROPDMGEXP = toupper(damageData$PROPDMGEXP)
damageData$CROPDMGEXP = toupper(damageData$CROPDMGEXP)

damageData = damageData[damageData$PROPDMGEXP %in% c("H", "M", "K", "B") | damageData$CROPDMGEXP %in% c("H", "M", "K", "B"),]
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


```r
damageData$propDmgDollars = damageData$propMultiplier * damageData$PROPDMG / damageData$inflationFactor
damageData$cropDmgDollars = damageData$cropMultiplier * damageData$CROPDMG / damageData$inflationFactor

uncategorized = damageData[is.na(damageData$event),]

damage = damageData[!is.na(damageData$event), c("FATALITIES", "INJURIES", "propDmgDollars", "cropDmgDollars", "event")]
damage$event = factor(damage$event)

message("uncategorized damage: ", sum(uncategorized$propDmgDollars) + sum(uncategorized$cropDmgDollars))
```

```
## uncategorized damage: 254399252.965303
```

```r
message("categorized damage: ", sum(damage$propDmgDollars) + sum(damage$cropDmgDollars))
```

```
## categorized damage: 644753714588.105
```

```r
totals = ddply(damage, .(event), summarize,
               injuries = sum(INJURIES),
               fatalities = sum(FATALITIES),
               cropDamage = sum(cropDmgDollars),
               propDamage = sum(propDmgDollars))
totals$propDamagePct = 100 * totals$propDamage / sum(totals$propDamage)
totals$cropDamagePct = 100 * totals$cropDamage / sum(totals$cropDamage)
totals$injuriesPct = 100 * totals$injuries / sum(totals$injuries)
totals$fatalitiesPct = 100 * totals$fatalities / sum(totals$fatalities)
totals$fatalitiesRank = rank(totals$fatalitiesPct)
totals$injuriesRank = rank(totals$injuriesPct)
totals$cropDamageRank = rank(totals$cropDamagePct)
totals$propDamageRank = rank(totals$propDamagePct)
cutoff = 26
worstEvents = totals[totals$fatalitiesRank > cutoff &
                    totals$injuriesRank > cutoff &
                    totals$cropDamageRank > cutoff &
                    totals$propDamageRank > cutoff,]

final = worstEvents[,c("event","propDamage","cropDamage","injuries","fatalities")]
final = melt(final, id=c("event"))
colnames(final)[2] = "category"
final
```

```
##                  event   category     value
## 1             blizzard propDamage 9.171e+08
## 2          flash flood propDamage 2.940e+10
## 3                flood propDamage 1.635e+11
## 4           heavy snow propDamage 1.310e+09
## 5            high wind propDamage 7.437e+09
## 6  hurricane (typhoon) propDamage 1.022e+11
## 7          strong wind propDamage 2.000e+08
## 8    thunderstorm wind propDamage 1.651e+10
## 9              tornado propDamage 1.414e+11
## 10      tropical storm propDamage 9.694e+09
## 11            wildfire propDamage 1.052e+10
## 12        winter storm propDamage 9.750e+09
## 13            blizzard cropDamage 1.734e+08
## 14         flash flood cropDamage 9.756e+09
## 15               flood cropDamage 6.774e+09
## 16          heavy snow cropDamage 1.937e+08
## 17           high wind cropDamage 1.035e+09
## 18 hurricane (typhoon) cropDamage 6.938e+09
## 19         strong wind cropDamage 7.432e+07
## 20   thunderstorm wind cropDamage 1.615e+09
## 21             tornado cropDamage 5.302e+08
## 22      tropical storm cropDamage 8.532e+08
## 23            wildfire cropDamage 4.795e+08
## 24        winter storm cropDamage 3.998e+07
## 25            blizzard   injuries 7.780e+02
## 26         flash flood   injuries 1.578e+03
## 27               flood   injuries 6.785e+03
## 28          heavy snow   injuries 7.820e+02
## 29           high wind   injuries 1.289e+03
## 30 hurricane (typhoon)   injuries 1.328e+03
## 31         strong wind   injuries 2.180e+02
## 32   thunderstorm wind   injuries 5.014e+03
## 33             tornado   injuries 9.049e+04
## 34      tropical storm   injuries 3.800e+02
## 35            wildfire   injuries 1.328e+03
## 36        winter storm   injuries 1.027e+03
## 37            blizzard fatalities 7.000e+01
## 38         flash flood fatalities 7.780e+02
## 39               flood fatalities 4.140e+02
## 40          heavy snow fatalities 6.200e+01
## 41           high wind fatalities 2.060e+02
## 42 hurricane (typhoon) fatalities 1.090e+02
## 43         strong wind fatalities 7.700e+01
## 44   thunderstorm wind fatalities 3.760e+02
## 45             tornado fatalities 5.591e+03
## 46      tropical storm fatalities 5.600e+01
## 47            wildfire fatalities 7.900e+01
## 48        winter storm fatalities 8.900e+01
```

(48 baseline values, in eventtypes.csv)
(data loading)
(assignment of events)
(processing of damage costs)
(verifying small # of uncategorized)
(describe selection of most damaging event types)
(chart creation)


# Results
******
Here's some text

```r
set.seed(1)
x = rnorm(100)
mean(x)
```

```
## [1] 0.1089
```
