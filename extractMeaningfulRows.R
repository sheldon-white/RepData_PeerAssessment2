library(stringr)

#stormData = read.csv("repdata-data-StormData.csv")
#meaningfulData = stormData[stormData$FATALITIES > 0 | stormData$INJURIES > 0 | stormData$PROPDMG > 0 | stormData$CROPDMG > 0,]
#meaningfulData$normalizedEvtype = str_trim(tolower(meaningfulData$EVTYPE))
#meaningfulData$event = NA
#write.csv(meaningfulData, file = "meaningful.csv")

if (!exists("meaningfulData")) {
    meaningfulData = read.csv("meaningful.csv")
}

evTypes = read.csv("destEvtypes.csv", header = FALSE)
evTypes$V1 = tolower(evTypes$V1)
evTypes$evtype = evTypes$V1
colnames(evTypes) = c("pattern", "evtype")

rules = c(  
    "(typhoon|hurricane)", "hurricane (typhoon)",
    "(wall cloud|micoburst|microburst|downburst|turbulence|thunder|tstm)", "thunderstorm wind",
    "chill", "extreme cold/wind chill",  
    "wind", "high wind",   
    "ice fog", "freezing fog",
    "fog and cold temperatures", "freezing fog",
    "fog", "dense fog",
    "gustnado", "high wind",
    "torndao", "tornado",
    "funnel", "funnel cloud",
    "(ligntning|lighting)", "lightning",
    "(heavy|hvy|excessive|torrential|record).+(rain|rainfall|rainstorm|precipatation|precipitation)", "heavy rain",
    "(shower|rainstorm|rain damage|prolonged rain)", "heavy rain",
    "mixed precipitation", "heavy rain",   
    "rain.+heavy", "heavy rain",
    "(heavy|hvy|excessive|torrential|record).+snow", "heavy snow",
    "snow", "winter weather",
    "hail", "hail",
    "(sleet|freezing)", "sleet",
    "avalanche", "avalanche",
    "(swell|surf)", "high surf",
    "spout", "waterspout",
    "(coastal storm|surge)", "storm surge/tide",
    "(extreme|severe|heavy|hvy|excessive|torrential|record|unusual).+cold", "extreme cold/wind chill",
    "hypothermia", "extreme cold/wind chill",
    "cold", "cold/wind chill",
    "record low", "extreme cold/wind chill",
    "low temperature record", "extreme cold/wind chill",
    "low temperature", "cold/wind chill",
    "(frost|freeze)", "frost/freeze",
    "(tide)", "storm surge/tide",
    "(wave| seas)", "high surf",
    "smoke", "dense smoke",
    "fire", "wildfire",
    "wint", "winter weather",
    "(hot|warm).+wet", "excessive heat",
    "(hot|warm)", "heat",
    "record high", "heat",
    "high temperature", "excessive heat",
    "(slide|mud|landslump)", "debris flow",
    "avalance", "avalanche",
    "flash floooding", "flash flood",
    "(high water|fld|rapidly rising water)", "flood",
    "dust devel", "dust devil",
    "dust", "dust storm",
    "ice pellets", "hail",
    "(ice|icy|glaze)", "frost/freeze"
)

ruleset = data.frame(matrix(rules, ncol=2, byrow=TRUE))
colnames(ruleset) = c("pattern", "evtype")
allRules = rbind(evTypes, ruleset)

apply(allRules, 1, function(row) {
    pattern = row[1]   
    evtype = row[2]
    message(pattern, " - ", evtype)
    matches = grepl(pattern, meaningfulData$normalizedEvtype) & is.na(meaningfulData$event)
    meaningfulData[matches, "event"] <<- evtype
})

damageData = meaningfulData[meaningfulData$PROPDMG > 0 | meaningfulData$CROPDMG > 0,]
damageData = damageData[damageData$PROPDMGEXP %in% c("M", "K") | damageData$CROPDMGEXP %in% c("M", "K"),]
damageData$PROPDMGEXP = ifelse(damageData$PROPDMGEXP == "K", 1000, damageData$PROPDMGEXP)
damageData$CROPDMGEXP = ifelse(damageData$CROPDMGEXP == "K", 1000, damageData$CROPDMGEXP)
damageData$PROPDMGEXP = ifelse(damageData$PROPDMGEXP == "M", 1000000, damageData$PROPDMGEXP)
damageData$CROPDMGEXP = ifelse(damageData$CROPDMGEXP == "M", 1000000, damageData$CROPDMGEXP)
damageData$propDmgDollars = damageData$PROPDMGEXP * damageData$PROPDMG
damageData$cropDmgDollars = damageData$CROPDMGEXP * damageData$CROPDMG

uncategorized = damageData[is.na(damageData$event),]

damage = damageData[!is.na(damageData$event), c("propDmgDollars", "cropDmgDollars", "event")]
damage$event = factor(damage$event)

message("uncategorized damage: ", sum(uncategorized$propDmgDollars) + sum(uncategorized$cropDmgDollars))
message("categorized damage: ", sum(damage$propDmgDollars) + sum(damage$cropDmgDollars))


