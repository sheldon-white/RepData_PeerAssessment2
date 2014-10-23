library(stringr)

#stormData = read.csv("repdata-data-StormData.csv")
#meaningfulData = stormData[stormData$FATALITIES > 0 | stormData$INJURIES > 0 | stormData$PROPDMG > 0 | stormData$CROPDMG > 0,]
#meaningfulData$normalizedEvtype = str_trim(tolower(meaningfulData$EVTYPE))
#meaningfulData$event = NA
#write.csv(meaningfulData, file = "meaningful.csv")

if (!exists("meaningfulData")) {
    meaningfulData = read.csv("meaningful.csv")
}

evTypes = read.csv("eventtypes.csv", header = FALSE)
evTypes$V1 = str_trim(tolower(evTypes$V1))
evTypes$evtype = evTypes$V1
colnames(evTypes) = c("pattern", "evtype")
evTypes$pattern = paste("^", evTypes$pattern, "$", sep="")

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

apply(allRules, 1, function(row) {
    pattern = row[1]   
    evtype = row[2]
    matches = grepl(pattern, meaningfulData$normalizedEvtype) & is.na(meaningfulData$event)
    message(pattern, " -> ", evtype, ": ", sum(matches))
    
    meaningfulData[matches, "event"] <<- evtype
})

damageData = meaningfulData[meaningfulData$PROPDMG > 0 | meaningfulData$CROPDMG > 0,]
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

damageData$propDmgDollars = damageData$propMultiplier * damageData$PROPDMG
damageData$cropDmgDollars = damageData$cropMultiplier * damageData$CROPDMG

uncategorized = damageData[is.na(damageData$event),]

damage = damageData[!is.na(damageData$event), c("FATALITIES", "INJURIES", "propDmgDollars", "cropDmgDollars", "event")]
damage$event = factor(damage$event)

message("uncategorized damage: ", sum(uncategorized$propDmgDollars) + sum(uncategorized$cropDmgDollars))
message("categorized damage: ", sum(damage$propDmgDollars) + sum(damage$cropDmgDollars))


