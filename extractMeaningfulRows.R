library(stringr)

stormData = read.csv("repdata-data-StormData.csv")

meaningfulData = stormData[stormData$FATALITIES > 0 | stormData$INJURIES > 0 | stormData$PROPDMG > 0 | stormData$CROPDMG > 0,]

meaningfulData$normalizedEvtype = str_trim(tolower(meaningfulData$EVTYPE))
meaningfulData$destEvtype = NA

destEvtypes = read.csv("destEvtypes.csv", header = FALSE)
destEvtypes$V1 = tolower(destEvtypes$V1)
colnames(destEvtypes) = c("name")

by(destEvtypes, 1:nrow(destEvtypes), function(row) {
    evtype = row$name
    matches = grepl(evtype, meaningfulData$normalizedEvtype) & is.na(meaningfulData$destEvtype)
    meaningfulData[matches, "destEvtype"] <<- evtype
})

uncategorized = meaningfulData[is.na(meaningfulData$destEvtype),]
message("uncategorized count = ", nrow(uncategorized))


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
    "(landslide|mud|landslump)", "debris flow",
    "avalance", "avalanche",
    "flash floooding", "flash flood",
    "(high water|fld|rapidly rising water)", "flood",
    "dust devel", "dust devil",
    "dust", "dust storm",
    "ice pellets", "hail",
    "(ice|icy)", "frost/freeze"
)

ruleset = data.frame(matrix(rules, ncol=2, byrow=TRUE))
colnames(ruleset) = c("pattern", "evtype")

by(ruleset, 1:nrow(ruleset), function(rule) {
    pattern = rule$pattern
    evtype = rule$evtype
    message(pattern, " - ", evtype)
    meaningfulData[grepl(pattern, meaningfulData$normalizedEvtype) & is.na(meaningfulData$destEvtype), "destEvtype"] <<- evtype
})

categorized = meaningfulData[!is.na(meaningfulData$destEvtype),]
uncategorized = meaningfulData[is.na(meaningfulData$destEvtype),]
message("uncategorized count = ", nrow(uncategorized))

damageData = meaningfulData[stormData$PROPDMG > 0 | stormData$CROPDMG > 0,]
damageData[is.na(damageData$PROPDMGEXP), c("PROPDMGEXP")] = 1
damageData$PROPDMGEXP[damageData$PROPDMGEXP == factor('K')] = 1000
class(damageData$PROPDMGEXP)
