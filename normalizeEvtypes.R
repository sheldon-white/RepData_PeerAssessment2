library(stringr)

dataEvtypes = read.csv("dataEvtypes.csv", header = FALSE)
colnames(dataEvtypes) = c("idx", "rawEvtype")
dataEvtypes$normalized = str_trim(tolower(dataEvtypes$rawEvtype))
dataEvtypes$dest = NA

destEvtypes = read.csv("destEvtypes.csv", header = FALSE)
destEvtypes$V1 = tolower(destEvtypes$V1)
destEvtypes$evtype = destEvtypes$V1
colnames(destEvtypes) = c("pattern", "evtype")


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
    "(slide|mud|landslump)", "debris flow",
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
allRules = rbind(destEvtypes, ruleset)

apply(allRules, 1, function(row) {
    pattern = row[1]   
    evtype = row[2]
    message(pattern, " - ", evtype)
    matches = grepl(pattern, dataEvtypes$normalized) & is.na(dataEvtypes$dest)
    dataEvtypes[matches, "dest"] <<- evtype
})

uncategorized = dataEvtypes[is.na(dataEvtypes$dest),]


#dataEvtypes[grepl("(none|\\?|summary|other)", dataEvtypes$normalized) & is.na(dataEvtypes$dest), "dest"] = "unknown"



message("categorized count = ", sum(complete.cases(dataEvtypes)))
