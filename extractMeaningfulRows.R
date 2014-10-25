library(stringr)
library(plyr)
library(reshape)
library(ggplot2)

if (!exists("damageData")) {
    #stormData = read.csv(bzfile("repdata-data-StormData.csv.bz2"))
    #damageData = stormData[stormData$FATALITIES > 0 | stormData$INJURIES > 0 | stormData$PROPDMG > 0 | stormData$CROPDMG > 0,]
    #damageData$normalizedEvtype = str_trim(tolower(damageData$EVTYPE))
    #damageData$event = NA
    #write.csv(damageData, file = "meaningful.csv")
    
    
    rawDamageData = read.csv("meaningful.csv")
    damageData = rawDamageData[,c("FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "BGN_DATE", "EVTYPE")]
    damageData = damageData[damageData$INJURIES > 0 |
                            damageData$FATALITIES > 0 |
                            damageData$PROPDMGEXP %in% c("H", "M", "K", "B") |
                            damageData$CROPDMGEXP %in% c("H", "M", "K", "B"),]
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
damageData$normalizedEvtype = str_trim(tolower(damageData$EVTYPE))
damageData$event = NA

apply(allRules, 1, function(row) {
    pattern = row[1]   
    evtype = row[2]
    matches = grepl(pattern, damageData$normalizedEvtype) & is.na(damageData$event)
    damageData[matches, "event"] <<- evtype
})

damageData$year = strptime(damageData$BGN_DATE, "%m/%d/%Y %H:%M:%S")$year + 1900
class(damageData$year) = "integer"

inflationFactors = read.csv("inflationFactors.csv", header = FALSE)
colnames(inflationFactors) = c("year", "factor")
class(inflationFactors$year) = "integer"

inflation = function(y) {
    inflationFactors$factor[inflationFactors$year == y]
}
damageData$inflationFactor = as.numeric(lapply(damageData$year, inflation))

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

uncategorized = damageData[is.na(damageData$event),]

damage = damageData[!is.na(damageData$event), c("year", "FATALITIES", "INJURIES", "propDmgDollars", "cropDmgDollars", "event")]
damage$event = factor(damage$event)

message("uncategorized damage: ", sum(uncategorized$propDmgDollars) + sum(uncategorized$cropDmgDollars))
message("categorized damage: ", sum(damage$propDmgDollars) + sum(damage$cropDmgDollars))

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
worstDamageEvents = damageEvents[damageEvents$propDamagePct > 2 | damageEvents$cropDamagePct > 2, c("event","propDamage", "cropDamage")]
colnames(worstDamageEvents)[2:3] = c("Property Damage", "Crop Damage")
worstHealthEvents = healthEvents[healthEvents$injuriesPct > 2 & healthEvents$fatalitiesPct > 2, c("event","injuries", "fatalities")]
colnames(worstHealthEvents)[2:3] = c("Injuries", "Fatalities")

damageCollated = melt(worstDamageEvents, id=c("event"))
healthCollated = melt(worstHealthEvents, id=c("event"))
colnames(damageCollated)[2] = "category"
colnames(healthCollated)[2] = "category"
damageCollated$value = damageCollated$value / 1000000
ggplot(damageCollated, aes(event, value)) +
    geom_bar(aes(fill = category), position = "dodge", stat = "identity") + coord_flip() +
    xlab("Event Type") + ylab("Total Damage (Millons of Dollars)")
ggplot(healthCollated, aes(event, value)) +
    geom_bar(aes(fill = category), position = "dodge", stat="identity") + coord_flip() +
    xlab("Event Type") + ylab("Number of Affected People")

subset = damage[damage$event %in% c("tornado", "flood", "hurricane (typhoon)", "thunderstorm wind"),]
totals = ddply(subset, c("event", "year"), summarize,
               injuries = sum(INJURIES),
               fatalities = sum(FATALITIES),
               cropDamage = sum(cropDmgDollars),
               propDamage = sum(propDmgDollars))
totalsMelted = melt(totals, id=c("event", "year"))
ggplot(data=totalsMelted, aes(x=year, y = value, colour = event, group = event)) + 
    geom_line() + facet_wrap(~variable, scales = "free")

