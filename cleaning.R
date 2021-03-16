# Note: See https://censusreporter.org for a listing of ACS tables. For a tutorial on the 
# acs package, see https://rpubs.com/rosenblum_jeff/censustutorial1. The CPS data was 
# downloaded from https://cps.ipums.org.

# Set up
setwd("~/Documents/OportUNITY/Food Security Map/Data")
library(acs)
library(readxl)

# API key
api.key.install(key="4a6312d77db109879039890c413e71c845d9c3ee")

# Get ACS data
myendyear <- 2019
myspan <- 5
geo.lookup(state="IA")
mygeo <- geo.make(state=19, county="*", tract="*", block.group="*")
mytable1 <- acs.lookup(endyear=2015, table.number="B01001") # Use 2015 just for a "look up" year
mytable2 <- acs.lookup(endyear=2015, table.number="B25010")
mytable3 <- acs.lookup(endyear=2015, table.number="B03003")
mytable4 <- acs.lookup(endyear=2015, table.number="B02001")
mytable5 <- acs.lookup(endyear=2015, table.number="B15003")
mytable6 <- acs.lookup(endyear=2015, table.number="B23025")
mytable7 <- acs.lookup(endyear=2015, table.number="B12001")
myvars <- mytable1[c(1,3:6,20:30,44:49)] + mytable2[1] + mytable3[3] + 
  mytable4[3] + mytable5[21:25] + mytable6[c(4,6)] + mytable7[c(4,13)] 
mydata <- acs.fetch(endyear=myendyear, span=myspan, geography=mygeo, variable=myvars)
acs <- data.frame(mydata@estimate)
acs$kids <- rowSums(acs[,c("B01001_003","B01001_004","B01001_005","B01001_006",
    "B01001_027","B01001_028","B01001_029","B01001_030")])
acs$elderly <- rowSums(acs[,c("B01001_020","B01001_021","B01001_022","B01001_023",
    "B01001_024","B01001_025","B01001_044","B01001_045","B01001_046","B01001_047",
    "B01001_048","B01001_049")])
acs$education <- rowSums(acs[,c("B15003_021","B15003_022","B15003_023","B15003_024",
    "B15003_025")])
acs$employed <- rowSums(acs[,c("B23025_004","B23025_006")])
acs$married <- rowSums(acs[,c("B12001_004","B12001_013")])
acs <- acs[,c("B01001_001","B01001_026","B25010_001","B03003_003","B02001_003",
    "kids","elderly","education","employed", "married")]
colnames(acs) <- c("population", "female", "avg_hhsize","hispanic","black","kids",
    "elderly","education","employed","married")
acs$households <- acs$population/acs$avg_hhsize
write.csv(acs, "acs(clean).csv")

# Get CPS data
cps <- data.frame(read_excel("cps(raw).xlsx"))
cps <- cps[cps$STATEFIP==19,]
cps <- cps[, c("CPSID", "PERNUM", "FSRAWSCRA", "AGE", "SEX",  "FAMSIZE", "RACE", "HISPAN", 
    "EDUC", "EMPSTAT","MARST")]
cps$SEX <- cps$SEX - 1
cps$CHILD <- ifelse(cps$AGE < 18, 1, 0)
cps$ELDERLY <- ifelse(cps$AGE > 64, 1, 0)
cps$BLACK <- ifelse(cps$RACE==200, 1, 0)
cps$HISPANIC <- ifelse(cps$HISPAN>0, 1, 0)
cps$EDUC <- as.integer(cps$EDUC %in% c(91,92,111,123,124,125))
cps$EMP <- as.integer(cps$EMPSTAT %in% c(1,10,12))
cps$MARRIED <- as.integer(cps$MARST %in% c(1,2))
cps <- merge(
  aggregate(list(fs=cps$FSRAWSCRA, hhsize=cps$FAMSIZE), by = list(id=cps$CPSID), mean),
  aggregate(list(female=cps$SEX, kids=cps$CHILD, elderly=cps$ELDERLY, black=cps$BLACK, 
      hispanic=cps$HISPANIC, education=cps$EDUC, employed=cps$EMP,
      married=cps$MARRIED), by = list(id=cps$CPSID), sum))
cps <- cps[cps$fs!=99,]
cps <- cps[cps$fs!=98,]
write.csv(cps, "cps(clean).csv")
