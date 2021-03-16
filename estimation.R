# Note: See https://censusreporter.org for a listing of ACS tables. For a tutorial on the 
# acs package, see https://rpubs.com/rosenblum_jeff/censustutorial1. The CPS data was 
# downloaded from https://cps.ipums.org.

# Set up
#setwd("~/Documents/OportUNITY/Food Security Map/Data")
cps <- read.csv("cps(clean).csv")
acs <- read.csv("acs(clean).csv")

# Estimate CPS model
reg <- lm(fs ~ hhsize + female + kids + elderly + black + hispanic + education +
    employed + married, data=cps)
coeffs <- coefficients(reg)

# Estimate food security for block groups
acs$fshat <- coeffs[1]*acs$households + coeffs[2]*acs$population + coeffs[3]*acs$female +
    coeffs[4]*acs$kids + coeffs[5]*acs$elderly + coeffs[6]*acs$black + 
    coeffs[7]*acs$hispanic + coeffs[8]*acs$education + coeffs[9]*acs$employed +
    coeffs[10]*acs$married
acs$fshat <- acs$fshat/acs$households
summary(acs$fshat)
plot(density(na.omit(acs$fshat)))
