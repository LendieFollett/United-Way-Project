library(dplyr)
library(BART)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(tigris)
library(sp)
library(tidycensus)
library(rgeos)#spatial

cps <- read.csv("cps(clean).csv")#household level
acs <- read.csv("acs(clean).csv")#aggregated (sums) to census tract

cps_X <- cps %>% 
  dplyr::select("hhsize", "female", "kids", "elderly", "black", "hispanic", "education",
                  "employed", "married")
acs_X <- acs %>%
  mutate_at(c("female", "kids", "elderly", "black", "hispanic", "education",
              "employed", "married"), ~ . /  households) %>%
  dplyr::select("avg_hhsize", "female", "kids", "elderly", "black", "hispanic", "education",
                "employed", "married") %>%
  rename(hhsize = avg_hhsize)

#compare distributions
(melt(cps_X) %>% mutate(source = "CPS")) %>%
  rbind((melt(acs_X) %>% mutate(source = "ACS")) ) %>%
  ggplot() +
  geom_boxplot(aes(x = variable, y = value, colour = source)) +
  coord_flip()

(melt(cps_X) %>% mutate(source = "CPS")) %>%
  rbind((melt(acs_X) %>% mutate(source = "ACS")) ) %>%
  ggplot() +
  geom_histogram(aes(x = value,  fill = source), position = "dodge") +
  facet_wrap(~variable, scales = "free")


#Issue: adding interactions (via OLR or BART) complicates...
#The average of predicted values != predicted value for avg household
#The latter is what we would be predicting.
#Lose correlation among x variables. 
# Estimate CPS model
reg <- lm(fs ~ hhsize + female + kids + elderly + black + hispanic + education +
            employed + married, data=cps)
coeffs <- coefficients(reg)
breg <- wbart(cps_X,cps$fs,
      nskip=5000,
      sigdf=3, #default is 3
      ntree = 200,
      sigquant = .95,#default is .9
      sigest = sqrt(mean(reg$residuals^2)),
      ndpost=5000,
      printevery=1000L,
      x.test = acs_X)

table(cps$fs)
#There are quite a few 0's in this data...

breg_bin <- pbart(cps_X,
                  cps$fs != 0, #predicting presence of food insecurity
              nskip=5000,
              ntree = 200,
              ndpost=5000,
              printevery=1000L,
              x.test = acs_X)
#fit on nonzero data
breg_num <- gbart(cps_X[cps$fs != 0,],
                  cps$fs[cps$fs != 0], #predicting severity given presence of insecurity
                  nskip=5000,
                  sigdf=3, #default is 3
                  ntree = 200,
                  sigquant = .95,#default is .9
                  sigest = sqrt(mean(reg$residuals^2)),
                  ndpost=5000,
                  printevery=1000L,
                  x.test = cps_X)



#plot in-sample residuals
p1 <- qplot(x = predict(reg), y = residuals(reg)) + ggtitle("OLR") +scale_y_continuous(limits = c(-4, 7))
p2 <- qplot(x = breg$yhat.train.mean, y = cps$fs - breg$yhat.train.mean) + ggtitle("BART")+scale_y_continuous(limits = c(-4, 7))
p3 <- qplot(x = breg_bin$prob.train.mean*breg_num$yhat.test.mean, y = cps$fs - breg_bin$prob.train.mean*breg_num$yhat.test.mean) + ggtitle("2 step BART")+scale_y_continuous(limits = c(-4, 7))

grid.arrange(p1, p2,p3, nrow = 1)

qplot(predict(reg),breg_bin$prob.train.mean*breg_num$yhat.test.mean)+
  geom_abline(aes(intercept = 0, slope = 1))

####PREDICTION ON ACS

acs$olr_fshat <- coeffs[1]*acs$households + coeffs[2]*acs$population + coeffs[3]*acs$female +
  coeffs[4]*acs$kids + coeffs[5]*acs$elderly + coeffs[6]*acs$black + 
  coeffs[7]*acs$hispanic + coeffs[8]*acs$education + coeffs[9]*acs$employed +
  coeffs[10]*acs$married
acs$olr_fshat <- acs$olr_fshat/acs$households

acs$bart_fshat <- breg$yhat.test.mean

ggplot(data=acs) +
  geom_point(aes(x = olr_fshat, y = bart_fshat)) +
  geom_abline(aes(intercept = 0, slope = 1))

##### MAPPING

ia_shp <- tracts(state = 'IA')
#for merging - just take tract number
acs$NAME <- gsub("[,A-Za-z ]","",substr(acs$X, 16,nchar(acs$X)))
#join onto shape file
ia_shp_join <- left_join(ia_shp, acs, by="NAME" )
str(ia_shp_join)

ggplot(aes(fill  = bart_fshat),data = ia_shp_join) +
  geom_sf()+
  scale_fill_viridis_c()
#i feel like this shouldn't take as long as it does.......?

#Individual block g
ggplot(aes(fill  = bart_fshat),data = subset(ia_shp_join, grepl("Block Group 1", X))) +
  geom_sf()+
  scale_fill_viridis_c()


