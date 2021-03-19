rm(list = ls())
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
                  "employed", "married", "disability")
acs_X <- acs %>%
  mutate_at(c("female", "kids", "elderly", "black", "hispanic", "education",
              "employed", "married", "disability"), ~ . /  households) %>%
  dplyr::select("avg_hhsize", "female", "kids", "elderly", "black", "hispanic", "education",
                "employed", "married", "disability") %>%
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

table(cps$fsecurity)
qplot(cps$fsecurity, geom = "histogram", binwidth = 1)
#There are quite a few 0's in this data... try just predicting
#whether or not household answered "Yes" at least once (binary)

# Estimate CPS model - binary presence of >0 "Yes" answer to food insecurity questions
reg_bin <- glm(as.numeric(fsecurity != 0) ~ hhsize + female + kids + elderly + black + hispanic + education +
            employed + married + disability, data=cps[!is.na(cps$fsecurity),], family = binomial)
breg_bin <- pbart(cps_X[!is.na(cps$fsecurity),],
                  as.numeric(cps$fsecurity[!is.na(cps$fsecurity)] != 0), #predicting presence of food insecurity
              nskip=5000,
              ntree = 200,
              ndpost=5000,
              printevery=1000L,
              x.test = acs_X)
#Estimate CPS model - expenditure
reg<- lm(fexpend ~ hhsize + female + kids + elderly + black + hispanic + education +
                 employed + married + disability, data=cps[!is.na(cps$fexpend),])
breg <- wbart(cps_X[!is.na(cps$fexpend),],
              cps$fexpend[!is.na(cps$fexpend)], #predicting presence of food insecurity
                  nskip=5000,
                  ntree = 200,
                  ndpost=5000,
                  printevery=1000L,
                  x.test = acs_X)


#compare in-sample (cps) predicted values
qplot(predict(reg_bin, type = "response"),breg_bin$prob.train.mean)+
  labs(x = "Logistic regression probability", y = "BART probability")+
  geom_abline(aes(intercept = 0, slope = 1))

qplot(predict(reg, type = "response"),breg$yhat.train.mean)+
  labs(x = "OLR mean", y = "BART mean")+
  geom_abline(aes(intercept = 0, slope = 1))


#effects of x variables on predicted probability
ggplot() +
  geom_smooth(aes(x = cps$female[!is.na(cps$fsecurity)], y = predict(reg_bin, type = "response")))+
  geom_smooth(aes(x = cps$female[!is.na(cps$fsecurity)], y = breg_bin$prob.train.mean), colour = "red") +
  labs(x = "# Females in Household", y = "Predicted Probability Some Food Insecurity")

ggplot() +
  geom_boxplot(aes(x = as.factor(cps$disability[!is.na(cps$fsecurity)]), y = predict(reg_bin, type = "response")))+
  geom_boxplot(aes(x = as.factor(cps$disability[!is.na(cps$fsecurity)]), y = breg_bin$prob.train.mean), colour = "red") +
  labs(x = "Disability in Household", y = "Predicted Probability Some Food Insecurity")

#Other traits constant, the odds that a household with a disability experiences some level 
#of food insecurity are
exp(coef(reg_bin)['disability']) #over 4
#times those of a household without a disability.
#In the plot above, we can see that on average, no-disability homes have a probability of
#approximately 6% while homes with a disability have a probability of approximately 15%. 

#effects of x variables on food expenditures
ggplot() +
  geom_smooth(aes(x = cps$female[!is.na(cps$fexpend)], y = predict(reg, type = "response")))+
  geom_smooth(aes(x = cps$female[!is.na(cps$fexpend)], y = breg$yhat.train.mean), colour = "red") +
  labs(x = "# Females in Household", y = "Average Food Expenditure")

ggplot() +
  geom_boxplot(aes(x = as.factor(cps$disability[!is.na(cps$fexpend)]), y = predict(reg, type = "response")))+
  geom_boxplot(aes(x = as.factor(cps$disability[!is.na(cps$fexpend)]), y = breg$yhat.train.mean), colour = "red") +
  labs(x = "Disability in Household", y =  "Average Food Expenditure")



####PREDICTION ON ACS
acs$olr_bin_pred <- predict(reg_bin, acs_X, type = "response")
acs$bart_bin_pred <- breg_bin$prob.test.mean

acs$olr_pred <- predict(reg, acs_X, type = "response")
acs$bart_pred <- breg$yhat.test.mean

qplot(olr_bin_pred,bart_bin_pred, data = acs)+
  labs(x = "Logistic regression probability", y = "BART probability")+
  geom_abline(aes(intercept = 0, slope = 1))

qplot(olr_pred,bart_pred, data = acs)+
  labs(x = "OLR mean", y = "BART mean")+
  geom_abline(aes(intercept = 0, slope = 1))



##### MAPPING

#ia_shp <- tracts(state = 'IA') #this is census tracts
ia_shp <- block_groups(state = "IA") #this is block groups w/in tracts
#for merging - use GEOID
#join onto shape file
acs$GEOID <- as.character(paste0(acs$GEOID, substr(acs$X, 13, 13)))
ia_shp_join <- left_join(ia_shp, acs, by="GEOID" )
str(ia_shp_join)

ggplot(aes(fill  = bart_bin_pred, colour=bart_bin_pred),data = ia_shp_join) +
  geom_sf()+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()

ggplot(aes(fill  = bart_pred, colour=bart_pred),data = ia_shp_join) +
  geom_sf()+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()


#Can subset by county - we will have the ability to make this reactive in the dashboard environment

polk_shp <- block_groups(state = 'IA', county = "Polk")
polk_shp_join <- left_join(polk_shp, acs, by="GEOID" )
str(polk_shp_join)

ggplot(aes(fill  = bart_bin_pred, colour=bart_bin_pred),data = polk_shp_join) +
  geom_sf()+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()


ggplot(aes(fill  = bart_pred, colour=bart_pred),data = polk_shp_join) +
  geom_sf()+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()



