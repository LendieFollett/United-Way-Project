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

table(cps$fsecurity)
qplot(cps$fsecurity, geom = "histogram", binwidth = 1)
#There are quite a few 0's in this data... try just predicting
#whether or not household answered "Yes" at least once (binary)

# Estimate CPS model
reg <- glm(as.numeric(fsecurity != 0) ~ hhsize + female + kids + elderly + black + hispanic + education +
            employed + married, data=cps, family = binomial)
coeffs <- coefficients(reg)


breg_bin <- pbart(cps_X,
                  as.numeric(cps$fsecurity != 0), #predicting presence of food insecurity
              nskip=5000,
              ntree = 200,
              ndpost=5000,
              printevery=1000L,
              x.test = acs_X)


#compare in-sample (cps) predicted values
qplot(predict(reg, type = "response"),breg_bin$prob.train.mean)+
  labs(x = "Logistic regression probability", y = "BART probability")+
  geom_abline(aes(intercept = 0, slope = 1))

####PREDICTION ON ACS
acs$olr_fshat <- predict(reg, acs_X, type = "response")
acs$bart_fshat <- breg_bin$prob.test.mean

qplot(olr_fshat,bart_fshat, data = acs)+
  labs(x = "Logistic regression probability", y = "BART probability")+
  geom_abline(aes(intercept = 0, slope = 1))

#effects of x variables on predicted probability
ggplot(data=acs) +
  geom_smooth(aes(x = female/population, y = olr_fshat)) +
  geom_smooth(aes(x = female/population, y = bart_fshat, colour = "red")) 
ggplot(data=acs) +
  geom_smooth(aes(x = employed/population, y = olr_fshat)) +
  geom_smooth(aes(x = employed/population, y = bart_fshat, colour = "red")) 


##### MAPPING

ia_shp <- tracts(state = 'IA')
#for merging - use GEOID
#acs$NAME <- gsub("[,A-Za-z ]","",substr(acs$X, 16,nchar(acs$X)))
#join onto shape file
acs$GEOID <- as.character(acs$GEOID)
ia_shp_join <- left_join(ia_shp, acs, by="GEOID" )
str(ia_shp_join)

ggplot(aes(fill  = bart_fshat, colour=bart_fshat),data = ia_shp_join) +
  geom_sf()+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()
#i feel like this shouldn't take as long as it does.......?
#ah, it's because it's a many-to-many merge
#can we get more geoID's on the acs data?


polk_shp <- tracts(state = 'IA', county = "Polk")
polk_shp_join <- left_join(polk_shp, acs, by="NAME" )
str(polk_shp_join)

ggplot(aes(fill  = bart_fshat, colour=bart_fshat),data = polk_shp_join) +
  geom_sf()+
  scale_fill_viridis_c()+
  scale_colour_viridis_c()



