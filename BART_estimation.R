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
library(glmnet)#lasso

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

#----------BINARY FOOD INSECURITY MODELS -------
# Estimate CPS model - binary presence of >0 "Yes" answer to food insecurity questions
reg_bin <- glm(as.numeric(fsecurity != 0) ~ hhsize + female + kids + elderly + black + hispanic + education +
            employed + married + disability, data=cps[!is.na(cps$fsecurity),], family = binomial)
#Lasso regression
y <- as.numeric(cps[!is.na(cps$fsecurity), "fsecurity"] != 0)
x <- cps[!is.na(cps$fsecurity), c("hhsize", "female", "kids", "elderly", "black", "hispanic", "education",
                                "employed", "married", "disability")] %>%as.matrix()

lasso_bin <- cv.glmnet(x, y,alpha = 1,family = "binomial") #alpha = 1 --> lasso, alpha = 0 --> ridge
lasso_cv_bin  <- cv.glmnet(x, y,alpha = 1) #alpha = 1 --> lasso, alpha = 0 --> ridge
plot(lasso_cv_bin)
optimal_lambda_lasso_bin <- lasso_cv_bin$lambda.min


breg_bin <- pbart(cps_X[!is.na(cps$fsecurity),],
                  as.numeric(cps$fsecurity[!is.na(cps$fsecurity)] != 0), #predicting presence of food insecurity
              nskip=5000,
              ntree = 200,
              ndpost=5000,
              printevery=1000L,
              x.test = acs_X)

#----------EXPENDITURE MODELS -------
#Estimate CPS model - expenditure
reg<- lm(fexpend ~ hhsize + female + kids + elderly + black + hispanic + education +
                 employed + married + disability, data=cps[!is.na(cps$fexpend),])
#Lasso regression
y <- cps[!is.na(cps$fexpend), "fexpend"]
x <- cps[!is.na(cps$fexpend), c("hhsize", "female", "kids", "elderly", "black", "hispanic", "education",
           "employed", "married", "disability")] %>%as.matrix()

lasso <- cv.glmnet(x, y,alpha = 1) #alpha = 1 --> lasso, alpha = 0 --> ridge
lasso_cv <- cv.glmnet(x, y,alpha = 1) #alpha = 1 --> lasso, alpha = 0 --> ridge
plot(lasso_cv)
optimal_lambda_lasso <- lasso_cv$lambda.min

breg <- wbart(cps_X[!is.na(cps$fexpend),],
              cps$fexpend[!is.na(cps$fexpend)], #predicting presence of food insecurity
                  nskip=5000,
                  ntree = 200,
                  ndpost=5000,
                  printevery=1000L,
                  x.test = acs_X)

####PREDICTION ON ACS
#predicted probabilities of any indicatin of food insecurity
acs$lasso_bin_pred <- predict(lasso_bin, as.matrix(acs_X), s = optimal_lambda_lasso_bin, type = "response")
acs$bart_bin_pred <- breg_bin$prob.test.mean

#predicted mean expenditures
acs$lasso_pred <- predict(lasso, as.matrix(acs_X), s = optimal_lambda_lasso, type = "response")
acs$bart_pred <- breg$yhat.test.mean



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
exp(coef(reg_bin)['disability']) #around four
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



#compare out of sample predicted values
p1 <- qplot(lasso_bin_pred,bart_bin_pred, data = acs)+
  labs(x = "Lasso logistic regression probability", y = "BART probability")+
  geom_abline(aes(intercept = 0, slope = 1))

p2 <- qplot(lasso_pred,bart_pred, data = acs)+
  labs(x = "Lasso regression mean", y = "BART mean")+
  geom_abline(aes(intercept = 0, slope = 1))

grid.arrange(p1,p2, nrow = 1)


write.csv(acs, "acs_predictions.csv")

##### MAPPING
#maybe:
#https://bookdown.org/robinlovelace/geocompr/adv-map.html#interactive-maps

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



