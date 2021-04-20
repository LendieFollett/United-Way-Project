rm(list = ls())
library(dplyr)
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

#----------BINARY FOOD INSECURITY MODELS -------

#Lasso regression
y_bin <- as.numeric(cps[!is.na(cps$fsecurity), "fsecurity"] != 0)
x_bin <- cps[!is.na(cps$fsecurity), c("hhsize", "female", "kids", "elderly", "black", "hispanic", "education",
                                  "employed", "married", "disability")] %>%as.matrix()

lasso_bin <- cv.glmnet(x_bin, y_bin,alpha = 1,family = "binomial", weights = cps$weight[!is.na(cps$fsecurity)]) #alpha = 1 --> lasso, alpha = 0 --> ridge
lasso_cv_bin  <- cv.glmnet(x_bin, y_bin,alpha = 1) #alpha = 1 --> lasso, alpha = 0 --> ridge
plot(lasso_cv_bin)
optimal_lambda_lasso_bin <- lasso_cv_bin$lambda.min

lasso_bin_coef <- coef(lasso_bin, s = "lambda.min")

#----------EXPENDITURE MODELS -------
#Lasso regression
y <- cps[!is.na(cps$fexpend), "fexpend"]
x <- cps[!is.na(cps$fexpend), c("hhsize", "female", "kids", "elderly", "black", "hispanic", "education",
                                "employed", "married", "disability")] %>%as.matrix()

lasso <- cv.glmnet(x, y,alpha = 1, weights = cps$weight[!is.na(cps$fexpend)]) #alpha = 1 --> lasso, alpha = 0 --> ridge
lasso_cv <- cv.glmnet(x, y,alpha = 1) #alpha = 1 --> lasso, alpha = 0 --> ridge
plot(lasso_cv)
optimal_lambda_lasso <- lasso_cv$lambda.min
#"weights is for the observation weights. Default is 1 for each observation. 
#(Note: glmnet rescales the weights to sum to N, the sample size."
lasso_coef<- coef(lasso_cv, s = "lambda.min")


#----------PREDICTION ON ACS--------
#save into csv for dashboard
acs_predictions <- acs %>%
  mutate_at(c("female", "kids", "elderly", "black", "hispanic", "education",
              "employed", "married", "disability"), ~ . /  households) %>%
  mutate_at(c("female", "kids", "elderly", "black", "hispanic", "education",
              "employed", "married", "disability"), round, 2) %>%
  dplyr::select("X","GEOID","avg_hhsize", "female", "kids", "elderly", "black", "hispanic", "education",
                "employed", "married", "disability") 
#predicted probabilities of any indication of food insecurity
acs_predictions$lasso_bin_pred <- predict(lasso_bin, as.matrix(acs_X), s = optimal_lambda_lasso_bin, type = "response")

#predicted mean expenditures
acs_predictions$lasso_pred <- predict(lasso, as.matrix(acs_X), s = optimal_lambda_lasso, type = "response")

write.csv(acs_predictions, "acs_predictions.csv", row.names=FALSE)

#----------EFFECTS OF DEMOGRAPHIC TRAITS--------

######## ON PROBABILITY OF SOME FOOD INSECURITY######## 
ggplot() +
  geom_boxplot(aes(x = cps$female[!is.na(cps$fsecurity)], 
                   y = predict(lasso_bin, x_bin,s = optimal_lambda_lasso_bin,type = "response"),
                   group = cps$female[!is.na(cps$fsecurity)]))+
  labs(x = "# Females in Household", y = "Predicted Probability Some Food Insecurity")

ggplot() +
  geom_boxplot(aes(x = cps$disability[!is.na(cps$fsecurity)], 
                   y = predict(lasso_bin, x_bin,s = optimal_lambda_lasso_bin,type = "response"),
                   group = cps$disability[!is.na(cps$fsecurity)]))+
  labs(x = "Disability", y = "Predicted Probability Some Food Insecurity")

#Other traits constant, the odds that a household with a disability experiences some level 
#of food insecurity are
exp(lasso_bin_coef['disability',]) #around 1.2
#times those of a household without a disability.
#In the plot above, we can see that on average, no-disability homes have a probability of
#approximately 7% while homes with a disability have a probability of approximately 20%. 

######## ON MEAN FOOD EXPENDITURE######## 
ggplot() +
  geom_boxplot(aes(x = cps$female[!is.na(cps$fexpend)], 
                   y = predict(lasso, x,s = optimal_lambda_lasso,type = "response"),
                   group = cps$female[!is.na(cps$fexpend)]))+
  labs(x = "# Females in Household", y = "Predicted Food Expenditure")

ggplot() +
  geom_boxplot(aes(x = cps$disability[!is.na(cps$fexpend)], 
                   y = predict(lasso, x,s = optimal_lambda_lasso,type = "response"),
                   group = cps$disability[!is.na(cps$fexpend)]))+
  labs(x = "Disability", y = "Predicted Food Expenditure")


## CREATE SPATIAL OBJECTS FOR DASHBOARD

ia_shp <- block_groups(state = "IA")  #TIME INTENSIVE 1st run (so each dashboard run)
saveRDS(ia_shp, "ia_shp.RDS")
county_list <- unique(counties("Iowa")$NAME)
saveRDS(county_list, "county_list.RDS")
