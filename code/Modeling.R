### modeling for ### ### ###  Examining variability and generalization in dimension based statistical learning for speech: The case of place of articulation  ### ### ###

# last updated by Jeremy Steffman 28th February 2025

# load required packages and set wd
library(brms);library(tidyverse);library(bayestestR);library(emmeans)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# load in data
same.all <- read.csv("data/same_place_all_frames.csv")
switch.all <- read.csv("data/switch_place_all_frames.csv")


# same place model

priors_norm <- c(set_prior("normal(0,1.5)", class = "b"),
                 set_prior("normal(0,1.5)", class = "Intercept"))

same.all %>% filter(trial_type=="test") -> same.all.test
same.all.test$F0_step<-as.factor(same.all.test$F0_step)
contrasts(same.all.test$F0_step) <-c(-0.5,0.5)
same.all.test$dist<-as.factor(same.all.test$dist)
contrasts(same.all.test$dist) <-c(-0.5,0.5)
same.all.test$place<-as.factor(same.all.test$place)
contrasts(same.all.test$place) <-contr.sum(3)

switch.all %>% filter(trial_type=="test") -> switch.all.test
switch.all.test$F0_step<-as.factor(switch.all.test$F0_step)
contrasts(switch.all.test$F0_step) <-c(-0.5,0.5)
switch.all.test$dist<-as.factor(switch.all.test$dist)
contrasts(switch.all.test$dist) <-c(-0.5,0.5)
switch.all.test$place<-as.factor(switch.all.test$place)
contrasts(switch.all.test$place) <-contr.sum(3)


# "filename" identifies each unique participant
model.same.place<-brm(formula= response_num~F0_step*dist*place+
                     (1+F0_step|filename)+(1+F0_step*place|frame),
                   data = same.all.test,
                   prior = priors_norm,
                   family = bernoulli(link = "logit"),
                   file = "models/same.all.place",
                   iter = 6000, warmup = 1000,cores=4,chains = 4,
                   control = list(adapt_delta=0.999,max_treedepth=15))

summary(model.same.place)
pd(model.same.place)
emmeans(model.same.place, pairwise ~ F0_step|dist)
emmeans(model.same.place, pairwise ~ F0_step|dist*place)
emmeans(model.same.place, "dist")

model.switch.place<-brm(formula= response_num~F0_step*dist*place+
                        (1+F0_step|filename)+(1+F0_step*place|frame),
                      data = switch.all.test,
                      prior = priors_norm,
                      family = bernoulli(link = "logit"),
                      file = "models/switch.all.place",
                      iter = 6000, warmup = 1000,cores=4,chains = 4,
                      control = list(adapt_delta=0.999,max_treedepth=15))

summary(model.switch.place)
pd(model.switch.place)
emmeans(model.switch.place, pairwise ~ F0_step|dist)
emmeans(model.switch.place, pairwise ~ F0_step|dist*place)



## alternative models with place included as a random effect
model.same.all<-brm(formula= response_num~F0_step*dist+
                        (1+F0_step|filename)+(1+F0_step|place*frame),
                      data = same.all.test,
                      prior = priors_norm,
                      family = bernoulli(link = "logit"),
                      file = "models/model.same.all.rand.place",
                      iter = 6000, warmup = 1000,cores=4,chains = 4,
                      control = list(adapt_delta=0.999,max_treedepth=15))
pd(model.same.all)

model.switch.all<-brm(formula= response_num~F0_step*dist+
                      (1+F0_step|filename)+(1+F0_step|place*frame),
                    data = switch.all.test,
                    prior = priors_norm,
                    family = bernoulli(link = "logit"),
                    file = "models/model.switch.all.rand.place",
                    iter = 6000, warmup = 1000,cores=4,chains = 4,
                    control = list(adapt_delta=0.999,max_treedepth=15))


pd(model.switch.all)
