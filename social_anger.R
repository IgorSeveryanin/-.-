

library(tidyverse)
library(car)
library(stargazer)
library(xtable)
library(AER)

# Set working directory
dir <- "set_working_directory_here"
setwd(dir)

#################
# ANES analysis #
#################
anesfull <- read.delim("data/anes_timeseries_cdf_rawdata.txt", sep = "|")

# rename variables
anesfull <- anesfull %>%
  rename(year = VCF0004,
         pid7 = VCF0301,
         anger_dem_pres_can = VCF0358,
         anger_gop_pres_can = VCF0370)

anesfull$anger_dem_pres_can <- recode(anesfull$anger_dem_pres_can, "2=0;1=1")
anesfull$anger_gop_pres_can <- recode(anesfull$anger_gop_pres_can, "2=0;1=1")

anesfull$pid7[anesfull$pid7==0] <- NA
anesfull <- anesfull %>%
  mutate(pid2 = case_when(
    pid7 < 4 ~ "Democrat",
    pid7 > 4 ~ "Republican"),
    democrat = as.numeric(pid2 == "Democrat"),
    republican = as.numeric(pid2 == "Republican"),
    pid2factor = as.factor(pid2))

anesfull <- anesfull %>%
  mutate(anger_opposing_candidate = case_when(
    pid2 == "Democrat" ~ anger_gop_pres_can,
    pid2 == "Republican" ~ anger_dem_pres_can
  ))

anesfull$anger_opposing_candidate[anesfull$anger_opposing_candidate %in% c(8,9)] <- NA

plotdat <- anesfull %>%
  filter(year %in% c(1980,1984,1988,1992,1996,2000,2004,2008,2012,2016)) %>%
  group_by(year) %>%
  summarize(prop_anger = mean(anger_opposing_candidate, na.rm = TRUE)) %>%
  mutate(pct_angry = prop_anger * 100)

anger_plot <- ggplot(plotdat, aes(year, pct_angry)) + geom_line() +
  theme_bw() + xlab("Year") + ylab("Percent Angry") +
  scale_y_continuous(breaks = c(40,50,60,70,80,90)) +
  scale_x_continuous(breaks = c(1980,1984,1988,1992,1996,
                                2000,2004,2008,2012,2016),
                     labels = c("1980","1984","1988","1992",
                                "1996","2000","2004",
                                "2008","2012","2016"))

anger_plot

############################
# Social polarization data #
############################
#### 1. Load in data and clean variables ####
# load data
load("data/anger_social.RData")

# filter out independentsand those who did not answer PID question
dat <- dat %>%
  filter(!(pid7 %in% c("Completely Independent", "")))

dat <- dat %>%
  filter(Finished == "True")

# clean variables
# partisanship
dat$pid7 <- as.character(dat$pid7)
dat <- dat %>%
  mutate(pid = case_when(
    pid7 == "Strong Republican" ~ 1,
    pid7 == "Weak Republican" ~ 2,
    pid7 == "Independent but lean Republican" ~ 3,
    pid7 == "Independent but lean Democrat" ~ 4,
    pid7 == "Weak Democrat" ~ 5,
    pid7 == "Strong Democrat" ~ 6),
    democrat = as.numeric(pid > 3))

# gender
dat$gender <- as.character(dat$gender)
dat <- dat %>%
  mutate(female = case_when(
    gender == "Female" ~ 1,
    gender == "Male" ~ 0
  ))

# race / ethnicity
dat$race_eth <- as.character(dat$race_eth)
dat$race_eth[dat$race_eth == ""] <- NA
dat <- dat %>%
  mutate(nonwhite = as.numeric(race_eth != "White, non-Hispanic"))

# education
dat$educ <- as.character(dat$educ)
dat$educ[dat$educ == ""] <- NA
dat <- dat %>%
  mutate(ba_plus = as.numeric(
    educ %in% c("Bachelor's degree","Post-graduate degree","Professional degree")
  ))

# ideology
dat$ideo7 <- as.character(dat$ideo7)
dat$ideo7[dat$ideo7 == ""] <- NA
dat <- dat %>%
  mutate(ideology = case_when(
    ideo7 == "Very liberal" ~ 1,
    ideo7 == "Liberal" ~ 2,
    ideo7 == "Slightly liberal" ~ 3,
    ideo7 == "Moderate; middle of the road" ~ 4,
    ideo7 == "Slightly conservative" ~ 5,
    ideo7 == "Conservative" ~ 6,
    ideo7 == "Very conservative" ~ 7))

# birth year / age
dat$birth_yr <- as.numeric(as.character(dat$birth_yr))
dat$age <- 2020 - dat$birth_yr

# self-monitoring questions
dat$sm1 <- as.character(dat$sm1)
dat$sm2 <- as.character(dat$sm2)
dat$sm3 <- as.character(dat$sm3)

dat <- dat %>%
  mutate(selfmonitoring1 = case_when(
    sm1 == "Never" ~ 0,
    sm1 == "Once in a while" ~ 1,
    sm1 == "Some of the time" ~ 2,
    sm1 == "Most of the time" ~ 3,
    sm1 == "Always" ~ 4),
    selfmonitoring2 = case_when(
      sm2 == "Never" ~ 0,
      sm2 == "Once in a while" ~ 1,
      sm2 == "Some of the time" ~ 2,
      sm2 == "Most of the time" ~ 3,
      sm2 == "Always" ~ 4),
    selfmonitoring3 = case_when(
      sm3 == "Very poor" ~ 0,
      sm3 == "Poor" ~ 1,
      sm3 == "Fair" ~ 2,
      sm3 == "Good" ~ 3,
      sm3 == "Excellent" ~ 4),
    selfmonitoring = selfmonitoring1 + selfmonitoring2 + selfmonitoring3)

dat <- dat %>%
  mutate(treated = as.numeric(experimentfordemocrats_DO == "treatment_dems" |
                                experimentforrepublicans_DO == "treatment_gop"),
         control = as.numeric(experimentfordemocrats_DO == "breakfast_dems" |
                                experimentforrepublicans_DO == "breakfast_gop"))


# dependent variables
# fourpack
dat$fourpack_1 <- as.character(dat$fourpack_1)
dat$fourpack_2 <- as.character(dat$fourpack_2)
dat$fourpack_3 <- as.character(dat$fourpack_3)
dat$fourpack_4 <- as.character(dat$fourpack_4)

dat$fourpack_1 <- car::recode(dat$fourpack_1,
                              "'Always'=0;
                              'Most of the time'=1;
                              'About half the time'=2;
                              'Sometimes'=3;
                              'Never'=4")

dat$fourpack_1[!(dat$fourpack_1 %in% c(0,1,2,3,4))] <- NA
dat$fourpack_1 <- as.numeric(dat$fourpack_1)

dat$fourpack_2 <- car::recode(dat$fourpack_2,
                              "'Always'=0;
                              'Most of the time'=1;
                              'About half the time'=2;
                              'Sometimes'=3;
                              'Never'=4")

dat$fourpack_2[!(dat$fourpack_2 %in% c(0,1,2,3,4))] <- NA
dat$fourpack_2 <- as.numeric(dat$fourpack_2)

dat$fourpack_3 <- car::recode(dat$fourpack_3,
                              "'Always'=0;
                              'Most of the time'=1;
                              'About half the time'=2;
                              'Sometimes'=3;
                              'Never'=4")

dat$fourpack_3[!(dat$fourpack_3 %in% c(0,1,2,3,4))] <- NA
dat$fourpack_3 <- as.numeric(dat$fourpack_3)

dat$fourpack_4 <- car::recode(dat$fourpack_4,
                              "'Always'=0;
                              'Most of the time'=1;
                              'About half the time'=2;
                              'Sometimes'=3;
                              'Never'=4")

dat$fourpack_4[!(dat$fourpack_4 %in% c(0,1,2,3,4))] <- NA
dat$fourpack_4 <- as.numeric(dat$fourpack_4)

# talking at a party
dat$talking <- as.character(dat$talking)
dat$talking <- car::recode(dat$talking,
                           "'Continue talking to them, including about politics'=0;
                           'Continue talking to them, but not about politics'=1;
                           'Try to find a polite way out of the conversation'=2;
                           'Leave the conversation without worrying about being polite'=3;
                           'Attack their political views'=4")
dat$talking[!(dat$talking %in% c(0,1,2,3,4))] <- NA
dat$talking <- as.numeric(dat$talking)

# coffee / drink / meal
dat$meal <- as.character(dat$meal)
dat$meal <- car::recode(dat$meal,
                        "'Certainly go'=0;
                        'Go if I had nothing better to do'=1;
                        'Try to find a polite way to say no'=2;
                        'Say no'=3;
                        'Say no and attack their political views'=4")
dat$meal[!(dat$meal %in% c(0,1,2,3,4))] <- NA
dat$meal <- as.numeric(dat$meal)

# social event or club
dat$club <- as.character(dat$club)
dat$club <- car::recode(dat$club,
                        "'Certainly go'=0;
                        'Go if I had nothing better to do'=1;
                        'Try to find a polite way to say no'=2;
                        'Say no'=3;
                        'Say no and talk badly about those who are attending to your friends'=4")
dat$club[!(dat$club %in% c(0,1,2,3,4))] <- NA
dat$club <- as.numeric(dat$club)

# first date
dat$date <- as.character(dat$date)
dat$date <- car::recode(dat$date,
                        "'Certainly go'=0;
                        'Go if I had nothing better to do'=1;
                        'Try to find a polite way to say no'=2;
                        'Say no'=3;
                        'Say no and talk badly about the person to your friends'=4")
dat$date[!(dat$date %in% c(0,1,2,3,4))] <- NA
dat$date <- as.numeric(dat$date)

# family member dating out-partisan
dat$family_date <- as.character(dat$family_date)
dat$family_date <- car::recode(dat$family_date,
                               "'Certainly'=0;
                               'Yes, but only if the person was otherwise a good person'=1;
                               'Probably not'=2;
                               'Certainly not'=3")
dat$family_date[!(dat$family_date %in% c(0,1,2,3))] <- NA
dat$family_date <- as.numeric(dat$family_date)

# close friend is an out-partisan
dat$friendship <- as.character(dat$friendship)
dat$friendship <- car::recode(dat$friendship,
                              "'I would change nothing about my friendship'=0;
                              'I would remain friends with them but would not discuss politics'=1;
                              'I would remain friends with them but I would attack their political beliefs'=2;
                              'I would end the friendship'=3")
dat$friendship[!(dat$friendship %in% c(0,1,2,3))] <- NA
dat$friendship <- as.numeric(dat$friendship)

# family member is out-partisan
dat$family <- as.character(dat$family)
dat$family <- car::recode(dat$family,
                          "'I would treat them the exact same'=0;
                          'I would treat them the same but not talk about politics'=1;
                          'I would distance myself a bit'=2;
                          'I would cut them out of my life as much as possible'=3;
                          'I would cut them out of my life as much as possible, and I would attack their political beliefs'=4")
dat$family[dat$family == ""] <- NA
dat$family <- as.numeric(dat$family)

# friend got divorced from out-partisan
dat$divorce_gop <- as.character(dat$divorce_gop)
dat$divorce_gop <- car::recode(dat$divorce_gop,
                               "'Very sad'=0;
                               'Sad'=1;
                               'Neither sad nor happy'=2;
                               'Happy'=3;
                               'Very happy'=4")
dat$divorce_gop[!(dat$divorce_gop %in% c(0,1,2,3,4))] <- NA
dat$divorce_gop <- as.numeric(dat$divorce_gop)

# comfortable having friends who are out-partisans?
dat$friends_comfort <- as.character(dat$friends_comfort)
dat$friends_comfort <- car::recode(dat$friends_comfort,
                                   "'Extremely comfortable'=0;
                                   'Moderately comfortable'=1;
                                   'Slightly comfortable'=2;
                                   'Neither comfortable nor uncomfortable'=3;
                                   'Slightly uncomfortable'=4;
                                   'Moderately uncomfortable'=5;
                                   'Extremely uncomfortable'=6")
dat$friends_comfort[!(dat$friends_comfort %in% c(0,1,2,3,4,5,6))] <- NA
dat$friends_comfort <- as.numeric(dat$friends_comfort)

# comfortable having neighbors on your street who are out-partisans?
dat$neighbors_comfort <- as.character(dat$neighbors_comfort)
dat$neighbors_comfort <- car::recode(dat$neighbors_comfort,
                                     "'Extremely comfortable'=0;
                                   'Moderately comfortable'=1;
                                   'Slightly comfortable'=2;
                                   'Neither comfortable nor uncomfortable'=3;
                                   'Slightly uncomfortable'=4;
                                   'Moderately uncomfortable'=5;
                                   'Extremely uncomfortable'=6")
dat$neighbors_comfort[!(dat$neighbors_comfort %in% c(0,1,2,3,4,5,6))] <- NA
dat$neighbors_comfort <- as.numeric(dat$neighbors_comfort)

# upset with son or daughter marrying out-partisan?
dat$marry_upset <- as.character(dat$marry_upset)
dat$marry_upset <- car::recode(dat$marry_upset,
                               "'Not at all upset'=0;
                               'Not too upset'=1;
                               'Somewhat upset'=2;
                               'Upset'=3;
                               'Extremely upset'=4")
dat$marry_upset[!(dat$marry_upset %in% c(0,1,2,3,4))] <- NA
dat$marry_upset <- as.numeric(dat$marry_upset)

# share "questionable" news about out-partisans?
dat$news_share <- as.character(dat$news_share)
dat$news_share <- car::recode(dat$news_share,
                              "'Extremely unlikely'=0;
                              'Moderately unlikely'=1;
                              'Slightly unlikely'=2;
                              'Neither unlikely nor likely'=3;
                              'Slightly likely'=4;
                              'Moderately likely'=5;
                              'Extremely likely'=6")
dat$news_share[!(dat$news_share %in% c(0,1,2,3,4,5,6))] <- NA
dat$news_share <- as.numeric(dat$news_share)

# scale dv's to range from 0-1
dat$fourpack_1_01 <- (dat$fourpack_1 - min(dat$fourpack_1, na.rm=T)) / (max(dat$fourpack_1, na.rm=T) - min(dat$fourpack_1, na.rm=T))
dat$fourpack_2_01 <- (dat$fourpack_2 - min(dat$fourpack_2, na.rm=T)) / (max(dat$fourpack_2, na.rm=T) - min(dat$fourpack_2, na.rm=T))
dat$fourpack_3_01 <- (dat$fourpack_3 - min(dat$fourpack_3, na.rm=T)) / (max(dat$fourpack_3, na.rm=T) - min(dat$fourpack_3, na.rm=T))
dat$fourpack_4_01 <- (dat$fourpack_4 - min(dat$fourpack_4, na.rm=T)) / (max(dat$fourpack_4, na.rm=T) - min(dat$fourpack_4, na.rm=T))
dat$talking_01 <- (dat$talking - min(dat$talking, na.rm=T)) / (max(dat$talking, na.rm=T) - min(dat$talking, na.rm=T))
dat$meal_01 <- (dat$meal - min(dat$meal, na.rm=T)) / (max(dat$meal, na.rm=T) - min(dat$meal, na.rm=T))
dat$club_01 <- (dat$club - min(dat$club, na.rm=T)) / (max(dat$club, na.rm=T) - min(dat$club, na.rm=T))
dat$date_01 <- (dat$date - min(dat$date, na.rm=T)) / (max(dat$date, na.rm=T) - min(dat$date, na.rm=T))
dat$family_date_01 <- (dat$family_date - min(dat$family_date, na.rm=T)) / (max(dat$family_date, na.rm=T) - min(dat$family_date, na.rm=T))
dat$friendship_01 <- (dat$friendship - min(dat$friendship, na.rm=T)) / (max(dat$friendship, na.rm=T) - min(dat$friendship, na.rm=T))
dat$family_01 <- (dat$family - min(dat$family, na.rm=T)) / (max(dat$family, na.rm=T) - min(dat$family, na.rm=T))
dat$divorce_gop_01 <- (dat$divorce_gop - min(dat$divorce_gop, na.rm=T)) / (max(dat$divorce_gop, na.rm=T) - min(dat$divorce_gop, na.rm=T))
dat$friends_comfort_01 <- (dat$friends_comfort - min(dat$friends_comfort, na.rm=T)) / (max(dat$friends_comfort, na.rm=T) - min(dat$friends_comfort, na.rm=T))
dat$neighbors_comfort_01 <- (dat$neighbors_comfort - min(dat$neighbors_comfort, na.rm=T)) / (max(dat$neighbors_comfort, na.rm=T) - min(dat$neighbors_comfort, na.rm=T))
dat$marry_upset_01 <- (dat$marry_upset - min(dat$marry_upset, na.rm=T)) / (max(dat$marry_upset, na.rm=T) - min(dat$marry_upset, na.rm=T))
dat$news_share_01 <- (dat$news_share - min(dat$news_share, na.rm=T)) / (max(dat$news_share, na.rm=T) - min(dat$news_share, na.rm=T))


#### 2. Models and figures found in the  manuscript ####
# Regressions (scaled dv's)
m1b <- lm(fourpack_1_01 ~ treated + democrat, data = dat)
m2b <- lm(fourpack_2_01 ~ treated + democrat, data = dat)
m3b <- lm(fourpack_3_01 ~ treated + democrat, data = dat)
m4b <- lm(fourpack_4_01 ~ treated + democrat, data = dat)
m5b <- lm(talking_01 ~ treated + democrat, data = dat)
m6b <- lm(meal_01 ~ treated + democrat, data = dat)
m7b <- lm(club_01 ~ treated + democrat, data = dat)
m8b <- lm(date_01 ~ treated + democrat, data = dat)
m9b <- lm(family_date_01 ~ treated + democrat, data = dat)
m10b <- lm(friendship_01 ~ treated + democrat, data = dat)
m11b <- lm(family_01 ~ treated + democrat, data = dat)
m12b <- lm(divorce_gop_01 ~ treated + democrat, data = dat)
m13b <- lm(friends_comfort_01 ~ treated + democrat, data = dat)
m14b <- lm(neighbors_comfort_01 ~ treated + democrat, data = dat)
m15b <- lm(marry_upset_01 ~ treated + democrat, data = dat)
m16b <- lm(news_share_01 ~ treated + democrat, data = dat)

results1 <- list(m1b, m2b, m3b, m4b, m5b, m6b, m7b, m8b)
results2 <- list(m9b, m10b, m11b, m12b, m13b, m14b, m15b, m16b)

# Output main regression results (found in Appendix of paper)
# These tables give the coefficient estimates that are used to
# produce Figures 2 & 3 in the manuscript.
stargazer(results1,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Democrat"),
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(results2,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Democrat"),
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))


# Treatment effects plot (Figures 2 & 3)
fx <- data.frame(
  Variable = c("Do favors","Watch property","Ask personal things","Talk about politics",
               "Talking at party","Coffee / meal / drink","Social gathering","First date",
               "Family/friend dating out-partisan","Friend is an out-partisan",
               "Family member is an out-partisan","Friend's divorce","Comfort w/ out-partisan friends",
               "Comfort w/ out-partisan neighbors","Child marry an out-partisan","Share fake news"),
  Effect = c(m1b$coefficients[2], m2b$coefficients[2], m3b$coefficients[2], m4b$coefficients[2],
             m5b$coefficients[2], m6b$coefficients[2], m7b$coefficients[2], m8b$coefficients[2],
             m9b$coefficients[2], m10b$coefficients[2], m11b$coefficients[2], m12b$coefficients[2],
             m13b$coefficients[2], m14b$coefficients[2], m15b$coefficients[2], m16b$coefficients[2]),
  SE = c(sqrt(diag(vcov(m1b)))[2], sqrt(diag(vcov(m2b)))[2], sqrt(diag(vcov(m3b)))[2], sqrt(diag(vcov(m4b)))[2],
         sqrt(diag(vcov(m5b)))[2], sqrt(diag(vcov(m6b)))[2], sqrt(diag(vcov(m7b)))[2], sqrt(diag(vcov(m8b)))[2],
         sqrt(diag(vcov(m9b)))[2], sqrt(diag(vcov(m10b)))[2], sqrt(diag(vcov(m11b)))[2], sqrt(diag(vcov(m12b)))[2],
         sqrt(diag(vcov(m13b)))[2], sqrt(diag(vcov(m14b)))[2], sqrt(diag(vcov(m15b)))[2], sqrt(diag(vcov(m16b)))[2])
)

fx1 <- fx %>% filter(Variable %in%
                       c("Do favors","Watch property","Ask personal things","Talk about politics"))

fx2 <- fx %>% filter(!(Variable %in%
                         c("Do favors","Watch property","Ask personal things","Talk about politics")))
fx2$Variable <- factor(fx2$Variable, levels = fx2$Variable[order(fx2$Effect)])

coefs <- ggplot(fx1)
coefs <- coefs + geom_hline(yintercept = 0, color = gray(1/2), lty = 2)
coefs <- coefs + geom_linerange(aes(x = Variable, ymin = Effect - SE*interval95, ymax = Effect + SE*interval95), 
                                lwd = 1, position = position_dodge(width = 1/2))
coefs <- coefs + geom_pointrange(aes(x = Variable, y = Effect, ymin = Effect - SE*interval95, ymax = Effect + SE*interval95),
                                 lwd = 1/2, position = position_dodge(width = 1/2), shape = 21, fill = "BLACK")
coefs <- coefs + coord_flip() + theme_classic()
coefs <- coefs + ylab("Treatment Effect") + xlab("Social Polarization Measure")
coefs

coefs2 <- ggplot(fx2)
coefs2 <- coefs2 + geom_hline(yintercept = 0, color = gray(1/2), lty = 2)
coefs2 <- coefs2 + geom_linerange(aes(x = Variable, ymin = Effect - SE*interval95, ymax = Effect + SE*interval95), 
                                  lwd = 1, position = position_dodge(width = 1/2))
coefs2 <- coefs2 + geom_pointrange(aes(x = Variable, y = Effect, ymin = Effect - SE*interval95, ymax = Effect + SE*interval95),
                                   lwd = 1/2, position = position_dodge(width = 1/2), shape = 21, fill = "BLACK")
coefs2 <- coefs2 + coord_flip() + theme_classic()
coefs2 <- coefs2 + ylab("Treatment Effect") + xlab("Social Polarization Measure")
coefs2

# Instrumental variables analysis
# read in LIWC data
liwc <- read.csv("data/liwc_social.csv")
liwc_dat <- left_join(dat, liwc, by = c("ResponseId"="Source..J."))

# IV regressions
m1b_iv <- ivreg(fourpack_1_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m2b_iv <- ivreg(fourpack_2_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m3b_iv <- ivreg(fourpack_3_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m4b_iv <- ivreg(fourpack_4_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m5b_iv <- ivreg(talking_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m6b_iv <- ivreg(meal_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m7b_iv <- ivreg(club_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m8b_iv <- ivreg(date_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m9b_iv <- ivreg(family_date_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m10b_iv <- ivreg(friendship_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m11b_iv <- ivreg(family_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m12b_iv <- ivreg(divorce_gop_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m13b_iv <- ivreg(friends_comfort_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m14b_iv <- ivreg(neighbors_comfort_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m15b_iv <- ivreg(marry_upset_01 ~ anger + democrat | democrat + treated, data = liwc_dat)
m16b_iv <- ivreg(news_share_01 ~ anger + democrat | democrat + treated, data = liwc_dat)

mods1 <- list(m1b_iv,m2b_iv,m3b_iv,m4b_iv,m5b_iv,m6b_iv,m7b_iv,m8b_iv)
mods2 <- list(m9b_iv,m10b_iv,m11b_iv,m12b_iv,m13b_iv,m14b_iv,m15b_iv,m16b_iv)

stargazer(mods1,
          type="latex",
          style="apsr",
          covariate.labels = c("Predicted % angry words","Democrat"),
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = "n")

stargazer(mods2,
          type="latex",
          style="apsr",
          covariate.labels = c("Predicted % angry words","Democrat"),
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = "n")


#### 3. Appendix tables and figures ####
# Balance stats
# democrats
x_dem1 <- with(subset(dat, democrat == 1), t.test(age ~ treated))
x_dem2 <- with(subset(dat, democrat == 1), t.test(female ~ treated))
x_dem3 <- with(subset(dat, democrat == 1), t.test(ba_plus ~ treated))
x_dem4 <- with(subset(dat, democrat == 1), t.test(ideology ~ treated))
x_dem5 <- with(subset(dat, democrat == 1), t.test(nonwhite ~ treated))
# republicans
x_gop1 <- with(subset(dat, democrat == 0), t.test(age ~ treated))
x_gop2 <- with(subset(dat, democrat == 0), t.test(female ~ treated))
x_gop3 <- with(subset(dat, democrat == 0), t.test(ba_plus ~ treated))
x_gop4 <- with(subset(dat, democrat == 0), t.test(ideology ~ treated))
x_gop5 <- with(subset(dat, democrat == 0), t.test(nonwhite ~ treated))

balance <- data.frame(
  Party =  c(rep("Democrat", 10), rep("Republican", 10)),
  Variable = rep(c("Age","Female","BA Degree+","Ideology","Non-White"), each = 2),
  Treated = rep(c("Yes","No"), 10),
  Value = c(x_dem1$estimate[2], x_dem1$estimate[1],
            x_dem2$estimate[2], x_dem2$estimate[1],
            x_dem3$estimate[2], x_dem3$estimate[1],
            x_dem4$estimate[2], x_dem4$estimate[1],
            x_dem5$estimate[2], x_dem5$estimate[1],
            x_gop1$estimate[2], x_gop1$estimate[1],
            x_gop2$estimate[2], x_gop2$estimate[1],
            x_gop3$estimate[2], x_gop3$estimate[1],
            x_gop4$estimate[2], x_gop4$estimate[1],
            x_gop5$estimate[2], x_gop5$estimate[1]))

balance <- balance %>%
  group_by(Party, Variable) %>%
  mutate(Difference = Value[Treated == "No"] - Value[Treated == "Yes"]) %>%
  ungroup() %>%
  mutate(SE = rep(c(x_dem1$stderr, x_dem2$stderr,
                    x_dem3$stderr, x_dem4$stderr, 
                    x_dem5$stderr, x_gop1$stderr,
                    x_gop2$stderr, x_gop3$stderr,
                    x_gop4$stderr, x_gop5$stderr),
                  each = 2))

interval95 <- -qnorm((1-0.95)/2)
p1 <- ggplot(balance, aes(color = Party))
p1 <- p1 + geom_hline(yintercept = 0, color = gray(1/2), lty = 2)
p1 <- p1 + geom_linerange(aes(x = Variable, ymin = Difference - SE*interval95, ymax = Difference + SE*interval95), 
                          lwd = 1, position = position_dodge(width = 1/2))
p1 <- p1 + geom_pointrange(aes(x = Variable, y = Difference, ymin = Difference - SE*interval95, ymax = Difference + SE*interval95),
                           lwd = 1/2, position = position_dodge(width = 1/2), shape = 21)
p1 <- p1 + coord_flip() + theme_classic()
p1 <- p1 + scale_color_manual(values = c("black","grey"))
p1 <- p1 + theme(legend.position = "bottom")
p1

# Summary statistics
sstats <- dat %>% select(pid, female, race_eth, educ, ideology, age, selfmonitoring)
sstats2 <- sstats %>%
  mutate(White = ifelse(race_eth == "White, non-Hispanic", 1, 0),
         Black = ifelse(race_eth == "Black, non-Hispanic", 1, 0),
         Asian = ifelse(race_eth == "Asian, native Hawaiian, or other Pacific Islander", 1, 0),
         NativeAmerican = ifelse(race_eth == "Native American", 1, 0),
         Hispanic = ifelse(race_eth == "Hispanic", 1, 0),
         OtherRace = ifelse(race_eth == "Other", 1, 0),
         HighSchoolOnly = ifelse(educ == "High school graduate or G.E.D.", 1, 0),
         SomeCollege = ifelse(educ == "Some college but no degree", 1, 0),
         BachelorsDegree = ifelse(educ == "Bachelor's degree", 1, 0),
         Male = ifelse(female == 0, 1, 0),
         Female = ifelse(female == 1, 1, 0),
         Democrat = ifelse(pid < 4, 1, 0),
         Republican = ifelse(pid > 4, 1, 0),
         Liberal = ifelse(ideology < 4, 1, 0),
         Conservative = ifelse(ideology > 4, 1, 0)) %>%
  select(White, Black, Asian, NativeAmerican, Hispanic, OtherRace,
         HighSchoolOnly, SomeCollege, BachelorsDegree, Male, Female, Democrat, 
         Republican, Liberal, Conservative)

stargazer(sstats2,
          type="latex",
          style="apsr",
          title="Summary Statistics of Data",
          #notes="This table displays summary statistics for the data used in the analyses in this paper.",
          #font.size="footnotesize",
          label="tab:summary_stats")

# Regressions (regular dv's)
m1 <- lm(fourpack_1 ~ treated + democrat, data = dat)
m2 <- lm(fourpack_2 ~ treated + democrat, data = dat)
m3 <- lm(fourpack_3 ~ treated + democrat, data = dat)
m4 <- lm(fourpack_4 ~ treated + democrat, data = dat)
m5 <- lm(talking ~ treated + democrat, data = dat)
m6 <- lm(meal ~ treated + democrat, data = dat)
m7 <- lm(club ~ treated + democrat, data = dat)
m8 <- lm(date ~ treated + democrat, data = dat)
m9 <- lm(family_date ~ treated + democrat, data = dat)
m10 <- lm(friendship ~ treated + democrat, data = dat)
m11 <- lm(family ~ treated + democrat, data = dat)
m12 <- lm(divorce_gop ~ treated + democrat, data = dat)
m13 <- lm(friends_comfort ~ treated + democrat, data = dat)
m14 <- lm(neighbors_comfort ~ treated + democrat, data = dat)
m15 <- lm(marry_upset ~ treated + democrat, data = dat)
m16 <- lm(news_share ~ treated + democrat, data = dat)

results1_not_standardized <- list(m1, m2, m3, m4, m5, m6, m7, m8)
results2_not_standardized <- list(m9, m10, m11, m12, m13, m14, m15, m16)

stargazer(results1_not_standardized,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Democrat"),
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(results2_not_standardized,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Democrat"),
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))


# Self-monitoring interactions
m1c <- lm(fourpack_1_01 ~ treated*selfmonitoring + democrat, data = dat)
m2c <- lm(fourpack_2_01 ~ treated*selfmonitoring + democrat, data = dat)
m3c <- lm(fourpack_3_01 ~ treated*selfmonitoring + democrat, data = dat)
m4c <- lm(fourpack_4_01 ~ treated*selfmonitoring + democrat, data = dat)
m5c <- lm(talking_01 ~ treated*selfmonitoring + democrat, data = dat)
m6c <- lm(meal_01 ~ treated*selfmonitoring + democrat, data = dat)
m7c <- lm(club_01 ~ treated*selfmonitoring + democrat, data = dat)
m8c <- lm(date_01 ~ treated*selfmonitoring + democrat, data = dat)
m9c <- lm(family_date_01 ~ treated*selfmonitoring + democrat, data = dat)
m10c <- lm(friendship_01 ~ treated*selfmonitoring + democrat, data = dat)
m11c <- lm(family_01 ~ treated*selfmonitoring + democrat, data = dat)
m12c <- lm(divorce_gop_01 ~ treated*selfmonitoring + democrat, data = dat)
m13c <- lm(friends_comfort_01 ~ treated*selfmonitoring + democrat, data = dat)
m14c <- lm(neighbors_comfort_01 ~ treated*selfmonitoring + democrat, data = dat)
m15c <- lm(marry_upset_01 ~ treated*selfmonitoring + democrat, data = dat)
m16c <- lm(news_share_01 ~ treated*selfmonitoring + democrat, data = dat)

sm_models <- list(m1c, m2c, m3c, m4c, m5c, m6c, m7c, m8c)
sm_models1 <- list(m9c, m10c, m11c, m12c, m13c, m14c, m15c, m16c)

stargazer(sm_models,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Self-Monitoring","Democrat","Treated X Self-Monitoring"),
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(sm_models1,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Self-Monitoring","Democrat","Treated X Self-Monitoring"),
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))


# CATE models
# regressions for democrats
m1bd <- lm(fourpack_1_01 ~ treated, data = subset(dat, democrat == 1))
m2bd <- lm(fourpack_2_01 ~ treated, data = subset(dat, democrat == 1))
m3bd <- lm(fourpack_3_01 ~ treated, data = subset(dat, democrat == 1))
m4bd <- lm(fourpack_4_01 ~ treated, data = subset(dat, democrat == 1))
m5bd <- lm(talking_01 ~ treated, data = subset(dat, democrat == 1))
m6bd <- lm(meal_01 ~ treated, data = subset(dat, democrat == 1))
m7bd <- lm(club_01 ~ treated, data = subset(dat, democrat == 1))
m8bd <- lm(date_01 ~ treated, data = subset(dat, democrat == 1))
m9bd <- lm(family_date_01 ~ treated, data = subset(dat, democrat == 1))
m10bd <- lm(friendship_01 ~ treated, data = subset(dat, democrat == 1))
m11bd <- lm(family_01 ~ treated, data = subset(dat, democrat == 1))
m12bd <- lm(divorce_gop_01 ~ treated, data = subset(dat, democrat == 1))
m13bd <- lm(friends_comfort_01 ~ treated, data = subset(dat, democrat == 1))
m14bd <- lm(neighbors_comfort_01 ~ treated, data = subset(dat, democrat == 1))
m15bd <- lm(marry_upset_01 ~ treated, data = subset(dat, democrat == 1))
m16bd <- lm(news_share_01 ~ treated, data = subset(dat, democrat == 1))

# regressions for republicans
m1br <- lm(fourpack_1_01 ~ treated, data = subset(dat, democrat == 0))
m2br <- lm(fourpack_2_01 ~ treated, data = subset(dat, democrat == 0))
m3br <- lm(fourpack_3_01 ~ treated, data = subset(dat, democrat == 0))
m4br <- lm(fourpack_4_01 ~ treated, data = subset(dat, democrat == 0))
m5br <- lm(talking_01 ~ treated, data = subset(dat, democrat == 0))
m6br <- lm(meal_01 ~ treated, data = subset(dat, democrat == 0))
m7br <- lm(club_01 ~ treated, data = subset(dat, democrat == 0))
m8br <- lm(date_01 ~ treated, data = subset(dat, democrat == 0))
m9br <- lm(family_date_01 ~ treated, data = subset(dat, democrat == 0))
m10br <- lm(friendship_01 ~ treated, data = subset(dat, democrat == 0))
m11br <- lm(family_01 ~ treated, data = subset(dat, democrat == 0))
m12br <- lm(divorce_gop_01 ~ treated, data = subset(dat, democrat == 0))
m13br <- lm(friends_comfort_01 ~ treated, data = subset(dat, democrat == 0))
m14br <- lm(neighbors_comfort_01 ~ treated, data = subset(dat, democrat == 0))
m15br <- lm(marry_upset_01 ~ treated, data = subset(dat, democrat == 0))
m16br <- lm(news_share_01 ~ treated, data = subset(dat, democrat == 0))

# export CATE models
mods_dems_cate1 <- list(m1bd,m2bd,m3bd,m4bd,m5bd,m6bd,m7bd,m8bd)
mods_dems_cate2 <- list(m9bd,m10bd,m11bd,m12bd,m13bd,m14bd,m15bd,m16bd)

mods_gop_cate1 <- list(m1br,m2br,m3br,m4br,m5br,m6br,m7br,m8br)
mods_gop_cate2 <- list(m9br,m10br,m11br,m12br,m13br,m14br,m15br,m16br)

stargazer(mods_dems_cate1,
          type="latex",
          style="apsr",
          covariate.labels = "Treated",
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(mods_dems_cate2,
          type="latex",
          style="apsr",
          covariate.labels = "Treated",
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(mods_gop_cate1,
          type="latex",
          style="apsr",
          covariate.labels = "Treated",
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(mods_gop_cate2,
          type="latex",
          style="apsr",
          covariate.labels = "Treated",
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

# ATEs by strength of partisanship
dat <- dat %>%
  mutate(strong_partisan = as.numeric(
    pid7 %in% c("Strong Democrat","Strong Republican")
  ))

m1b_strength <- lm(fourpack_1_01 ~ treated*strong_partisan + democrat, data = dat)
m2b_strength <- lm(fourpack_2_01 ~ treated*strong_partisan + democrat, data = dat)
m3b_strength <- lm(fourpack_3_01 ~ treated*strong_partisan + democrat, data = dat)
m4b_strength <- lm(fourpack_4_01 ~ treated*strong_partisan + democrat, data = dat)
m5b_strength <- lm(talking_01 ~ treated*strong_partisan + democrat, data = dat)
m6b_strength <- lm(meal_01 ~ treated*strong_partisan + democrat, data = dat)
m7b_strength <- lm(club_01 ~ treated*strong_partisan + democrat, data = dat)
m8b_strength <- lm(date_01 ~ treated*strong_partisan + democrat, data = dat)
m9b_strength <- lm(family_date_01 ~ treated*strong_partisan + democrat, data = dat)
m10b_strength <- lm(friendship_01 ~ treated*strong_partisan + democrat, data = dat)
m11b_strength <- lm(family_01 ~ treated*strong_partisan + democrat, data = dat)
m12b_strength <- lm(divorce_gop_01 ~ treated*strong_partisan + democrat, data = dat)
m13b_strength <- lm(friends_comfort_01 ~ treated*strong_partisan + democrat, data = dat)
m14b_strength <- lm(neighbors_comfort_01 ~ treated*strong_partisan + democrat, data = dat)
m15b_strength <- lm(marry_upset_01 ~ treated*strong_partisan + democrat, data = dat)
m16b_strength <- lm(news_share_01 ~ treated*strong_partisan + democrat, data = dat)

mods1 <- list(m1b_strength,m2b_strength,m3b_strength,m4b_strength,
              m5b_strength,m6b_strength,m7b_strength,m8b_strength)
mods2 <- list(m9b_strength,m10b_strength,m11b_strength,m12b_strength,
              m13b_strength,m14b_strength,m15b_strength,m16b_strength)

stargazer(mods1,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Strong Partisan","Democrat","Treated X Strong Partisan"),
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(mods2,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Strong Partisan","Democrat","Treated X Strong Partisan"),
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

# ATEs by ideological extremity
dat <- dat %>%
  mutate(ideological_extremity = abs(ideology - 4))

m1b_ideo <- lm(fourpack_1_01 ~ treated*ideological_extremity + democrat, data = dat)
m2b_ideo <- lm(fourpack_2_01 ~ treated*ideological_extremity + democrat, data = dat)
m3b_ideo <- lm(fourpack_3_01 ~ treated*ideological_extremity + democrat, data = dat)
m4b_ideo <- lm(fourpack_4_01 ~ treated*ideological_extremity + democrat, data = dat)
m5b_ideo <- lm(talking_01 ~ treated*ideological_extremity + democrat, data = dat)
m6b_ideo <- lm(meal_01 ~ treated*ideological_extremity + democrat, data = dat)
m7b_ideo <- lm(club_01 ~ treated*ideological_extremity + democrat, data = dat)
m8b_ideo <- lm(date_01 ~ treated*ideological_extremity + democrat, data = dat)
m9b_ideo <- lm(family_date_01 ~ treated*ideological_extremity + democrat, data = dat)
m10b_ideo <- lm(friendship_01 ~ treated*ideological_extremity + democrat, data = dat)
m11b_ideo <- lm(family_01 ~ treated*ideological_extremity + democrat, data = dat)
m12b_ideo <- lm(divorce_gop_01 ~ treated*ideological_extremity + democrat, data = dat)
m13b_ideo <- lm(friends_comfort_01 ~ treated*ideological_extremity + democrat, data = dat)
m14b_ideo <- lm(neighbors_comfort_01 ~ treated*ideological_extremity + democrat, data = dat)
m15b_ideo <- lm(marry_upset_01 ~ treated*ideological_extremity + democrat, data = dat)
m16b_ideo <- lm(news_share_01 ~ treated*ideological_extremity + democrat, data = dat)

mods1 <- list(m1b_ideo,m2b_ideo,m3b_ideo,m4b_ideo,
              m5b_ideo,m6b_ideo,m7b_ideo,m8b_ideo)
mods2 <- list(m9b_ideo,m10b_ideo,m11b_ideo,m12b_ideo,
              m13b_ideo,m14b_ideo,m15b_ideo,m16b_ideo)

stargazer(mods1,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Ideological Extremity","Democrat","Treated X Ideological Extremity"),
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(mods2,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Ideological Extremity","Democrat","Treated X Ideological Extremity"),
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

# ATEs by gender
m1b_gender <- lm(fourpack_1_01 ~ treated*female + democrat, data = dat)
m2b_gender <- lm(fourpack_2_01 ~ treated*female + democrat, data = dat)
m3b_gender <- lm(fourpack_3_01 ~ treated*female + democrat, data = dat)
m4b_gender <- lm(fourpack_4_01 ~ treated*female + democrat, data = dat)
m5b_gender <- lm(talking_01 ~ treated*female + democrat, data = dat)
m6b_gender <- lm(meal_01 ~ treated*female + democrat, data = dat)
m7b_gender <- lm(club_01 ~ treated*female + democrat, data = dat)
m8b_gender <- lm(date_01 ~ treated*female + democrat, data = dat)
m9b_gender <- lm(family_date_01 ~ treated*female + democrat, data = dat)
m10b_gender <- lm(friendship_01 ~ treated*female + democrat, data = dat)
m11b_gender <- lm(family_01 ~ treated*female + democrat, data = dat)
m12b_gender <- lm(divorce_gop_01 ~ treated*female + democrat, data = dat)
m13b_gender <- lm(friends_comfort_01 ~ treated*female + democrat, data = dat)
m14b_gender <- lm(neighbors_comfort_01 ~ treated*female + democrat, data = dat)
m15b_gender <- lm(marry_upset_01 ~ treated*female + democrat, data = dat)
m16b_gender <- lm(news_share_01 ~ treated*female + democrat, data = dat)

mods1 <- list(m1b_gender,m2b_gender,m3b_gender,m4b_gender,
              m5b_gender,m6b_gender,m7b_gender,m8b_gender)
mods2 <- list(m9b_gender,m10b_gender,m11b_gender,m12b_gender,
              m13b_gender,m14b_gender,m15b_gender,m16b_gender)

stargazer(mods1,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Female","Democrat","Treated X Female"),
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(mods2,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Female","Democrat","Treated X Female"),
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

# ATEs by race
m1b_nonwhite <- lm(fourpack_1_01 ~ treated*nonwhite + democrat, data = dat)
m2b_nonwhite <- lm(fourpack_2_01 ~ treated*nonwhite + democrat, data = dat)
m3b_nonwhite <- lm(fourpack_3_01 ~ treated*nonwhite + democrat, data = dat)
m4b_nonwhite <- lm(fourpack_4_01 ~ treated*nonwhite + democrat, data = dat)
m5b_nonwhite <- lm(talking_01 ~ treated*nonwhite + democrat, data = dat)
m6b_nonwhite <- lm(meal_01 ~ treated*nonwhite + democrat, data = dat)
m7b_nonwhite <- lm(club_01 ~ treated*nonwhite + democrat, data = dat)
m8b_nonwhite <- lm(date_01 ~ treated*nonwhite + democrat, data = dat)
m9b_nonwhite <- lm(family_date_01 ~ treated*nonwhite + democrat, data = dat)
m10b_nonwhite <- lm(friendship_01 ~ treated*nonwhite + democrat, data = dat)
m11b_nonwhite <- lm(family_01 ~ treated*nonwhite + democrat, data = dat)
m12b_nonwhite <- lm(divorce_gop_01 ~ treated*nonwhite + democrat, data = dat)
m13b_nonwhite <- lm(friends_comfort_01 ~ treated*nonwhite + democrat, data = dat)
m14b_nonwhite <- lm(neighbors_comfort_01 ~ treated*nonwhite + democrat, data = dat)
m15b_nonwhite <- lm(marry_upset_01 ~ treated*nonwhite + democrat, data = dat)
m16b_nonwhite <- lm(news_share_01 ~ treated*nonwhite + democrat, data = dat)

mods1 <- list(m1b_nonwhite,m2b_nonwhite,m3b_nonwhite,m4b_nonwhite,
              m5b_nonwhite,m6b_nonwhite,m7b_nonwhite,m8b_nonwhite)
mods2 <- list(m9b_nonwhite,m10b_nonwhite,m11b_nonwhite,m12b_nonwhite,
              m13b_nonwhite,m14b_nonwhite,m15b_nonwhite,m16b_nonwhite)

stargazer(mods1,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Non-white","Democrat","Treated X Non-white"),
          dep.var.labels = c("Do favors","Watch property","Ask personal items","Talk politics",
                             "Talk at party","Coffee/meal/drink","Gathering","First date"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))

stargazer(mods2,
          type="latex",
          style="apsr",
          covariate.labels = c("Treated","Non-white","Democrat","Treated X Non-white"),
          dep.var.labels = c("Family date","Friend out-partisan","Family out-partisan","Friend divorce",
                             "Friends (comfort)", "Neighbors (comfort)","Marriage","Fake news"),
          model.numbers = FALSE,
          keep.stat = c("n","rsq"))


# Average F-Statistic
f <- c(801.92,800.17,802.53,803.25,754.62,802.91,803.65,804.9,803.67,801.05,
       804.41,804.14,804.64,801.49,801.88,803.90)
mean(f)
