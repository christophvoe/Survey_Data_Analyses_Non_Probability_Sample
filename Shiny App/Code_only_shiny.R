require(shiny)
require(tidyverse)
require(ggplot2)
require(RColorBrewer)
require(scales)
require(MASS)
require(class)
require(ISLR)
require(tree)
require(survey)
require(ggpubr)

# Data prep

comix.data <- read.csv("data/CoMix_uk_contact_common.csv")
comix.data2 <- read.csv("data/CoMix_uk_contact_extra.csv")
comix.data3 <- read.csv("data/CoMix_uk_hh_common.csv")
comix.data4 <- read.csv("data/CoMix_uk_hh_extra.csv")
comix.data5 <- read.csv("data/CoMix_uk_participant_common.csv")
comix.data6 <- read.csv("data/CoMix_uk_participant_extra.csv")
comix.data7 <- read.csv("data/CoMix_uk_sday.csv")

clean_cont <- merge(comix.data, comix.data7, by=("part_id"))
clean_cont <- clean_cont %>%
  mutate(contact = 1) %>%
  group_by(part_id) %>%
  mutate(avg_nr_contacts = sum(contact, na.rm = TRUE))


# Dataset for participants. With Age categories like the population data
clean_part <- merge(clean_cont, comix.data5, by=("part_id"))
clean_part <- clean_part %>%
  distinct(part_id, .keep_all = TRUE) %>%
  mutate(part_age_group = cut(part_age,
                              breaks = c(0, 4, 11, 17, 29, 39, 49, 59, 69, 120),
                              labels = c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70-120"))) %>%
  dplyr::select(part_id, hh_id, part_age_group, part_age, part_gender, sday_id, wave, avg_nr_contacts)

# Average number of contacts per day
clean_part <- clean_part %>%
  group_by(sday_id) %>%
  mutate(MEAN_number_of_contacts_per_day = mean(avg_nr_contacts)) %>%
  mutate(number_of_contacts_per_day = sum(avg_nr_contacts))

clean_part <- clean_part %>%
  group_by(sday_id, part_age_group, part_gender) %>%
  mutate(number_of_contacts_per_day_STRATA = sum(avg_nr_contacts)) %>%
  mutate(MEAN_number_of_contacts_per_day_STRATA = mean(avg_nr_contacts))

load("UKpopulation.Rdata")

pop <- filter(pop, sample_type == "adult", !part_gender == "other")
pop <- pop %>%
  mutate(pop_proportionNEW = round(pop_estimate/sum(pop_estimate), 4))

pop$part_gender[pop$part_gender=="female"]<-"F"
pop$part_gender[pop$part_gender=="male"]<-"M"



samplesizes <- as.data.frame(table(clean_part$part_gender, clean_part$part_age_group))

samplesizes <- samplesizes %>%
  mutate(samp_proportion = round(Freq/sum(Freq), 4))


samplesizes <- setNames(samplesizes,c("part_gender","part_age_group","Freq",
                                      "samp_proportion"))



# Something like this is needed for the weights I would say
prop <- merge(samplesizes, pop, by.x=c("part_gender", "part_age_group"))
prop <- prop %>%
  mutate(weights = pop_proportionNEW/samp_proportion) %>%
  dplyr::select(weights, part_age_group, part_gender, Freq, pop_estimate, samp_proportion, pop_proportionNEW)


## Part 3: Getting data on covid cases in the UK


library(covidregionaldata)
library(socialmixr)
cases <- get_national_data(countries = "UK")
## Part 4: Get data on government measures against COVID


stringency.index <- read.csv("covid-stringency-index.csv")
stringency.index <- subset(stringency.index, stringency.index$Code == "GBR")
stringency.index$sday_id <- as.Date(stringency.index$Day)
# Part 5: Merge and clean the data


# merge all information into one dataset
complete.data <- merge(clean_part, prop, by=c("part_age_group","part_gender"))
complete.data <- complete.data %>%
  mutate(date = as.character(as.Date(sday_id, "%Y.%m.%d"),"%Y-%m-%d"))
complete.data <- subset(complete.data, 
                        select=c("part_id","part_age_group","part_gender","date",
                                 "wave","MEAN_number_of_contacts_per_day",
                                 "number_of_contacts_per_day",
                                 "MEAN_number_of_contacts_per_day_STRATA",
                                 "number_of_contacts_per_day_STRATA","weights",
                                 "pop_proportionNEW"))
complete.data$mean_contacts_wgt <- complete.data$weights*
  complete.data$MEAN_number_of_contacts_per_day
complete.data$mean_contacts_wgt_strata <- complete.data$weights*
  complete.data$MEAN_number_of_contacts_per_day_STRATA

# merge all information into one dataset
cases2 <- subset(cases, select=c("date","cases_new"))
cases2$date <- as.character(as.Date(cases2$date))
complete.data <- merge(complete.data, cases2, by=c("date"))

# merge all information into one dataset
stringency.index <- read.csv("covid-stringency-index.csv")
stringency.index <- subset(stringency.index, stringency.index$Code == "GBR")
stringency.index$sday_id <- as.Date(stringency.index$Day)
stringency.index$date <- as.character(as.Date(stringency.index$Day))
stringency2 <- subset(stringency.index, select=c("date","stringency_index"))
complete.data <- merge(complete.data, stringency2, by=c("date"))
complete.data2 <- subset(complete.data, 
                         select=c(date, part_gender,part_age_group,
                                  MEAN_number_of_contacts_per_day_STRATA))
complete.data2 <- complete.data2[!duplicated(complete.data2[c(1,2,3,4)]), ]
complete.data2 <- complete.data2 %>%
  mutate(id = row_number())

# get the postratification weights by gender x age group
library(survey)
svy.unweighted <-svydesign(ids=~1, data=complete.data2)

prop <- prop %>%
  group_by(part_gender, part_age_group) %>%
  slice(rep(1:n(), each = pop_proportionNEW*10000))

genderage.dist <-xtabs(~part_gender+part_age_group, data=prop)

svy.poststrat <- postStratify(design=svy.unweighted, 
                              strata=~part_gender+part_age_group,
                              population=genderage.dist)

# this will take some time
data.strata.wgt <- svyby(~MEAN_number_of_contacts_per_day_STRATA,
                         ~part_age_group+part_gender+date,
                         design=svy.poststrat, svymean)

# compute the mean number of contacts per day of each strata (i.e. gender x age group)
# unweighted as well as weighted
data.strata <- complete.data %>%
  pivot_wider(names_from = c(part_gender, part_age_group), 
              values_from = c(MEAN_number_of_contacts_per_day_STRATA))

data.strata <- data.strata %>% 
  group_by(date) %>% 
  mutate_at(c("F_18-29","F_30-39","F_40-49","F_50-59","F_60-69","F_70-120",
              "M_18-29","M_30-39","M_40-49","M_50-59","M_60-69","M_70-120"), 
            function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

data.strata.wgt <- data.strata.wgt %>%
  pivot_wider(names_from = c(part_gender, part_age_group), 
              values_from = c(MEAN_number_of_contacts_per_day_STRATA))

data.strata.wgt <- data.strata.wgt %>% 
  group_by(date) %>% 
  mutate_at(c("F_18-29","F_30-39","F_40-49","F_50-59","F_60-69","F_70-120",
              "M_18-29","M_30-39","M_40-49","M_50-59","M_60-69","M_70-120"), 
            function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)))

# replace the resulting NaN by NA
data.strata <- data.strata[!duplicated(data.strata$date), ]
data.strata <- data.strata %>% 
  mutate_all(function(x) ifelse(is.nan(x), NA, x))

data.strata.wgt <- data.strata.wgt[!duplicated(data.strata.wgt$date), ]
data.strata.wgt <- data.strata.wgt %>% 
  mutate_all(function(x) ifelse(is.nan(x), NA, x))

data.strata <- data.strata %>%
  rename_with(~ tolower(gsub("-", "_", .x, fixed = TRUE))) %>%
  rename(c(F_18_29 = f_18_29,
           F_30_39 = f_30_39,
           F_40_49 = f_40_49,
           F_50_59 = f_50_59,
           F_60_69 = f_60_69,
           F_70_120 = f_70_120,
           M_18_29 = m_18_29,
           M_30_39 = m_30_39,
           M_40_49 = m_40_49,
           M_50_59 = m_50_59,
           M_60_69 = m_60_69,
           M_70_120 = m_70_120))

# create weighted average contacts per stratum
data.strata.wgt <- data.strata.wgt %>%
  rename_with(~ tolower(gsub("-", "_", .x, fixed = TRUE))) %>%
  rename(c(F_18_29_wgt = f_18_29,
           F_30_39_wgt = f_30_39,
           F_40_49_wgt = f_40_49,
           F_50_59_wgt = f_50_59,
           F_60_69_wgt = f_60_69,
           F_70_120_wgt = f_70_120,
           M_18_29_wgt = m_18_29,
           M_30_39_wgt = m_30_39,
           M_40_49_wgt = m_40_49,
           M_50_59_wgt = m_50_59,
           M_60_69_wgt = m_60_69,
           M_70_120_wgt = m_70_120))

# combine the data sets
data.complete <- merge(data.strata, data.strata.wgt, by=c("date"))

# I noticed that the plots didn't look very nice with the dates on the x-axis and that # some days had missing values. I, # thus, aggregated the mean over the calendar weeks # and procceded by plotting the data over the weeks
data.complete <- data.complete %>%
  mutate(weeks = case_when(
    date >= "2020-03-23" & date <= "2020-03-29" ~ 13,
    date >= "2020-03-30" & date <= "2020-04-05" ~ 14,
    date >= "2020-04-06" & date <= "2020-04-12" ~ 15,
    date >= "2020-04-13" & date <= "2020-04-19" ~ 16,
    date >= "2020-04-20" & date <= "2020-04-26" ~ 17,
    date >= "2020-04-27" & date <= "2020-05-03" ~ 18,
    date >= "2020-05-04" & date <= "2020-05-10" ~ 19,
    date >= "2020-05-11" & date <= "2020-05-17" ~ 20,
    date >= "2020-05-18" & date <= "2020-05-24" ~ 21,
    date >= "2020-05-25" & date <= "2020-05-31" ~ 22,
    date >= "2020-06-01" & date <= "2020-06-07" ~ 23,
    date >= "2020-06-08" & date <= "2020-06-14" ~ 24,
    date >= "2020-06-15" & date <= "2020-06-21" ~ 25,
    date >= "2020-06-22" & date <= "2020-06-28" ~ 26,
    date >= "2020-06-29" & date <= "2020-07-05" ~ 27,
    date >= "2020-07-06" & date <= "2020-07-12" ~ 28,
    date >= "2020-07-13" & date <= "2020-07-19" ~ 29,
    date >= "2020-07-20" & date <= "2020-07-26" ~ 30,
    date >= "2020-07-27" & date <= "2020-08-02" ~ 31,
    date >= "2020-08-03" ~ 32
  ))

data.weeks <- data.complete %>%
  group_by(weeks) %>%
  mutate_all(function(x) mean(x, na.rm=TRUE))
data.weeks <- data.weeks[!duplicated(data.weeks$weeks), ]
data.weeks <- subset(data.weeks, 
                     select=c(weeks,cases_new,stringency_index,13:24,26:37))