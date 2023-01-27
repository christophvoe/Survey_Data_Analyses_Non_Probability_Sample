library(readr)
library(survey)
library(tidyverse)
library(covidregionaldata)
library(socialmixr)
library(ggplot2)
library(data.table)

complete.data <- read_csv("Data/Data_processed/full_data.csv")
load("Data/uk_pop.Rdata")
clean_cont <-read_csv("Data/Data_processed/clean_cont.csv")
clean_part <-read_csv("Data/Data_processed/clean_part.csv")

source("Functions/contact_matrices.R")
source("Functions/plotting_post_strat.R")
source("Functions/plotting_with_Covid.R")

# switching some var names for plots
complete.data <-complete.data %>%
  mutate(part_gender = case_when(part_gender == "M" ~ "Male",
                                 part_gender == "F" ~ "Female"),
         hh_type = case_when(hh_type == 1 ~ "Not alone",
                             hh_type == 0 ~ "Alone"),
         senior = case_when(senior == 1 ~ "Senior",
                            senior == 0 ~ "No senior")
  )

# define survey design without weights
svy.unweighted <- svydesign(ids=~part_id, data=complete.data)

# get the postratification weights by age, gender, and household type
uk_pop <- uk_pop %>%
  mutate(part_gender = case_when(part_gender == "M" ~ "Male",
                                 part_gender == "F" ~ "Female"),
         hh_type = case_when(hh_type == 1 ~ "Not alone",
                             hh_type == 0 ~ "Alone")) %>%
  group_by(part_gender, part_age_group, hh_type) %>%
  slice(rep(1:n(), each = pop_estimate))
# create contingency table
pop_data.dist <- xtabs(~part_gender+part_age_group+hh_type, data=uk_pop)
# define post-stratification survey design
svy.poststrat <- postStratify(design=svy.unweighted, 
                              strata=~part_gender+part_age_group+hh_type,
                              population=pop_data.dist)

glm.fit <- svyglm(contacts ~ part_age_group + part_gender + factor(hh_type) + date, design=svy.poststrat)
glm.gov <- svyglm(contacts ~ stringency_index + cases_new + date, design=svy.poststrat)
glm.age <- svyglm(contacts ~ part_age_group + date, design=svy.poststrat)
glm.gender <- svyglm(contacts ~ part_gender + date, design=svy.poststrat)
glm.hhtype <- svyglm(contacts ~ factor(hh_type) + date, design=svy.poststrat)
glm.location <- svyglm(contacts ~ factor(location) + date, design=svy.poststrat)
glm.periods <- svyglm(contacts ~ factor(periods) + date, design=svy.poststrat)
glm.senior.gen <- svyglm(contacts ~ factor(senior)*part_gender + date, design=svy.poststrat)
glm.senior.hh <- svyglm(contacts ~ factor(senior)*factor(hh_type) + date, design=svy.poststrat)

# contacts (with and w/o poststratification)
data.wgt <- svyby(~contacts, ~weeks, design=svy.poststrat, svymean)
data.wgt <- data.wgt %>%
  mutate(wgt = 0)

data.non.wgt <- svyby(~contacts, ~weeks, design=svy.unweighted, svymean)
data.non.wgt <- data.non.wgt %>%
  mutate(wgt = 1)

data.weeks <- rbind(data.wgt, data.non.wgt)

# contacts by age group (with and w/o poststratification)
data.age.wgt <- svyby(~contacts, ~part_age_group+weeks, design=svy.poststrat, svymean)
data.age.wgt <- data.age.wgt %>%
  mutate(wgt = 0)

data.age.non.wgt <- svyby(~contacts, ~part_age_group+weeks, design=svy.unweighted, svymean)
data.age.non.wgt <- data.age.non.wgt %>%
  mutate(wgt = 1)

data.age.weeks <- rbind(data.age.wgt, data.age.non.wgt)

# contacts by gender (with and w/o poststratification)
data.gender.wgt <- svyby(~contacts, ~part_gender+weeks,design=svy.poststrat, svymean)
data.gender.wgt <- data.gender.wgt %>%
  mutate(wgt = 0)

data.gender.non.wgt <- svyby(~contacts, ~part_gender+weeks, design=svy.unweighted, svymean)
data.gender.non.wgt <- data.gender.non.wgt %>%
  mutate(wgt = 1)

data.gender.weeks <- rbind(data.gender.wgt, data.gender.non.wgt)

# contacts by household type (with and w/o poststratification)
data.hhtype.wgt <- svyby(~contacts, ~hh_type+weeks, design=svy.poststrat, svymean)
data.hhtype.wgt <- data.hhtype.wgt %>%
  mutate(wgt = 0)

data.hhtype.non.wgt <- svyby(~contacts, ~hh_type+weeks, design=svy.unweighted, svymean)
data.hhtype.non.wgt <- data.hhtype.non.wgt %>%
  mutate(wgt = 1)

data.hhtype.weeks <- rbind(data.hhtype.wgt, data.hhtype.non.wgt)

# contacts of 65 year olds and over by gender (with and w/o poststratification)
data.seniors.wgt <- svyby(~contacts, ~senior+part_gender+weeks, design=svy.poststrat, svymean)
data.seniors.wgt <- data.seniors.wgt %>%
  mutate(wgt = 0)

data.seniors.non.wgt <- svyby(~contacts, ~senior+part_gender+weeks, design=svy.unweighted, svymean)
data.seniors.non.wgt <- data.seniors.non.wgt %>%
  mutate(wgt = 1)

data.seniors.weeks <- rbind(data.seniors.wgt, data.seniors.non.wgt)

# contacts of 65 year olds and over by household type (with and w/o 
# poststratification)
data.seniors.hh.wgt <- svyby(~contacts, ~senior+hh_type+weeks, design=svy.poststrat, svymean)
data.seniors.hh.wgt <- data.seniors.hh.wgt %>%
  mutate(wgt = 0)

data.seniors.hh.non.wgt <- svyby(~contacts, ~senior+hh_type+weeks, design=svy.unweighted, svymean)
data.seniors.hh.non.wgt <- data.seniors.hh.non.wgt %>%
  mutate(wgt = 1)

data.seniors.hh <- rbind(data.seniors.hh.wgt, data.seniors.hh.non.wgt)

# cases per week
cases.weeks <- subset(complete.data, select=c(weeks, cases_new))
cases.weeks <- cases.weeks %>%
  group_by(weeks) %>%
  mutate(cases = mean(cases_new)) %>%
  distinct(weeks, .keep_all=TRUE)

p1 <- plotting_post(data.weeks,data.weeks$wgt,"Figure 1: Contacts over Time")
p2 <- plotting_post(data.age.weeks,data.age.weeks$part_age_group,"Figure 3: Contacts by Age Group")
p3 <- plotting_post(data.gender.weeks,data.gender.weeks$part_gender,"Figure 5: Contacts by Gender")
p4 <- plotting_post(data.hhtype.weeks,data.hhtype.weeks$hh_type,"Figure 7: Contacts by Household Typ")
p5 <- plotting_post(data.seniors.weeks,interaction(factor(data.seniors.weeks$senior), data.seniors.weeks$part_gender),"Figure 9: Contacts by Age Group and Gender")
p6 <- plotting_post(data.seniors.hh,interaction(factor(data.seniors.hh$senior), data.seniors.hh$hh_type),"Figure 11: Contacts by Age Group and Household Type")


p7 <- plotting_covid(data.weeks,data.weeks$wgt,"Figure 2: Contacts over Time")
p8 <- plotting_covid(data.age.weeks,data.age.weeks$part_age_group,"Figure 4: Contacts by Age Group")
p9 <- plotting_covid(data.gender.weeks,data.gender.weeks$part_gender,"Figure 6: Contacts by Gender")
p10 <- plotting_covid(data.hhtype.weeks,data.hhtype.weeks$hh_type,"Figure 8: Contacts by Household Typ")
p11 <- plotting_covid(data.seniors.weeks,interaction(factor(data.seniors.weeks$senior), data.seniors.weeks$part_gender),"Figure 10: Contacts by Age Group and Gender")
p12 <- plotting_covid(data.seniors.hh,interaction(factor(data.seniors.hh$senior), data.seniors.hh$hh_type),"Figure 12: Contacts by Age Group and Household Type")


# Plot over whole time Comix data
comixdata <- survey(complete.data, clean_cont)
contactcomix <- contact_matrix(comixdata, age.limits = c(18, 24, 44, 64, 120))
# Initial plot POLYMOD 
contactUK <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(18, 24, 44, 64, 120))

# Matrices - contacts per day (Type of contact)
# Only physical contact for Comix data
phys_contactcomix <- clean_cont %>%
  filter(phys_contact == 1)
phys_comixdata <- survey(complete.data, phys_contactcomix)
phys_contactcomix <- contact_matrix(phys_comixdata,age.limits = c(18, 24, 44, 64, 120))

# Only physical contact for Polymod data
phys_contactpoly <- polymod$contacts %>%
  filter(phys_contact == 1)
phys_poly <- survey(polymod$participants, phys_contactpoly)
phys_contactUK <- contact_matrix(phys_poly, countries = "United Kingdom", age.limits = c(18, 24, 44, 64, 120))

# Data prep - Matrices - contacts per day (Time periods)
Period_1 <- complete.data %>%
  filter(sday_id >= "2020.03.23" & sday_id <= "2020.06.03")

Period_2 <- complete.data %>%
  filter(sday_id >= "2020.06.24" & sday_id <= "2020.07.29")

Period_3 <- complete.data %>%
  filter(sday_id >= "2020.07.30" & sday_id <= "2020.08.08")

# Period 1
period1_comixdata <- survey(Period_1, clean_cont)
period1_contactcomix <- contact_matrix(period1_comixdata, age.limits = c(18, 24, 44, 64, 120))

# Period 2
period2_comixdata <- survey(Period_2, clean_cont)
period2_contactcomix <- contact_matrix(period2_comixdata, age.limits = c(18, 24, 44, 64, 120))

#Period 3
period3_comixdata <- survey(Period_3, clean_cont)
period3_contactcomix <- contact_matrix(period3_comixdata, age.limits = c(18, 24, 44, 64, 120))

gplot1 <- contactmatrices(contactcomix, "Social contact CoMix")
gplot2 <- contactmatrices(contactUK, "Social contact POLYMOD")
compare_plot <- cowplot::plot_grid(gplot1,gplot2, labels = "AUTO")

gplot3 <- contactmatrices(phys_contactcomix, "Physical contact CoMix")
gplot4 <- contactmatrices(phys_contactUK, "Physical contact POLYMOD")
compare_plot1 <- cowplot::plot_grid(gplot3,gplot4, gplot1, gplot2, labels = "AUTO")

gplot5 <- contactmatrices(period1_contactcomix, "Lockdown Social contact CoMix")
gplot6 <- contactmatrices(period2_contactcomix, "Lockdown easing Social contact CoMix")
gplot7 <- contactmatrices(period3_contactcomix, "Reduced restrictions Social contact CoMix")

compare_plot2 <- cowplot::plot_grid(gplot2, gplot5,gplot6,gplot7, labels = "AUTO")








