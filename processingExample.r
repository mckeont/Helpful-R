#####################################################
##### Processing TEDS-A data
March 6th 2023 Cannabis data 2008-2020 #######
#####################################################

#Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)

##Read in State FIPS codes with corresponding state names 
stfips<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/stfips.csv", header = TRUE)
## This is needed because TEDS-A data doesn't use statenames. This table will be used later as a reference to rename stfips to statenames.


# Load the population data Census tables: 
data <- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/populationestimates2000_2021.csv", header = TRUE)

# Estimates of the Resident Population by Single Year of Age and Sex for States and the United States 
# Table names: 2010 to 2019 data: sc-est2019-agesex-civ and 2000 to 2009 data: st-est00int-agesex 


### Read in TEDS data downloaded July 2020 
## Read file, if .tsv use read.table, 2008,2009,2010,2011,2012
teds2008 <- read.table(file = 'C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2008.tsv', sep = '\t', header = TRUE)
teds2009 <- read.table(file = 'C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2010.tsv', sep = '\t', header = TRUE)
teds2011 <- read.table(file = 'C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2009.tsv', sep = '\t', header = TRUE)
teds2010 <- read.table(file = 'C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2011.tsv', sep = '\t', header = TRUE)
teds2012 <- read.table(file = 'C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2012.tsv', sep = '\t', header = TRUE)
## read.csv for 2013,2014,2015,2016,2017
teds2013<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2013.csv", header = TRUE)
teds2014<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2014.csv", header = TRUE)
teds2015<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2015.csv", header = TRUE)
teds2016<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2016.csv", header = TRUE)
teds2017<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2017.csv", header = TRUE)

## TEDS-A data downloaded March 2023
teds2018<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2018.csv", header = TRUE)
teds2019<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2019.csv", header = TRUE)
teds2020<- read.csv("C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS2020.csv", header = TRUE)

# TEDS 2017, 2018 and 2019 has the YEAR column labeled as ADMYR. Column name changed to YEAR to be consistent with other years.
teds2017 <- teds2017 %>% rename(YEAR = ADMYR)
teds2018 <- teds2018 %>% rename(YEAR = ADMYR)
teds2019 <- teds2019 %>% rename(YEAR = ADMYR)
teds2020 <- teds2020 %>% rename(YEAR = ADMYR)


#Combine 2008 to 2015 TEDS years into a list of dataframes 
TEDS2008_2015 <-list()
for (j in 2008:2015){
  sTEDS <- paste("teds", j, sep="")
  dTEDS <- get(paste("teds", j, sep=""))
  TEDS2008_2015[[sTEDS]] <-dTEDS
}

# Subset with selected columns: STFIPS = State FIPS, YEAR = Year, AGE = Age,
# Sub1 = Primary Substance, NOPRIOR = No Prior admissions status, 
# PSOURCE = Primary Source of referral)   THIS IS USING THE 2020 CODES FOR AGE. Note if you download these datasets again the AGE codes will be different

# For Adolescents
h <- vector("list", length(TEDS2008_2015))
for(i in 1:length(TEDS2008_2015)){
  h[[i]] <- subset(TEDS2008_2015[[i]], AGE ==2 | AGE ==3, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
}
TEDSA2008_2015_adolescent <- do.call(rbind, h)


# For Young Adults
k <- vector("list", length(TEDS2008_2015))
for(i in 1:length(TEDS2008_2015)){
  k[[i]] <- subset(TEDS2008_2015[[i]], AGE ==4, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
}
TEDSA2008_2015_youngadult <- do.call(rbind, k)

# For Legal Young Adults
k <- vector("list", length(TEDS2008_2015))
for(i in 1:length(TEDS2008_2015)){
  k[[i]] <- subset(TEDS2008_2015[[i]], AGE ==5, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
}
TEDSA2008_2015_legalyoungadult <- do.call(rbind, k)



#Combine 2016 2017 2018 2019 and 2020 TEDS years into a list of dataframes 
TEDS2016_2020 <-list()
for (j in 2016:2020){
  sTEDS <- paste("teds", j, sep="")
  dTEDS <- get(paste("teds", j, sep=""))
  TEDS2016_2020[[sTEDS]] <-dTEDS
}

##Subset with new age categories.
# For Adolescents
h <- vector("list", length(TEDS2016_2020))
for(i in 1:length(TEDS2016_2020)){
  h[[i]] <- subset(TEDS2016_2020[[i]], AGE ==1 | AGE ==2, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
}
TEDSA2016_2020_adolescent <- do.call(rbind, h)


# For Young Adults
k <- vector("list", length(TEDS2016_2020))
for(i in 1:length(TEDS2016_2020)){
  k[[i]] <- subset(TEDS2016_2020[[i]], AGE ==3, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
}
TEDSA2016_2020_youngadult <- do.call(rbind, k)

# For Legal Young Adults
k <- vector("list", length(TEDS2016_2020))
for(i in 1:length(TEDS2016_2020)){
  k[[i]] <- subset(TEDS2016_2020[[i]], AGE ==4, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
}
TEDSA2016_2020_legalyoungadult <- do.call(rbind, k)



#Combine all together for adolescents and young adults
TEDSA2008_2020_adolescent <- rbind(TEDSA2008_2015_adolescent, TEDSA2016_2020_adolescent)
TEDSA2008_2020_youngadult <- rbind(TEDSA2008_2015_youngadult, TEDSA2016_2020_youngadult)
TEDSA2008_2020_legalyoungadult <- rbind(TEDSA2008_2015_legalyoungadult, TEDSA2016_2020_legalyoungadult)

#Subset cannabis as primary SUD #Recode Criminal Referrals  #Recode NOPRIORs

## For Legal Young Adults
TEDSA2008_2020_legalyoungadult <-subset(TEDSA2008_2020_legalyoungadult, SUB1 == 4, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
TEDSA2008_2020_legalyoungadult$PSOURCErecoded <- ifelse(TEDSA2008_2020_legalyoungadult$PSOURCE == 7 , 1, 0)
TEDSA2008_2020_legalyoungadult$NOPRIORrecoded <- ifelse(TEDSA2008_2020_legalyoungadult$NOPRIOR == 0 , 0, 1)

## For Young Adults
TEDSA2008_2020_youngadult <-subset(TEDSA2008_2020_youngadult, SUB1 == 4, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
TEDSA2008_2020_youngadult$PSOURCErecoded <- ifelse(TEDSA2008_2020_youngadult$PSOURCE == 7 , 1, 0)
TEDSA2008_2020_youngadult$NOPRIORrecoded <- ifelse(TEDSA2008_2020_youngadult$NOPRIOR == 0 , 0, 1)

## For Adolescents
TEDSA2008_2020_adolescent <-subset(TEDSA2008_2020_adolescent, SUB1 == 4, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"), all = TRUE)
TEDSA2008_2020_adolescent$PSOURCErecoded <- ifelse(TEDSA2008_2020_adolescent$PSOURCE == 7 , 1, 0)
TEDSA2008_2020_adolescent$NOPRIORrecoded <- ifelse(TEDSA2008_2020_adolescent$NOPRIOR == 0 , 0, 1)


#calculate population estimates by age groups. SEX = 0 corresponds to both sexes. 

pop2020 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2020_CIV))

pop2020 <- pop2020 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2020 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2020_wide <- pivot_wider(pop2020, names_from = age_range, values_from = pop2020)

# add prefix to column names
pop2020_wide <- pop2020_wide %>% 
  rename_with(~ paste0("pop2020_", .x), -c("STATE", "SEX"))

# print result
pop2020_wide
#########################

pop2019 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2019_CIV))

pop2019 <- pop2019 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2019 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2019_wide <- pivot_wider(pop2019, names_from = age_range, values_from = pop2019)

# add prefix to column names
pop2019_wide <- pop2019_wide %>% 
  rename_with(~ paste0("pop2019_", .x), -c("STATE", "SEX"))

# print result
pop2019_wide
##########################

pop2018 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2018_CIV))

pop2018 <- pop2018 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2018 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2018_wide <- pivot_wider(pop2018, names_from = age_range, values_from = pop2018)

# add prefix to column names
pop2018_wide <- pop2018_wide %>% 
  rename_with(~ paste0("pop2018_", .x), -c("STATE", "SEX"))

# print result
pop2018_wide
#########################

pop2017 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2017_CIV))

pop2017 <- pop2017 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2017 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2017_wide <- pivot_wider(pop2017, names_from = age_range, values_from = pop2017)

# add prefix to column names
pop2017_wide <- pop2017_wide %>% 
  rename_with(~ paste0("pop2017_", .x), -c("STATE", "SEX"))

# print result
pop2017_wide
#########################

pop2016 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2016_CIV))

pop2016 <- pop2016 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2016 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2016_wide <- pivot_wider(pop2016, names_from = age_range, values_from = pop2016)

# add prefix to column names
pop2016_wide <- pop2016_wide %>% 
  rename_with(~ paste0("pop2016_", .x), -c("STATE", "SEX"))

# print result
pop2016_wide
#########################


pop2015 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2015_CIV))

pop2015 <- pop2015 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2015 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2015_wide <- pivot_wider(pop2015, names_from = age_range, values_from = pop2015)

# add prefix to column names
pop2015_wide <- pop2015_wide %>% 
  rename_with(~ paste0("pop2015_", .x), -c("STATE", "SEX"))

# print result
pop2015_wide
#########################


pop2014 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2014_CIV))

pop2014 <- pop2014 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2014 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2014_wide <- pivot_wider(pop2014, names_from = age_range, values_from = pop2014)

# add prefix to column names
pop2014_wide <- pop2014_wide %>% 
  rename_with(~ paste0("pop2014_", .x), -c("STATE", "SEX"))

# print result
pop2014_wide
#########################


pop2013 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2013_CIV))

pop2013 <- pop2013 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2013 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2013_wide <- pivot_wider(pop2013, names_from = age_range, values_from = pop2013)

# add prefix to column names
pop2013_wide <- pop2013_wide %>% 
  rename_with(~ paste0("pop2013_", .x), -c("STATE", "SEX"))

# print result
pop2013_wide
#########################


pop2012 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2012_CIV))

pop2012 <- pop2012 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2012 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2012_wide <- pivot_wider(pop2012, names_from = age_range, values_from = pop2012)

# add prefix to column names
pop2012_wide <- pop2012_wide %>% 
  rename_with(~ paste0("pop2012_", .x), -c("STATE", "SEX"))

# print result
pop2012_wide
#########################

pop2011 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2011_CIV))

pop2011 <- pop2011 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2011 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2011_wide <- pivot_wider(pop2011, names_from = age_range, values_from = pop2011)

# add prefix to column names
pop2011_wide <- pop2011_wide %>% 
  rename_with(~ paste0("pop2011_", .x), -c("STATE", "SEX"))

# print result
pop2011_wide
#########################

pop2010 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPEST2010_CIV))

pop2010 <- pop2010 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2010 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2010_wide <- pivot_wider(pop2010, names_from = age_range, values_from = pop2010)

# add prefix to column names
pop2010_wide <- pop2010_wide %>% 
  rename_with(~ paste0("pop2010_", .x), -c("STATE", "SEX"))

# print result
pop2010_wide
#########################

pop2009 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPESTIMATE2009))

pop2009 <- pop2009 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2009 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2009_wide <- pivot_wider(pop2009, names_from = age_range, values_from = pop2009)

# add prefix to column names
pop2009_wide <- pop2009_wide %>% 
  rename_with(~ paste0("pop2009_", .x), -c("STATE", "SEX"))

# print result
pop2009_wide

#########################


pop2008 <- data %>%
  filter(AGE >= 12 & AGE <= 24, SEX <= 0) %>%  # Filter to only include ages 12 to 17 and sex 0
  group_by(STATE, SEX, AGE) %>%
  summarize(TOTAL = sum(POPESTIMATE2008))

pop2008 <- pop2008 %>% 
  mutate(age_range = cut(AGE, breaks = c(12, 18, 21, 25), right = FALSE,
                         labels = c("12-17", "18-20", "21-24"))) %>%
  group_by(STATE, SEX, age_range) %>% 
  summarise(pop2008 = sum(TOTAL)) %>% 
  ungroup() %>% 
  arrange(STATE, SEX, age_range)

# pivot to wide format
pop2008_wide <- pivot_wider(pop2008, names_from = age_range, values_from = pop2008)

# add prefix to column names
pop2008_wide <- pop2008_wide %>% 
  rename_with(~ paste0("pop2008_", .x), -c("STATE", "SEX"))

# print result
pop2008_wide
#########################



# Combine all the dataframes into a list
pop_list <- list(pop2008_wide, pop2009_wide, pop2010_wide, pop2011_wide, pop2012_wide, pop2013_wide, 
                 pop2014_wide, pop2015_wide, pop2016_wide, pop2017_wide, pop2018_wide, pop2019_wide, pop2020_wide)

# Merge all dataframes iteratively based on common columns
population_agegroups <- reduce(pop_list, full_join, by = c("STATE", "SEX"))

## rename STATE to stfips
population_agegroups <- population_agegroups %>% 
  rename(stfips = STATE)

## drop USA, DC and puerto Rico
population_agegroups <- population_agegroups %>% 
  filter(!stfips %in% c(0, 11, 72))

## Remove the sex column
population_agegroups <- population_agegroups %>% 
  select(-SEX)

## Remove tallies for populations outside this study's age ranges. 
population_agegroups <- population_agegroups %>%
  select(-contains("NA"))


# merge stfips dataset so we can rename.
merged_df <- merge(population_agegroups, stfips, by = 'stfips')
# 
# # Rename the 'stfips' column with 'statename'
# # merged_df <- subset(merged_df, select = -c(stfips))
# colnames(merged_df)[colnames(merged_df) == 'statename.x'] <- 'statename'
# 
# # select columns to reshape
# pop_cols <- c(paste0("pop", 2008:2020, "_", c("12-17", "18-20", "21-24")), "statename", "stfips")
# 
# # reshape to long format
# pop_long <- merged_df %>%
#   select(pop_cols) %>%
#   pivot_longer(cols = starts_with("pop"), names_to = "year_age_range", values_to = "population") %>%
#   separate(year_age_range, into = c("YEAR", "age_range"), sep = "_") %>%
#   arrange(statename, YEAR, age_range)
# 
# 
# pop_long$YEAR <- sub("pop", "", pop_long$YEAR)
# 
# # print result
# pop_long



#Tally Legal Young adults
tally_LYA_tally <- TEDSA2008_2020_legalyoungadult %>% group_by(STFIPS,YEAR) %>%  tally()
sumall <- sum(tally_LYA_tally$n)

### Tally No Priors for Legal Adults 
mj_legaladult_noprior <-subset(TEDSA2008_2020_legalyoungadult, NOPRIORrecoded == 0 | is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
tally_LYA_tallynp <- mj_legaladult_noprior %>% group_by(STFIPS,YEAR) %>% tally()
sumallnp <- sum(tally_LYA_tallynp$n)

## Tally No Prior and No criminal referral for Legal Adults 
mj_legaladult_noprio_nocrim <-subset(TEDSA2008_2020_legalyoungadult, NOPRIORrecoded == 0 & PSOURCErecoded == 0| is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
tally_LYA_tallynpnc <- mj_legaladult_noprio_nocrim  %>% group_by(STFIPS,YEAR) %>% tally()
sumallnpnc <- sum(tally_LYA_tallynpnc$n)

## Tally No Prior and criminal referral for Legal Adults 
mj_legaladult_noprio_crim <-subset(TEDSA2008_2020_legalyoungadult, NOPRIORrecoded == 0 & PSOURCErecoded == 1| is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
tally_LYA_tallynpc <- mj_legaladult_noprio_crim  %>% group_by(STFIPS,YEAR) %>% tally()
sumallnpc <- sum(tally_LYA_tallynpc$n)

## Tally criminal referral for Legal Adults 
mj_legaladult_crim <-subset(TEDSA2008_2020_legalyoungadult, PSOURCErecoded == 1, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
tally_LYA_tallyc <- mj_legaladult_crim  %>% group_by(STFIPS,YEAR) %>% tally()
sumallc <- sum(tally_LYA_tallyc$n)

#Tally Young adults
tally_YA_tally <- TEDSA2008_2020_youngadult %>% group_by(STFIPS,YEAR) %>%  tally()
sumall <- sum(tally_YA_tally$n)

### Tally No Priors for Adults 
mj_adult_noprior <-subset(TEDSA2008_2020_youngadult, NOPRIORrecoded == 0 | is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
tally_YA_tallynp <- mj_adult_noprior %>% group_by(STFIPS,YEAR) %>% tally()
sumallnp <- sum(tally_YA_tallynp$n)

## Tally No Prior and No criminal referral for Adults 
mj_adult_noprio_nocrim <-subset(TEDSA2008_2020_youngadult, NOPRIORrecoded == 0 & PSOURCErecoded == 0| is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
tally_YA_tallynpnc <- mj_adult_noprio_nocrim  %>% group_by(STFIPS,YEAR) %>% tally()
sumallnpnc <- sum(tally_YA_tallynpnc$n)

## Tally No Prior and criminal referral for Adults 
mj_adult_noprio_crim <-subset(TEDSA2008_2020_youngadult, NOPRIORrecoded == 0 & PSOURCErecoded == 1| is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
tally_YA_tallynpc <- mj_adult_noprio_crim  %>% group_by(STFIPS,YEAR) %>% tally()
sumallnpc <- sum(tally_YA_tallynpc$n)

## Tally No Prior and criminal referral for Adults 
mj_adult_crim <-subset(TEDSA2008_2020_youngadult, PSOURCErecoded == 1, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
tally_YA_tallyc <- mj_adult_crim  %>% group_by(STFIPS,YEAR) %>% tally()
sumallc <- sum(tally_YA_tallyc$n)

# Tally Adolescents
tally_adolescent_tally <- TEDSA2008_2020_adolescent %>% group_by(STFIPS, YEAR) %>% tally()
sumall_ad <- sum(tally_adolescent_tally$n)

### Tally No prior adolescent
adolescent_mjnoprior <-subset(TEDSA2008_2020_adolescent, NOPRIORrecoded == 0 | is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
adolescent_mjnopriortally <- adolescent_mjnoprior %>%  group_by(STFIPS, YEAR) %>% tally()
sumall_adnp <- sum(adolescent_mjnopriortally$n)

###Tally  No prior adolescent and No criminal referral for adolescent
adolescent_mjnoprior_nocrim <-subset(TEDSA2008_2020_adolescent, NOPRIORrecoded == 0 & PSOURCErecoded == 0| is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
adolescent_mjnopriortally_nocrim <- adolescent_mjnoprior_nocrim %>%  group_by(STFIPS, YEAR) %>% tally()
sumall_adnpnc <- sum(adolescent_mjnopriortally_nocrim$n)

###Tally  No prior adolescent and criminal referral for adolescent
adolescent_mjnoprior_crim <-subset(TEDSA2008_2020_adolescent, NOPRIORrecoded == 0 & PSOURCErecoded == 1| is.na(NOPRIORrecoded), select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
adolescent_mjnopriortally_crim <- adolescent_mjnoprior_crim %>%  group_by(STFIPS, YEAR) %>% tally()
sumall_adnpc <- sum(adolescent_mjnopriortally_crim$n)

###Tally criminal referral for adolescent
adolescent_mj_crim <-subset(TEDSA2008_2020_adolescent, PSOURCErecoded == 1, select = c("STFIPS", "YEAR", "AGE", "SUB1", "NOPRIOR", "PSOURCE"))
adolescent_mjtally_crim <- adolescent_mj_crim %>%  group_by(STFIPS, YEAR) %>% tally()
sumall_adc <- sum(adolescent_mjtally_crim$n)



## merge state fips with TEDS data #Filter out D.C.

## For Legal Young Adults
LegalYoungAdult2008_2020_stfips <- merge(tally_LYA_tally, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
LegalYoungAdult2008_2020_stfips <- filter(LegalYoungAdult2008_2020_stfips, statename != "District of Columbia")

## For Legal Young Adults with No Priors 
NoPrior_LegalYoungAdult2008_2020_stfips <- merge(tally_LYA_tallynp, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_LegalYoungAdult2008_2020_stfips <- filter(NoPrior_LegalYoungAdult2008_2020_stfips, statename != "District of Columbia")

## For Legal Young Adults with No Priors and No Criminal Referral 
NoPrior_nocrim_LegalYoungAdult2008_2020_stfips <- merge(tally_LYA_tallynpnc, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_nocrim_LegalYoungAdult2008_2020_stfips <- filter(NoPrior_nocrim_LegalYoungAdult2008_2020_stfips, statename != "District of Columbia")

## For Legal Young Adults with No Priors and Criminal Referral 
NoPrior_crim_LegalYoungAdult2008_2020_stfips <- merge(tally_LYA_tallynpc, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_crim_LegalYoungAdult2008_2020_stfips <- filter(NoPrior_crim_LegalYoungAdult2008_2020_stfips, statename != "District of Columbia")

## For Legal Young Adults and Criminal Referral 
crim_LegalYoungAdult2008_2020_stfips <- merge(tally_LYA_tallyc, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
crim_LegalYoungAdult2008_2020_stfips <- filter(crim_LegalYoungAdult2008_2020_stfips, statename != "District of Columbia")



## For Young Adults
YoungAdult2008_2020_stfips <- merge(tally_YA_tally, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
YoungAdult2008_2020_stfips <- filter(YoungAdult2008_2020_stfips, statename != "District of Columbia")

## For Young Adults with No Priors 
NoPrior_YoungAdult2008_2020_stfips <- merge(tally_YA_tallynp, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_YoungAdult2008_2020_stfips <- filter(NoPrior_YoungAdult2008_2020_stfips, statename != "District of Columbia")

## For Young Adults with No Priors and No Criminal Referral 
NoPrior_nocrim_YoungAdult2008_2020_stfips <- merge(tally_YA_tallynpnc, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_nocrim_YoungAdult2008_2020_stfips <- filter(NoPrior_nocrim_YoungAdult2008_2020_stfips, statename != "District of Columbia")

## For Young Adults with No Priors and Criminal Referral 
NoPrior_crim_YoungAdult2008_2020_stfips <- merge(tally_YA_tallynpc, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_crim_YoungAdult2008_2020_stfips <- filter(NoPrior_crim_YoungAdult2008_2020_stfips, statename != "District of Columbia")

## For Young Adults with No Priors and Criminal Referral 
crim_YoungAdult2008_2020_stfips <- merge(tally_YA_tallyc, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
crim_YoungAdult2008_2020_stfips <- filter(crim_YoungAdult2008_2020_stfips, statename != "District of Columbia")



## For Adolescents
Adolescent2008_2020_stfips <- merge(tally_adolescent_tally, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
Adolescent2008_2020_stfips <- filter(Adolescent2008_2020_stfips, statename != "District of Columbia")

## For Adolescents with No Priors 
NoPrior_Adolescent2008_2020_stfips <- merge(adolescent_mjnopriortally, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_Adolescent2008_2020_stfips <- filter(NoPrior_Adolescent2008_2020_stfips, statename != "District of Columbia")

## For Adolescents with No Priors and No Criminal Referral 
NoPrior_nocrim_Adolescent2008_2020_stfips <- merge(adolescent_mjnopriortally_nocrim, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_nocrim_Adolescent2008_2020_stfips <- filter(NoPrior_nocrim_Adolescent2008_2020_stfips, statename != "District of Columbia")

## For Adolescents with No Priors and No Criminal Referral 
NoPrior_crim_Adolescent2008_2020_stfips <- merge(adolescent_mjnopriortally_crim, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
NoPrior_crim_Adolescent2008_2020_stfips <- filter(NoPrior_crim_Adolescent2008_2020_stfips, statename != "District of Columbia")
## For Adolescents with No Priors and No Criminal Referral 
crim_Adolescent2008_2020_stfips <- merge(adolescent_mjtally_crim, stfips, by.x="STFIPS", by.y="stfips", all = TRUE)
crim_Adolescent2008_2020_stfips <- filter(crim_Adolescent2008_2020_stfips, statename != "District of Columbia")



#rename tally count field
LegalYoungAdult2008_2020_stfips <- LegalYoungAdult2008_2020_stfips %>% rename(Noy = n)
crim_LegalYoungAdult2008_2020_stfips <- crim_LegalYoungAdult2008_2020_stfips %>% rename (Noycj = n)


YoungAdult2008_2020_stfips <- YoungAdult2008_2020_stfips %>% rename(Nyy = n)
crim_YoungAdult2008_2020_stfips <- crim_YoungAdult2008_2020_stfips %>% rename (Nyycj = n)

Adolescent2008_2020_stfips <- Adolescent2008_2020_stfips  %>% rename(Nad = n)
crim_Adolescent2008_2020_stfips <- crim_Adolescent2008_2020_stfips %>% rename (Nadcj = n)


a <- Adolescent2008_2020_stfips
b <- YoungAdult2008_2020_stfips 
c <- LegalYoungAdult2008_2020_stfips
d <- crim_Adolescent2008_2020_stfips 
e <- crim_YoungAdult2008_2020_stfips
f <- crim_LegalYoungAdult2008_2020_stfips


a$m <- as.character(paste(a$STFIPS,a$YEAR,a$statename))
b$m <- as.character(paste(b$STFIPS,b$YEAR,b$statename))
c$m <- as.character(paste(c$STFIPS,c$YEAR,c$statename))
d$m <- as.character(paste(d$STFIPS,d$YEAR,d$statename))
e$m <- as.character(paste(e$STFIPS,e$YEAR,e$statename))
f$m <- as.character(paste(f$STFIPS,f$YEAR,f$statename))




merged <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                 list(a, b, c, d, e, f))
merged[complete.cases(merged), ]

merged <- subset( merged, select = -m )

# merged$age_range <- paste(merged$age_range, "yrs", sep = "")



write.csv (merged,
           file = "C:/Users/ureka/OneDrive/Documents/Cannabis2023/TEDS_2008_2020-march2023.csv",
           row.names = FALSE)

write.csv (merged_df,
           file = "C:/Users/ureka/OneDrive/Documents/Cannabis2023/populationestimates_byagegroup.csv",
           row.names = FALSE)





#Covariates for later....
#merger <- merge(merged, population, by=c("statename","YEAR"))
#merger <- merge(merger, nsduh_ya, by=c("statename","YEAR"))
#merger <- merge(merger, NSDUH2008_2019_adolescent, by=c("statename","YEAR"))

### Calculate TxNeed
#  e = TotalYoungAdult / X18_24_pop
#  f = TotalNoPriorYoungAdult	/ X18_24_pop
#  g = TotalNoPriorNocrimYoungAdult	/ X18_24_pop
#  h = TotalAdolescent / X12_17_pop	
#  i = TotalNoPriorAdolescent / X12_17_pop
#  j = TotalNoPriorNoCrimAdolescent / X12_17_pop

## Use
# Tx_e = e / adult_mj_use
# Tx_f = f / adult_mj_use
# Tx_g = g / adult_mj_use
# Tx_h = h / ado_mj_use
# Tx_i = i / ado_mj_use
# Tx_j = j / ado_mj_use

# ##Percent Admitted
# merger$PercentYoungAdultAdmitted <- merger$TotalYoungAdult / merger$X18_24_pop
# merger$PercentNoPriorYoungAdultAdmitted <- merger$TotalNoPriorYoungAdult / merger$X18_24_pop
# merger$PercentNoPriorNocrimYoungAdultAdmitted <- merger$TotalNoPriorNocrimYoungAdult / merger$X18_24_pop
# merger$PercentAdolescentAdmitted <- merger$TotalAdolescent / merger$X12_17_pop
# merger$PercentNoPriorAdolescentAdmitted <- merger$TotalNoPriorAdolescent / merger$X12_17_pop
# merger$PercentNoPriorNoCrimAdolescentAdmitted <- merger$TotalNoPriorNoCrimAdolescent / merger$X12_17_pop
# 
# ##TxNeed
# merger$TxNeed_YA <- merger$PercentYoungAdultAdmitted / merger$adult_mj_use
# merger$TxNeed_npYA <- merger$PercentNoPriorYoungAdultAdmitted / merger$adult_mj_use
# merger$TxNeed_npYAnc <- merger$PercentNoPriorNocrimYoungAdultAdmitted / merger$adult_mj_use
# 
# merger$TxNeed_Ado <- merger$PercentAdolescentAdmitted / merger$X12_17_pop
# merger$TxNeed_npAdo <- merger$PercentNoPriorAdolescentAdmitted / merger$X12_17_pop
# merger$TxNeed_npAdonc <- merger$PercentNoPriorNoCrimAdolescentAdmitted / merger$X12_17_pop
# 
# ##TxNeed Squared
# merger$TxNeed_YA_sqrt <- sqrt(merger$TxNeed_YA)
# merger$TxNeed_npYA_sqrt <- sqrt(merger$TxNeed_npYA)
# merger$TxNeed_npYAnc_sqrt <- sqrt(merger$TxNeed_npYAnc)
# 
# merger$TxNeed_Ado_sqrt <- sqrt(merger$TxNeed_Ado)
# merger$TxNeed_npAdo_sqrt <- sqrt(merger$TxNeed_npAdo)
# merger$TxNeed_npAdonc_sqrt <- sqrt(merger$TxNeed_npAdonc)
# 
# mj <- merger
