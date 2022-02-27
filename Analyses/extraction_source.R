library(haven)
library(here)
library(tidyverse)

## data is extracted in R script to be sourced in the results.Rmd for meta-analyses
## and data_extraction.Rmd for explanations where it was taken from and focal tests

###############################################
#### within-subject correspondence studies ####
###############################################


## extract data to calculate marginal tables for matched-pairs odds ratios
## includes exposed controls only, exposed experimental only, both exposed, neither exposed

## Ahmed et al. (2012)
ahmed_12 <- data.frame(id = "Ahmed et al. (2012)",
                       control_yes = 34,
                       forty_yes = 5,
                       both = 8,
                       neither = 419) 

## Jansons & Zukov (2012)
jansons_12 <- data.frame(id = "Jansons & Zukov (2012)",
                         control_yes = 19,
                         fifty_yes = 5,
                         both = 7,
                         neither = 215) 


#################################################
#### betweeen-subject correspondence studies ####
#################################################


## Capeau et al. (2012)

capeau_raw <- readxl::read_xlsx(here("Input", "sample_resumes.xlsx"), 
                                #first sheet containing full data info
                                #skipping first row without proper variable names
                                sheet = 1, range = "A2:M60")

capeau12 <- capeau_raw %>% #select(c(2,3,4,6,8,10)) %>% 
          #choose relevant variables
          select("ethnicity" = 'Origin/Type', age, 
                 "older_yes" = 'type invited /other is not invited',
                 "both" = '...6',
                 "neither" ='...8',
                 "control_yes" = 'type not invited / other invited') %>%  
          group_by(age) %>% 
          #add callbacks for each age group
          summarise_at(c("older_yes", "both", "neither", "control_yes"), sum) %>% 
          #filter out ages 23 and 35(this is 35 vs. 35, not control vs. older)
          filter(age != 23 & age != 35) %>% 
          #create separate column for forty and fifty year olds invited
          mutate(forty_yes = if_else(age == 47, older_yes, 0),
                 fifty_yes = if_else(age == 53, older_yes, 0)) %>% 
          #remove age and older_yes columns and add id column
          select(-c(age, older_yes)) %>% add_column(id = "Capéau et al. (2012)", .before = TRUE)

## Carlsson & Eriksson (2019) 

differentiated_callbacks <- readxl::read_xls(here("Input", "Carlsson_Eriksson.xls")) 

carlsson_eriksson19 <- differentiated_callbacks %>% 
  #group_by(age, callback) %>% 
  #group_by(age, interview) %>% 
  group_by(age, callback, interview) %>% 
  # count number of callbacks per group
  summarise(call_n = n()) %>%
  #ungroup for the rest of wrangling
  ungroup() %>% 
  #keep only control age and experimental age (36-39 was not our PICOS)
  filter(age == 35 | age >= 40) %>% 
  #create age categories, breaks start with one age prior to category
  mutate(age_group = cut(age, breaks = c(34, 39, 49, 59, 65, 70),
                         labels = c("control", "forty", "fifty", "sixty", "above_sixtyfive"), 
                         #order them correctly
                         ordered_result = TRUE)) %>%
  #group again to summarize
  group_by(age_group, callback, interview) %>% 
  summarise_if(is.numeric, sum) %>% 
  ungroup() %>% 
  #add whether they were invited or not to the age category based on
  #variable coding in the original dataset
  mutate(age_group = case_when(
    callback == 0 & interview == 0 ~ paste0(age_group,"_no"),
    callback == 1 & interview == 0 ~ paste0(age_group,"_maybe"),
    TRUE ~ paste0(age_group,"_yes"),
  )) %>% 
  #filter out maybe, keep only certain callbacks
  filter(!grepl("_maybe", age_group)) %>%  
  #only choose the age categories and number of callbacks
  select(age_group, call_n) %>% 
  #create a dataframe suitable for calculating effect sizes using escalc
  pivot_wider(names_from = age_group, values_from = call_n) %>% 
  add_column(id = c("Carlsson & Eriksson (2019)"), .before = TRUE) 


## Farber et al. (2019) 

farber_19 <- tibble(id = "Farber et al. (2019)",
                    age_group = c("control.prop_yes", "control.prop_no", 
                                  "forty.prop_yes", "forty.prop_no",
                                  "fifty.prop_yes", "fifty.prop_no",
                                  "sixty.prop_yes", "sixty.prop_no"),
                    rates = c(0.095, (1 - 0.095), 0.089, (1-0.089), 
                              0.080, (1-0.080), 0.075, (1-0.075)),
                    n = c(1460, 1460, 1368, 1368, 1439, 1439, 1345, 1345)) %>% 
  #calculate callback rates by multiplying callback proportion by the n of applications
  mutate(frequencies = round(rates * n)) %>% 
  select(age_group, frequencies) %>% 
  #clean up variable names
  mutate(age_group = stringr::str_remove(age_group, "\\.prop")) %>% 
  #create a dataframe suitable for calculating effect sizes using escalc
  pivot_wider(names_from = "age_group", values_from = "frequencies") %>%
  add_column(id = "Farber et al. (2019)", .before = TRUE)


## Neumark et al. (2016) Experimental Age

neumark16_raw <- read_dta(here("Input", "mergefinal.dta"))


neumark16 <- neumark16_raw %>% 
  # select age, positiveresponse(type of callback), and 1-0 callback variable
  select(positiveresponse, age, callback) %>% 
  #create age categories to fit our PICOS
  mutate(age_group = case_when(
    age == 29 | age == 30 | age == 31 ~ "control",
    age == 49 ~ "forty",
    age == 50 | age == 51 ~ "fifty",
    age == 64 | age == 65 ~ "sixty",
    TRUE ~ "above_sixtyfive"
  )) %>% 
  #filter out missing values
  filter(#age_group != "29", #filter out 29 year-olds from control
    !is.na(callback),
    positiveresponse != "maybe") %>% #filter out positive answers, keep only sure callbacks
  group_by(age_group) %>% 
  count(callback) %>% 
  #categorize age groups based on being invited or not
  mutate(age_group = case_when(
    callback == 0 ~ paste0(age_group, "_no"),
    TRUE ~ paste0(age_group, "_yes") 
  )) %>% 
  select(age_group, n) %>% 
  #create a dataframe suitable for calculating effect sizes using escalc
  pivot_wider(names_from = age_group, values_from = n) %>% 
  add_column(id = "Neumark et al. (2016)", .before = TRUE)



## Neumark et al. (2019) Do State Laws 

neumark19_raw <- read_dta(here("Input", "FinalData50States.dta"))

neumark19_state <- neumark19_raw %>% 
  select(positiveresponse, age, callback) %>% 
  mutate(age_group = case_when(
    age == 29 | age == 30 | age == 31 ~ "control",
    age == 64 | age == 65 ~ "sixty",
    TRUE ~ "above_sixtyfive"
  )) %>% 
  filter(#age_group != "29", #filter out 29 year-olds from control
    positiveresponse != "maybe") %>% #filter out positive answers, keep only sure callbacks
  group_by(age_group) %>% 
  count(callback) %>% 
  mutate(age_group = case_when(
    callback == 0 ~ paste0(age_group, "_no"),
    TRUE ~ paste0(age_group, "_yes") 
  )) %>% 
  select(age_group, n) %>% 
  pivot_wider(names_from = age_group, values_from = n) %>% 
  add_column(id = "Neumark et al. (2019)", .before = TRUE) 



###############################################
##### Within-subject scenario experiments #####
###############################################

##Montizaan & Fourage - missing raw data, can't extract


################################################
##### Between-subject scenario experiments #####
################################################

##Richardson (2013) - could not be extracted 

##### Oesch (2020) #####

oesch_raw <- read_dta(here("Input", "1141_LIVES-JOBVUL_Data_STATA_v1.0.0.dta"))

# v_age = vignette age applicant (1=35;2=40;3=45;4=50;5=55)
# rate = likelihood of being hired 1-10
oesch <- oesch_raw %>% 
  select(v_age, rate) %>% 
  # -1 is their coding for NAs, so mutate to NA
  mutate(rate = na_if(rate, '-1'),
         v_age = na_if(v_age, '-1')
  ) %>%
  # filter out missing data
  filter(!is.na(v_age), !is.na('rate')) %>% 
  mutate(v_age = case_when(
    # age 35 (1)
    v_age == 1 ~ "control",
    # age 40 (2) and 45 (3)
    v_age == 2 | v_age == 3 ~ "forty",
    TRUE ~ "fifty") 
  ) %>% 
  group_by(v_age) %>%
  # summarise
  summarise(#median_likelihood = median(rate, na.rm = TRUE),
            mean_like = mean(rate, na.rm = TRUE),
            sd_like = sd(rate, na.rm = TRUE),
            n_like = n()) %>% 
  # change data table format to calculate effect sizes
  pivot_wider(names_from = v_age, values_from = c(mean_like, sd_like, n_like))



##################################
#### Meta-analysis dataframes ####
##################################

ws_cs <- bind_rows(ahmed_12, capeau12, jansons_12)

bs_cs <- bind_rows(carlsson_eriksson19, farber_19, neumark16, neumark19_state)

ma_cs_all <- bind_rows(ws_cs, bs_cs)







