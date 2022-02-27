library(haven)
library(here)
library(tidyverse)

## NOTE: the data extraction used for this is not explicitly the same as the one 
## used for the effect size calculations (data used in MAs is coded in the extraction_source.R)


##### Baert coding for age experiments #####
lippens2021 <- read_delim(here("Input", "age_lippens.csv"), delim=';', n_max=6)

#################################################
##### Within-subject correspondence studies #####
#################################################


##### Ahmed et al. (2012) #####

# these are extracted this way to compare with Lippens et al.'s coding
# for actual MA, variables were both, neither, only control and only fourty
compare_ahmed_12 <- data.frame(id = "Ahmed et al. (2012)",
                       control_yes = 42,
                       control_no = 424,
                       fourty_yes = 13,
                       fourty_no = 453)  
 #write_csv(here("Csv Files", "ahmed_12.csv"))

# Baert coding: control_tested(tested_maj) = 446, control_yes(callback_maj) = 42
# Baert coding: forty_tested(tested_min) = 446, forty_yes(callback_min) = 13
# this one matches completely


##### Jansons & Zukov (2012) #####

compare_jansons_12 <- data.frame(id = "Jansons & Zukov (2012)",
                         control_yes = 56,
                         control_no = 473,
                         fifty_yes = 27,
                         fifty_no = 502) 
  #write_csv(here("Csv files","jansons_12.csv"))

# Lippens et al. did not include this one, assuming because it's grey literature

#################################################
##### Between-subject correspondence studies ####
#################################################


##### Carlsson & Eriksson (2019) #####

# Lippens et al. did not extract because of missing raw data 
  

##### Farber et al. (2019) #####

# used callback rates are taken from the interview invitation column
# numbers reported are not reported as percentages

compare_farber_19 <- tibble(age_group = c("control.prop_yes", "control.prop_no", 
                                  "fourty.prop_yes", "fourty.prop_no",
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

# our coding: control_no = 1321, control_yes = 139 - tested = 1460 - our control group is 33-34
# Lippens et al. coding: control_tested(tested_maj) = 1368, control_yes(callback_maj) = 122 - their control group includes 33-34 or 42-43
# our coding: sixty_no = 1244, sixty_yes = 101 - tested = 1345
# Lippens et al. coding: sixty_tested(tested_min) = 1345, sixty_yes(callback_min) = 101
# can't find data for the 51-52 in either group in Lippens et al., can't compare that



##### Neumark et al. (2016) Experimental Age https://www.aeaweb.org/articles?id=10.1257/aer.p20161008 ######

compare_neumark16_raw <- read_dta(here("Input" , "mergefinal.dta"))

compare_neumark16 <- compare_neumark16_raw %>% 
  #choose age and callback variables
  select(age, callback) %>% 
  #create age categories
  mutate(age_group = case_when(
    age <= 31 ~ "control",
    age == 49 | age == 50 | age == 51 ~ "fifty",
    TRUE ~ "sixty"
  )) %>% 
  group_by(age_group) %>% 
  count(callback)

# our coding: control_no = 10896, control_yes = 2505 - tested = 13401
# Lippens et al. coding: control_tested(tested_maj) = 13401, control_yes(callback_maj) = 2505
# There is a difference between tested_min and callback_min values


##### Neumark et al. (2019) Do State Laws https://doi.org/10.1086/704008 #######

## Neumark studies have both positiveresponse and callback variables, for their analyses 
## From my understandin, Lippens et al. took callback variables which counts both "maybe" and "yes" as a callback

compare_neumark19_raw <- read_dta(here("Input", "FinalData50States.dta"))

compare_neumark19_state <- compare_neumark19_raw %>% 
  select(positiveresponse, age, callback) %>% 
  mutate(age_group = case_when(
    age <= 31 ~ "control",
    TRUE ~ "sixty"
  )) %>% 
  group_by(age_group) %>% 
  count(positiveresponse, callback)

# our coding: control_no = 5288, control_yes = 137(maybe) + 1789(yes) = 1926; sum = 7214
# Lippens et al. coding: control_tested(tested_maj) = 7184, control_yes(callback_maj) = 1745

# our coding: sixty_no = 5870, sixty_yes = 119(maybe) + 1225(yes) = 1334; sum = 7214
# Lippens et al. coding: sixty_tested(tested_maj) = 7184, sixty_yes(callback_maj) = 1237

# these don't match at all


##### Challe et al. (2015) - could not be extracted (authors didn't provide data) ####

##### Capeau et al. (2012) #####

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
  


























