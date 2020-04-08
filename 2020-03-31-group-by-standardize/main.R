library(dplyr)

# read data
df <- readr::read_csv('data/FinalAbandonmentDataset_2-14-20.csv')

# I'm only going to standardize a few. I'm not going to 
# do any of the factoring, etc.

## write standardize function
standardize <- function (data) {
  data - mean(data, na.rm = T) / sd(data, na.rm = T)
}

# group by statements
# an example of how it works by counting the
# number of animals in each group (studyarea, species)
gb_count <- df %>% 
  group_by(StudyArea, Species) %>% 
  summarize(
    n = n(),
    mean_weight = mean(NeonateWeight, na.rm = T),
    sd_weight = sd(NeonateWeight, na.rm = T)
  )

gb_count  

# now mutate the data ...
standardized <- df %>% 
  group_by(StudyArea, Species) %>% 
  mutate(
    StdNeonateWeight = standardize(NeonateWeight)
  ) %>% 
  ungroup()

# you can check that it is correct ...
## standardize the neonate weight by hand
test_dat <- df %>%
  filter(StudyArea == 'DEER' & Species == 'ELK')

test_dat$StdNeonateWeight <- test_dat$NeonateWeight - mean(test_dat$NeonateWeight, na.rm = T) / sd(test_dat$NeonateWeight, na.rm = T)
test_dat$StdNeonateWeight

## get the std neonate weight created with the mutate function
StdNeonateWeight <- standardized %>% 
  filter(StudyArea == 'DEER' & Species == 'ELK') %>% 
  magrittr::extract2('StdNeonateWeight')

## compare the two
all.equal(test_dat$StdNeonateWeight, StdNeonateWeight)

# GET AFTER IT
