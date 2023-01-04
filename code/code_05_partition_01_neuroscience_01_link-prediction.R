# code_05_partition_01_link-prediction_03_neuroscience.R
# Setup -------------------------------------------------------------------

set.seed(300)
source(here::here("code", "code_01_setup_01_libraries.R"))

# Read data ---------------------------------------------------------------

data_aam <- read_rds(here("data", "data_03_transformed_01_neuroscience_02_author-author-matrix.rds"))

# Explores partition sizes by year ----------------------------------------

# Number of links
threshold <- data_aam  %>% count(year) 
threshold <- threshold %>% mutate(train = cumsum(n)/sum(n))
threshold <- threshold %>% mutate(test  = 1 - train)
threshold <- threshold %>% mutate(total = train + test)
threshold <- threshold %>% mutate(across(all_of(c("train", "test", "total")), percent, accuracy = 1))
threshold
threshold %>% ggplot(aes(x = year, y = n)) + geom_col()

# Generate training and testing partitions --------------------------------

data_aam <- data_aam %>% mutate(partition = if_else(year %>% is_greater_than(2019), "test", "train"))

data_aam_train <- data_aam %>% filter(partition == "train")
data_aam_test  <- data_aam %>% filter(partition == "test" )

# Write data --------------------------------------------------------------

data_aam_train  %>% write_rds(here("data","data_04_partition_01_neuroscience_01_link-prediction_01_aam_train.rds"))  
data_aam_test   %>% write_rds(here("data","data_04_partition_01_neuroscience_01_link-prediction_01_aam_test.rds")) 
