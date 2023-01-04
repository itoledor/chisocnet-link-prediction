# code_05_partition_01_link-prediction_03_neuroscience.R
# Setup -------------------------------------------------------------------

set.seed(300)
source(here::here("code", "code_01_setup_01_libraries.R"))

# Read data ---------------------------------------------------------------

data_aam <- read_rds(here("data", "data_03_transformed_01_neuroscience_02_author-author-matrix.rds"))

# Generate training and testing partitions --------------------------------

data_aam_train <- data_aam %>% slice_sample(prop = 0.8) %>% mutate(partition = "train")
data_aam_test  <- data_aam %>% anti_join(data_aam_train, by = names(data_aam)) %>% mutate(partition = "test")

# Write data --------------------------------------------------------------

data_aam_train  %>% write_rds(here("data","data_04_partition_01_neuroscience_02_missing-links_01_aam_train.rds"))  
data_aam_test   %>% write_rds(here("data","data_04_partition_01_neuroscience_02_missing-links_01_aam_test.rds")) 
