# code_04_transform_02_author-author-matrix_03_neuroscience.R
# Setup -------------------------------------------------------------------

source(here::here("code", "code_01_setup_01_libraries.R"))

# Read data ---------------------------------------------------------------

data <- read_rds(here("data", "data_03_transformed_01_neuroscience_01_document-author-matrix.rds"))

#  Generate Author-Author Matrix ------------------------------------------

data <- data %>% left_join(data, by=c("year", "document_id"), suffix = c("_x", "_y"))

# Write data --------------------------------------------------------------

data  %>% write_rds(here("data", "data_03_transformed_01_neuroscience_02_author-author-matrix.rds"))