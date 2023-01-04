# code_04_transform_01_document-author-matrix_03_neuroscience.R
# Setup -------------------------------------------------------------------

source(here::here("code", "code_01_setup_01_libraries.R"))

# Read data ---------------------------------------------------------------

data <- read_rds(here("data", "data_02_tidy-data_01_neuroscience.rds"))

#  Generate Document-Author Matrix ------------------------------------------

data <- data %>% rename(document_id = id)
data <- data %>% mutate(year      = document_metadata %>% map(extract2, "publication_year") %>% unlist)
data <- data %>% mutate(author_id = authorships %>% map(extract2, "authorships_author_id" ))
data <- data %>% mutate(author_id = author_id %>% map(unlist)) %>% unnest(author_id)
data <- data %>% select(year, document_id, author_id)

# Write data --------------------------------------------------------------

data  %>% write_rds(here("data", "data_03_transformed_01_neuroscience_01_document-author-matrix.rds"))