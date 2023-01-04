# code_02_import_01_download-raw-data_04_openalex_01_economics.R
# Setup -------------------------------------------------------------------

source(here::here("code", "code_01_setup_01_libraries.R"))

# Download data -----------------------------------------------------------

# Generates automated queries to OpenAlex API for downloading bibliographic data
# of works in the field of NEUROSCIENCE published in journals in one-month widows

openalex_polite("nacho.toledo@gmail.com") 

data <- tibble(search = "neuroscience", años = make_date(2012:2021))
data <- data %>% mutate(fecha_ini = años %>% map(~.x %m+% (months(0:11))))
data <- data %>% mutate(fecha_fin = años %>% map(~.x %m+% (months(1:12) - days(1))))
data <- data %>% unnest(cols = c(fecha_ini, fecha_fin))
data <- data %>% mutate(filter = str_c("type:journal-article,from_publication_date:", fecha_ini, ",to_publication_date:", fecha_fin))
data <- data %>% mutate(query = map2(filter, search, openalex:::openalex_query))
data <- data %>% mutate(data = map(query, ~openalex::openalex_crawl(entity = "works", query = .x)))
data <- data %>% mutate(data = map(data, openalex::openalex_flatten_long))

# Write data --------------------------------------------------------------

data %>% write_rds(here("data", "data_01_openalex_01_neuroscience_02.rds"))