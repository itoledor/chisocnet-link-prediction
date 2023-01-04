# code_03_tidy_01_tidy-data_03_neuroscience.R
# Setup -------------------------------------------------------------------

source(here::here("code", "code_01_setup_01_libraries.R"))

# Read data ---------------------------------------------------------------

data <- read_rds(here("data", "data_01_openalex_01_neuroscience_02.rds"))

# Tidy data ---------------------------------------------------------------

data <- data %>% select(-query)
data <- data %>% mutate(data = data %>% map(~.x %>% mutate(id = ifelse(name == "id", value, NA))))
data <- data %>% mutate(data = data %>% map(~.x %>% fill(id, .direction = "down")))
data <- data %>% mutate(data = data %>% map(~.x %>% group_by(id)))
data <- data %>% mutate(data = data %>% map(~.x %>% nest(c(name, value))))
data <- data %>% unnest(cols = data)

# Authorship --------------------------------------------------------------

data <- data %>% mutate(authorships = data        %>% map(~.x %>% filter(name %>% str_starts("authorships"))))
data <- data %>% mutate(authorships = authorships %>% map(~.x %>% mutate(author_id = ifelse(name == "authorships_author_id", value, NA))))
data <- data %>% mutate(authorships = authorships %>% map(~.x %>% mutate(author_id = ifelse(name == "authorships_author_position", lead(author_id), author_id))))
data <- data %>% mutate(authorships = authorships %>% map(~.x %>% fill(author_id, .direction = "down")))
data <- data %>% mutate(authorships = authorships %>% map(~.x %>% pivot_wider(id_cols = author_id, names_from = name, values_from = value)))
data <- data %>% mutate(authorships = authorships %>% map(~.x %>% unnest))
data <- data %>% mutate(authorships = authorships %>% map(~.x %>% group_by(across(starts_with("authorships_author")))))
data <- data %>% mutate(authorships = authorships %>% map(~.x %>% nest()))
data <- data %>% mutate(authorships = authorships %>% map(~.x %>% ungroup))

# Concepts ----------------------------------------------------------------

data <- data %>% mutate(concepts = data     %>% map(~.x %>% filter(name %>% str_starts("concepts"))))
data <- data %>% mutate(concepts = concepts %>% map(~.x %>% mutate(concepts_id = ifelse(name == "concepts_id", value, NA))))
data <- data %>% mutate(concepts = concepts %>% map(~.x %>% fill(concepts_id, .direction = "down")))
data <- data %>% mutate(concepts = concepts %>% map(~.x %>% filter(name != "concepts_id")))
data <- data %>% mutate(concepts = concepts %>% map(~.x %>% pivot_wider(id_cols = concepts_id, names_from = name, values_from = value)))

# Referenced_works --------------------------------------------------------

data <- data %>% mutate(referenced_works = data             %>% map(~.x %>% filter(name %>% str_starts("referenced"))))
data <- data %>% mutate(referenced_works = referenced_works %>% map(~.x %>% rename(referenced_work_id = value)))
data <- data %>% mutate(referenced_works = referenced_works %>% map(~.x %>% select(-name)))

# Related_works -----------------------------------------------------------

data <- data %>% mutate(related_works = data          %>% map(~.x %>% filter(name %>% str_starts("related"))))
data <- data %>% mutate(related_works = related_works %>% map(~.x %>% rename(related_works_id = value)))
data <- data %>% mutate(related_works = related_works %>% map(~.x %>% select(-name)))

# Abstracts ---------------------------------------------------------------

data <- data %>% mutate(abstract = data     %>% map(~.x %>% filter(name %>% str_starts("abstract"))))
data <- data %>% mutate(abstract = abstract %>% map(~.x %>% mutate(word = name %>% str_remove_all("abstract_inverted_index_"))))
data <- data %>% mutate(abstract = abstract %>% map(~.x %>% mutate(word = word %>% str_remove_all("\\d"))))
data <- data %>% mutate(abstract = abstract %>% map(~.x %>% mutate(word = word %>% str_remove_all("[[:punct:]]"))))
data <- data %>% mutate(abstract = abstract %>% map(~.x %>% mutate(word = word %>% str_to_lower())))
data <- data %>% mutate(abstract = abstract %>% map(~.x %>% mutate(value = value %>% str_pad(3,"left", "0"))))
data <- data %>% mutate(abstract = abstract %>% map(~.x %>% arrange(value)))
data <- data %>% mutate(abstract = abstract %>% map(~.x %>% select(-name)))

# Cited-by count by year --------------------------------------------------

data <- data %>% mutate(cited_by_count = data           %>% map(~.x %>% filter(name %>% str_starts("counts_by_year"))))
data <- data %>% mutate(cited_by_count = cited_by_count %>% map(~.x %>% mutate(year = ifelse(name == "counts_by_year_year", value, NA))))
data <- data %>% mutate(cited_by_count = cited_by_count %>% map(~.x %>% fill(year, .direction = "down")))
data <- data %>% mutate(cited_by_count = cited_by_count %>% map(~.x %>% pivot_wider(id_cols = year)))
data <- data %>% mutate(cited_by_count = cited_by_count %>% map(~.x %>% select(-year)))

# Mesh --------------------------------------------------------------------

data <- data %>% mutate(mesh = data %>% map(~.x %>% filter(name %>% str_starts("mesh"))))
data <- data %>% mutate(mesh = mesh %>% map(~.x %>% mutate(mesh_id = ifelse(name == "mesh_descriptor_ui", value, NA))))
data <- data %>% mutate(mesh = mesh %>% map(~.x %>% fill(mesh_id, .direction = "down")))
data <- data %>% mutate(mesh = mesh %>% map(~.x %>% distinct))
data <- data %>% mutate(mesh = mesh %>% map(~.x %>% filter(name != "mesh_descriptor_ui")))
data <- data %>% mutate(mesh = mesh %>% map(~.x %>% pivot_wider(id_cols = mesh_id, names_from = name, values_from = value)))

# Alternate host ----------------------------------------------------------

data <- data %>% mutate(alternate = data %>% map(~.x %>% filter(name %>% str_starts("alternate"))))
data <- data %>% mutate(alternate = alternate %>% map(~.x %>% mutate(alternate_id = ifelse(name == "alternate_host_venues_display_name", value, NA))))
data <- data %>% mutate(alternate = alternate %>% map(~.x %>% fill(alternate_id, .direction = "down")))
data <- data %>% mutate(alternate = alternate %>% map(~.x %>% distinct))
data <- data %>% mutate(alternate = alternate %>% map(~.x %>% filter(name != "alternate_host_venues_display_name")))
data <- data %>% mutate(alternate = alternate %>% map(~.x %>% pivot_wider(id_cols = alternate_id, names_from = name, values_from = value)))
data <- data %>% mutate(alternate = alternate %>% map_dbl(length) %>% equals(1) %>% if_else(alternate, alternate %>% map(~.x %>% unnest())))


# Document metadata -----------------------------------------------------------

data <- data %>% mutate(document_metadata = data %>% map(~.x %>% filter(name %>% str_starts("(authorships|concepts|referenced|related|abstract|counts_by_year|mesh)", negate =TRUE))))
data <- data %>% mutate(document_metadata = document_metadata %>% map(~.x %>% pivot_wider(names_from = name, values_from = value)))

# Write data --------------------------------------------------------------

data %>% write_rds(here("data", "data_02_tidy-data_01_neuroscience.rds"))

