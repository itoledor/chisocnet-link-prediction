# Setup -------------------------------------------------------------------
source(here::here("code", "code_01_setup_01_libraries.R"))
set.seed(300)

# Functions
first.component<- function(graph){
  cl<-clusters(graph)
  induced.subgraph(graph,which(cl$membership==order(cl$csize, decreasing = T)[1]))}

# Read data ---------------------------------------------------------------
data_aam_train <- read_rds(here("data","data_04_partition_01_neuroscience_01_link-prediction_01_aam_train.rds"))  
data_aam_test  <- read_rds(here("data","data_04_partition_01_neuroscience_01_link-prediction_01_aam_test.rds")) 

# tidy data ---------------------------------------------------------------
data_aam_train <- data_aam_train %>% select(starts_with("author"))
data_aam_train <- data_aam_train %>% distinct()
data_aam_train <- data_aam_train %>% bind_rows(data_aam_train %>% set_colnames(c("author_id_y", "author_id_x"))) 
data_aam_train <- data_aam_train %>% filter(author_id_x > author_id_y) 

data_aam_test <- data_aam_test %>% select(starts_with("author"))
data_aam_test <- data_aam_test %>% distinct()
data_aam_test <- data_aam_test %>% bind_rows(data_aam_test %>% set_colnames(c("author_id_y", "author_id_x"))) 
data_aam_test <- data_aam_test %>% filter(author_id_x > author_id_y) 
data_aam_test <- data_aam_test %>% set_colnames(c("from", "to")) 
data_aam_test <- data_aam_test %>% mutate(new_coauthorship = TRUE) 

# make graph --------------------------------------------------------------
graph_aam_train <- data_aam_train %>% graph_from_data_frame(directed = FALSE)
graph_aam_train <- graph_aam_train %>% first.component()
 
# Link prediction ---------------------------------------------------------

data_prediction <- linkprediction::proxfun(graph_aam_train, method = "aa", value = "edgelist")
data_prediction
data_prediction <- data_prediction %>% as_tibble
data_prediction <- data_prediction %>% mutate(from = V(graph_aam_train)[from] %>% names)
data_prediction <- data_prediction %>% mutate(to   = V(graph_aam_train)[to  ] %>% names)

data_prediction <- data_prediction %>% left_join(data_aam_test)
data_prediction <- data_prediction %>% replace_na(list(new_coauthorship = FALSE))
data_prediction 

#Using {pROC}
roc(data_prediction$new_coauthorship, data_prediction$value, na.rm = TRUE, plot = TRUE)
auc(roc(data_prediction$new_coauthorship, data_prediction$value, na.rm = TRUE, plot = TRUE))

#Using {PRROC}
roc <- roc.curve(scores.class0 = data_prediction$value, weights.class0 = data_prediction$new_coauthorship)
roc$auc
pr <- pr.curve(scores.class0 = data_prediction$value, weights.class0 = data_prediction$new_coauthorship)
pr$auc.integral
