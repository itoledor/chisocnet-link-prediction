# Setup -------------------------------------------------------------------
source(here::here("code", "code_01_setup_01_libraries.R"))
set.seed(300)


# Read data ---------------------------------------------------------------
graph_uw <- linkprediction::uw

# Tidy data ---------------------------------------------------------------
data_uw <- graph_uw %>% as_edgelist() 
data_uw <- data_uw %>% set_colnames(c("from", "to"))
data_uw <- data_uw %>% as_tibble()
data_uw <- data_uw %>% mutate(p1 = E(graph_uw)$p1) 
data_uw <- data_uw %>% mutate(p2 = E(graph_uw)$p2) 
data_uw <- data_uw %>% bind_rows(data_uw %>% set_colnames(c("to", "from", "p1", "p2"))) 
data_uw <- data_uw %>% filter(from > to) 

# Classification of all the dyads in the uw data ---------------------------
data_uw_full <- tibble(from = V(graph_uw) %>% names, to = V(graph_uw) %>% names) 
data_uw_full <- data_uw_full %>% complete(from, to) 
data_uw_full <- data_uw_full %>% filter(from > to) 
data_uw <- data_uw %>% right_join(data_uw_full) 
data_uw <- data_uw %>% replace_na(list(p1 = FALSE, p2 = FALSE)) 
data_uw <- data_uw %>% mutate(test_data = !p1)
data_uw <- data_uw %>% mutate(new_coauthorship = !p1&p2)
data_uw %>% count(p1,p2,test_data, new_coauthorship)

# Labelled dataset                              ---------------------------
data_test <- data_uw %>% filter(test_data)
data_test %>% count(new_coauthorship)

# Link prediction                                ---------------------------
data_prediction <- linkprediction::proxfun(graph_uw, method = "aa", value = "edgelist")
data_prediction <- linkprediction::proxfun(graph_uw, method = "rwr", value = "edgelist")
data_prediction
data_prediction <- data_prediction %>% as_tibble
data_prediction <- data_prediction %>% mutate(from = V(graph_uw)[from] %>% names)
data_prediction <- data_prediction %>% mutate(to   = V(graph_uw)[to  ] %>% names)
data_prediction 

#Evaluation
data_prediction <- data_prediction %>% right_join(data_test)
data_prediction <- data_prediction %>% filter(value %>% is.na %>% not)

#Using {pROC}
roc(data_prediction$new_coauthorship, data_prediction$value, na.rm = TRUE, plot = TRUE)
auc(roc(data_prediction$new_coauthorship, data_prediction$value, na.rm = TRUE, plot = TRUE))

#Using {PRROC}
roc <- roc.curve(scores.class0 = data_prediction$value, weights.class0 = data_prediction$new_coauthorship)
roc$auc
pr <- pr.curve(scores.class0 = data_prediction$value, weights.class0 = data_prediction$new_coauthorship)
pr$auc.integral