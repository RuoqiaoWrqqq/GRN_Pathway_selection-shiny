# library(dplyr)
# library(stringdist)
# 
# df <- merged_df %>%
#   dplyr::select(pathway_name,top_level_pathway_name)
# # Suppose df is your dataset
# # and ref is the complete Reactome file with both columns known
# ref <- df %>% filter(!is.na(top_level_pathway_name))
# df_missing <- df %>% filter(is.na(top_level_pathway_name))
# 
# # Compute string similarity
# df_filled <- df_missing %>%
#   rowwise() %>%
#   mutate(
#     top_level_pathway_name = ref$top_level_pathway_name[
#       which.min(stringdist(pathway_name, ref$pathway_name, method = "jw"))
#     ]
#   )
# 
# 
# 
# library(text2vec)
# library(glmnet)
# 
# # Prepare data
# df_train <- df %>% filter(!is.na(top_level_pathway_name))
# df_test <- df %>% filter(is.na(top_level_pathway_name))
# 
# # Tokenization and vectorization
# it <- itoken(df_train$pathway_name, progressbar = FALSE)
# vocab <- create_vocabulary(it)
# vectorizer <- vocab_vectorizer(vocab)
# dtm <- create_dtm(it, vectorizer)
# 
# # Train a multinomial logistic regression
# model <- cv.glmnet(dtm, df_train$top_level_pathway_name, family = "multinomial")
# 
# # Predict
# it_test <- itoken(df_test$pathway_name, progressbar = FALSE)
# dtm_test <- create_dtm(it_test, vectorizer)
# df_test$predicted_top <- predict(model, dtm_test, type = "class")
# compare_df <- df_test %>% inner_join(df_filled, by = "pathway_name")
# compare_df <- compare_df %>%
#   mutate(agreement = predicted_top == top_level_pathway_name.y)
# write.csv(file = "compare_df.csv",compare_df)
# summary_table <- compare_df %>%
#   summarise(
#     total = n(),
#     agreed = sum(agreement),
#     disagreed = total - agreed,
#     agreement_rate = round(agreed / total * 100, 2)
#   )
# print(summary_table)
# 
# write.csv(file = "Reactome pathways_compare_df.csv",summary_table)

# all reactome data base from website and gmt file 
merged_df <- read.csv("./inst/extdata/merged_df.csv", header = T,row.names = 1)
# get all known label 
merged_df <- merged_df %>% filter(!is.na(top_level_pathway_name)) 
# prediect label from BioBERT
Reactome_Pathway_Filled_BioBERT <- read.csv("./inst/extdata/Reactome_Pathway_Filled_BioBERT.csv", header = T)
Reactome_Pathway_Filled_BioBERT$top_level_pathway_name <- Reactome_Pathway_Filled_BioBERT$predicted_top_level_pathway_name

df <- rbind(merged_df[,c("pathway_name","top_level_pathway_name")],Reactome_Pathway_Filled_BioBERT[,c("pathway_name","top_level_pathway_name")])
write.csv(file = "./inst/extdata/Reactome Pathways hierarchy relationship update.csv",df)



