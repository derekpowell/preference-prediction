library(tidyverse)

artworks <- read_csv("Artworks - Sheet1.csv")
qualtrics_data <- read_csv("clean_complete_pilot_data.csv")
rating_columns <- names(qualtrics_data)[grepl('rating_scores', names(qualtrics_data))]


is_numeric_or_na <- function(x) {
  suppressWarnings(!is.na(as.numeric(as.character(x)))) | is.na(x)
}


filtered_qualtrics_data <- qualtrics_data %>%
  rowwise() %>%
  filter(all(sapply(c_across(all_of(rating_columns)), is_numeric_or_na))) %>%
  ungroup() %>%
  mutate(participantID=row_number())


qualtrics_long <- filtered_qualtrics_data %>%
  pivot_longer(
    cols = contains('rating_scores'),
    names_to = 'Artwork_ID',
    names_pattern = '(\\d+)_rating_scores', 
    values_to = 'Rating'
  ) %>%
  mutate(Artwork_ID = as.numeric(Artwork_ID))  


## ---- NEED TO CHECK THIS IS CORRECT I.E. ARTWORKS IN RIGHT ORDER
artworks$ID <- 1:nrow(artworks)
final_dataset <- merge(artworks, qualtrics_long, by = 'Artwork_ID')
final_dataset$Rating <- as.numeric(as.character(final_dataset$Rating))

## -------

# This should get you started -- DP 2024-04-30
recomm_data <- final_dataset %>% 
  select(participantID, ID, Rating) %>% 
  spread(ID, Rating) 

recomm_data_wide_filtered <- recomm_data %>% 
  pivot_longer(-participantID) %>% 
  group_by(participantID) %>% 
  filter(sum(!is.na(value)) == 60) %>% 
  spread(name, value) %>% 
  ungroup() %>% 
  select(-participantID)




### --- recommenderlab


library(recommenderlab)
ratings_data <- as(as.matrix(recomm_data_wide_filtered), "realRatingMatrix")

train <- ratings_data[1:250,]

get_item_embeddings <- function(rec_result, rec_data){
  mod <- getModel(rec_result)
  mat <- mod$svd$V
  
  d <- mat %>% 
    as_tibble() %>% 
    mutate(item = colnames(rec_data)) %>% 
    relocate(item)
  
  return(d)
}

get_user_embeddings <- function(rec_result){
  mod <- getModel(rec_result)
  return(mod$svd$U)
}


svfd <- Recommender(train, "SVDF", parameter = list(k = 5))
embeddings <- get_item_embeddings(svfd, train) %>% 
  mutate(item = as.numeric(item))

out <- embeddings %>% 
  left_join(artworks, by = c("item" = "Artwork_ID")) %>% 
  select(-contains("..")) %>% 
  rename(
    piclink = `Link to Picture`,
    imglink = `link to image`
  )

write_csv(out, "artwork-embeddings.csv")
