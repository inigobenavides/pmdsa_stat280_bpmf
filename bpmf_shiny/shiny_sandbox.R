ratings <- read_feather("../rating.feather")

# Top Users
top_users <- ratings %>%
  group_by(userId) %>% 
  summarise(
    num_movies = n()
  ) %>% 
  ungroup() %>% 
  arrange(desc(num_movies)) %$%
  userId %>% 
  {.[1:10]}


# Top movies 

# Sample movies with more than 4 user ratings
sample_movies <- ratings %>% 
  filter(userId %in% top_users) %>% 
  group_by(movieId) %>% 
  summarise(
    num_users = n()
  ) %>% ungroup() %>% 
  arrange(desc(num_users)) %>% 
  filter(num_users >= 5) %>% 
  sample_n(10) %$% 
  movieId %>% 
  {.[1:10]}

ratings %>% 
  filter(userId %in% top_users) %>% 
  filter(movieId %in% sample_movies) %>% 
  mutate(userId = as.numeric(factor(userId, labels = seq(1:10)))) %>% 
  mutate(movieId = as.numeric(factor(movieId, labels = seq(1:10)))) %>% 
  rename(
    row = userId,
    col = movieId,
    value = rating
  ) %>% 
  mutate(value = sapply(value, transform_rating_to_score)) %>% 
  as.data.frame() %>% 
  saveRDS("dummy_data/netflix_sample.rds")
