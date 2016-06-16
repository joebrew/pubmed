library(dplyr)
source('search_pubmed.R')

topic <- 
  paste0('(malaria) AND (adjunctive therapy)')
output <- pubmed(start_year = 1980,
                 end_year = 2015,
                 search_topic = topic,
                 counts_only = FALSE)
# Break into dataframe
results <- left_join(output$results, output$abstracts,
          by = 'id') %>%
  mutate(url = paste0("http://www.ncbi.nlm.nih.gov/pubmed/?term=", id)) 
# Select only useful columns


library(readr)
write_csv(results, '~/Desktop/rosauro.csv')
