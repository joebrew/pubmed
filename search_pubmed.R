library(RISmed)
library(dplyr)
library(ggplot2)
library(tidyr)
# http://amunategui.github.io/pubmed-query/

# Define function for getting results from pubmed
pubmed <- function(start_year = 2014,
                   end_year = 2016,
                   search_topic = 'malaria',
                   counts_only = TRUE){
  
  # Define a year range
  years <- start_year:end_year
  
  
  # If counts only, simply get the counts by year
  if(counts_only){
    # JUST COUNTING RESULTS
    
    # Create a placeholder for results
    return_object <- data.frame(year = years,
                                n = NA)
    
    for (year in 1:length(years)){
      message(paste0('Fetching records for year ', years[year]))
      
      # Perform search
      search_query <- try({ 
        EUtilsSummary(search_topic, 
                      db="pubmed", 
                      mindate=years[year], 
                      maxdate=years[year],
                      retmax = 5000)})
      if(inherits(search_query, 'try-error')){
        n <- 0
      } else {
        n <- QueryCount(search_query)
      }
      
      # Populate results dataframe
      return_object$n[year] <- n
    }
    
  } else {
    # NOT JUST COUNTS. ACTUALLY RETRIEVE RESULTS
    
    # Create an empty list for sticking results
    results_list <- list()
    # Create another empty list for sticking abstracts
    abstracts_list <- list()
    
    # Loop through each year to get results
    for (year in 1:length(years)){
      message(paste0('Fetching records for year ', years[year]))
      
      # Perform search
      search_query <- 
        EUtilsSummary(search_topic, 
                      db="pubmed", 
                      mindate=years[year], 
                      maxdate=years[year],
                      retmax = 5000)
      
      # See results summary
      # summary(search_query)
      
      # See number of results
      # QueryCount(search_query)
      
      # Get IDs of the articles returned in our query
      # Qids <- queryId(search_query)
      
      # Actually fetch data
      records <- EUtilsGet(search_query)
      
      # Turn into a data.frame
      pubmed_data <- 
        data.frame('title' = ArticleTitle(records),
                   # 'Abstract' = AbstractText(records),
                   'language' = Language(records),
                   'country' = Country(records),
                   'id' = ArticleId(records),
                   'year' = years[year])
      
      # Create separate dataframe for abstracts
      abstracts <- data.frame('id' = ArticleId(records),
                              'abstract' = AbstractText(records))
      
      # Add authors separately
      temp <- Author(records)
      first_authors <- lapply(temp,
                              function(x){
                                x[1,]
                              })
      last_authors <- lapply(temp,
                             function(x){
                               x[nrow(x),]
                             })
      for (i in c('first', 'last')){
        # Last name
        pubmed_data[,paste(i, '_author_last_name')] <- 
          unlist(lapply(get(paste0(i, '_authors')), function(x){x['LastName']}))
        
        # First name
        pubmed_data[,paste(i, '_author_first_name')] <- 
          unlist(lapply(get(paste0(i, '_authors')), function(x){x['ForeName']}))
        
        # Initials
        pubmed_data[,paste(i, '_author_initials')] <- 
          unlist(lapply(get(paste0(i, '_authors')), function(x){x['Initials']}))
      }
      
      # Add results to results_list
      results_list[[year]] <- pubmed_data
      
      # Add abstract to abstracts list
      abstracts_list[[year]] <- abstracts
      
      # Remove unecessary objects
      rm(pubmed_data, abstracts)
    }
    
    # Bind together the results
    results <- do.call('rbind', results_list)
    
    # Bind together the abstracts
    abstracts <- do.call('rbind', abstracts_list)
    
    # Put into list
    return_object <- 
      list(results = results,
           abstracts = abstracts)
  }
  # Return
  return(return_object)
}

# # Use pubmed to get results for malaria eradication
# malaria_eradication <- 
#   pubmed(start_year = 1945,
#          end_year = 2015,
#          search_topic = paste0('(malaria[Title/Abstract])', 
#                                'AND (elimination OR eradication)'))
# 
# # Use pubmed to get results for malaria more generally
# malaria <- 
#   pubmed(start_year = 1945,
#          end_year = 2015,
#          search_topic = paste0('(malaria[Title/Abstract])'))
# 
# # Horizontally bind
# combined <- 
#   left_join(malaria_eradication %>%
#           rename(eradication = n),
#         malaria %>%
#           rename(malaria = n),
#         by = 'year') %>%
#   mutate(p = eradication / malaria * 100)
# 
# # Rename to make more clear
# combined <- 
#   combined %>%
#   rename(`Mentions eradication or elmination` = eradication,
#          `General malaria` = malaria)
# 
# # Gather to make long
# combined <- gather(combined, 
#                    key, 
#                    value, `Mentions eradication or elmination`:p)
# 
# # Visualize
# ggplot(data = combined %>%
#          filter(key != 'p'),
#        aes(x = year,
#            y = value,
#            group = key,
#            fill = key)) +
#   geom_area() +
#   xlab('Year') +
#   ylab('Publications') +
#   scale_fill_manual(values = c('darkgrey', 'red'),
#                     name = '') +
#   theme_bw() +
#   ggtitle(expression(atop('Papers containing "malaria" in title/abstract: 1945-present', 
#                           atop(italic("Retrieved from PubMed"), "")))) +
#   theme(legend.position = 'bottom')
# ggsave('pubmed.pdf')
# 
# ggplot(data = combined %>%
#          filter(key == 'p'),
#        aes(x = year, 
#            y = value)) +
#   geom_area(alpha = 0.6,
#             color = 'black') +
#   xlab('Year') +
#   ylab('Percentage') +
#   theme_bw() +
#   ggtitle(expression(atop('Papers containing "eradication" or "elimination"', 
#                           atop(italic('As % of all "malaria" papers, searching title/abstract only, retrieved from PubMed'), ""))))
# ggsave('pubmed2.pdf')
# 
# # NTDs
# 
# ntds <- 
#   pubmed(start_year = 1990,
#          end_year = 2015,
#          search_topic = paste0('(onchocerciasis[Title/Abstract])',
#                                ' OR (leishmaniasis[Title/Abstract])',
#                                ' OR (human african trypanosomiasis[Title/Abstract])',
#                                ' OR (lymphatic filariasis[Title/Abstract])',
#                                ' OR (chagas[Title/Abstract])',
#                                ' OR (schistosomiasis[Title/Abstract])',
#                                ' OR (oncocerchiasis[Title/Abstract])'))
# 
# ntds_eradication <- 
#   pubmed(start_year = 1990,
#          end_year = 2015,
#          search_topic = paste0('(onchocerciasis[Title/Abstract])',
#                                ' OR (leishmaniasis[Title/Abstract])',
#                                ' OR (human african trypanosomiasis[Title/Abstract])',
#                                ' OR (lymphatic filariasis[Title/Abstract])',
#                                ' OR (chagas[Title/Abstract])',
#                                ' OR (schistosomiasis[Title/Abstract])',
#                                ' OR (oncocerchiasis[Title/Abstract])', 
#                                ' (AND (public-private partnership)',
#                                ' OR (ppp)',
#                                ' OR (pdp)',
#                                ' OR (public private))'))
# 
# 
# # Horizontally bind
# combined <- 
#   left_join(ntds_eradication %>%
#               rename(eradication = n),
#             ntds %>%
#               rename(ntds = n),
#             by = 'year') %>%
#   mutate(p = eradication / ntds * 100)
# 
# # Rename to make more clear
# combined <- 
#   combined %>%
#   rename(`Mentions eradication or elmination` = eradication,
#          `General NTDs` = ntds)
# 
# # Gather to make long
# combined <- gather(combined, 
#                    key, 
#                    value, `Mentions eradication or elmination`:p)
# 
# # Visualize
# g1 <- ggplot(data = combined %>%
#          filter(key != 'p'),
#        aes(x = year,
#            y = value,
#            group = key,
#            fill = key)) +
#   geom_area() +
#   xlab('Year') +
#   ylab('Publications') +
#   scale_fill_manual(values = c('darkgrey', 'red'),
#                     name = '') +
#   theme_bw() +
#   ggtitle(expression(atop('Papers containing NTDs in title/abstract: 1990-present', 
#                           atop(italic("Retrieved from PubMed"), "")))) +
#   theme(legend.position = 'bottom')
# ggsave('')
# 
# g2 <- ggplot(data = combined %>%
#          filter(key == 'p'),
#        aes(x = year, 
#            y = value)) +
#   geom_area(alpha = 0.6,
#             color = 'black') +
#   xlab('Year') +
#   ylab('Percentage') +
#   theme_bw() +
#   ggtitle(expression(atop('Papers mentioning PPP in abstract', 
#                           atop(italic('As % of all NTDs papers, searching title/abstract only, retrieved from PubMed'), ""))))
# 
# source('multiplot.R')
# multiplot(g1, g2)
# 
# # example for celine
# 
# x <- 
#   pubmed(start_year = 2015,
#          end_year = 2015,
#          search_topic = paste0('(ntds[Title/Abstract])'),
#          counts_only = FALSE)