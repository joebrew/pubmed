# Get area: From an abstract, guess the area
get_area <- function(abstract = 'malaria',
                     classify = NULL){
  # Make sure the abstract is character
  abstract <- as.character(abstract)
  # Establish a dataframe for storing results
  df <- data.frame(area = c('malaria', 
                            'tuberculosis',
                            'hiv'),
                   n = NA)
  # Split the abstract into words
  abstract_words <- strsplit(abstract, ' ')
  # Remove commas, periods, colons, etc.
  abstract_words <- sapply(abstract_words, function(x){
    x <- gsub('.', '', x, fixed = TRUE)
    x <- gsub(',', '', x, fixed = TRUE)
    x <- gsub(':', '', x, fixed = TRUE)
    x <- gsub(';', '', x, fixed = TRUE)
    x <- gsub(',', '', x, fixed = TRUE)
    x <- tolower(x)
  })
  # Go through each disease to get the score -----------
  
  # malaria
  df$n[df$area == 'malaria'] <- 
    length(which(abstract_words =='malaria')) +
    length(which(abstract_words =='malarial')) +
    length(which(abstract_words =='anapholes')) +
    length(which(abstract_words =='gambiae'))
  
  # tuberculosis
  df$n[df$area == 'tuberculosis'] <- 
    length(which(abstract_words =='tuberculosis')) +
    length(which(abstract_words =='tb'))
  
  # hiv
  df$n[df$area == 'hiv'] <- 
    length(which(abstract_words =='hiv')) +
    length(which(abstract_words =='aids')) +
    length(which(abstract_words =='sida')) +
    length(which(abstract_words =='immunodeficiency'))
  # -------------------
  if(!is.null(classify)){
    return(df$n[df$area == classify] > 0)
  } else {
    return(df)
  }
}