# identify articles for screening based on journal, date, and key terms

library(easyPubMed)
library(tidyverse)

# resource: http://www.biotechworld.it/bioinf/2016/01/05/querying-pubmed-via-the-easypubmed-package-in-r/ 

journals <- c("American journal of obstetrics and gynecology"
              , "Human reproduction (Oxford, England)"
              , "Human reproduction update"
              , "Fertility and sterility"
              , "Obstetrics and gynecology"
              , "Gynecologic oncology"
              , "Ultrasound in obstetrics gynecology : the official journal of the International Society of Ultrasound in Obstetrics and Gynecology"
              , "BJOG : an international journal of obstetrics and gynaecology")


terms <- c("racial disparity", "racial bias", "racism", "discrimination", 
            "minority", "black-white", "white-black", 
            "ethnic disparity", "prejudice", "inequality", "inequity", 
            "healthcare disparity",
            "racial disparities", "racial biases",  
            "minorities", 
            "ethnic disparities", "prejudices", "inequalities", "inequities", 
            "healthcare disparities")


query_function <- function(journals, start_year, terms){
  
  journals_real <- paste('("',journals,'"[Journal])', collapse = ' OR ')

  date_query <- paste0('(("',start_year,'/01/01"[Date - Publication] : "2021/06/15"[Date - Publication]))')

  terms_real <- paste0(terms, collapse= " OR ")

  query_string <- paste(# journals
                       '(', journals_real, ') AND '
                        # dates
                        , date_query, ' AND '
                       # terms
                        , '(', terms_real, ')')
    
  # searching for articles that match query string
  my_query <- get_pubmed_ids(pubmed_query_string = query_string) 
  
  #my_query$Count # number of search results, just for checking;
  
  my_data <- fetch_pubmed_data(my_query, retmax=9999) 
  
  # Outputs table w/ all of desired information 
  # right now is only set to first author but can change to ALL authors
  allFields <- table_articles_byAuth(my_data, included_authors = "first"
                                     , getKeywords = TRUE
                                     , max_chars = 5000) 
  
  return(allFields)
  
}

articles_identified <- query_function(journals=journals, start_year="2010", terms=terms)

# SAVE as permanent file so have record of articles screened
#saveRDS(articles_identified, file = "Data/articles_identified.Rds")

#max(nchar(articles_identified$abstract), na.rm=TRUE)
#maxed_out<-which(nchar(articles_identified$abstract)==5000)
#articles_identified$journal[maxed_out]
#articles_identified$abstract[maxed_out]
