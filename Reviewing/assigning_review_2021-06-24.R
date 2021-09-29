# assign two reviewers to review each article that was deemed eligible for inclusion
# output a separate google sheet for each reviewer with their assigned articles

# ------------------------- Assignments ---------------------- 

library(tidyverse)
library(googlesheets4)

articles_screened <- readRDS(file = "Data/articles_screened.Rds") 
reviewers <- c("AB","GR","JW", "JY", "KC","MJ","TH")

# extra check to ensure screeners match
#articles_screened %>% filter(eligible_s1 != eligible_s2)

articles_to_review <- articles_screened %>%
  filter(eligible_s1=="Yes" & eligible_s2=="Yes") %>%
  select(pmid, doi, journal, title, abstract)

set.seed(2021)

p <- length(reviewers)  # total number of reviewers
n <- nrow(articles_to_review)  # total number of articles

# create a random number for each row
random <- sample(1:n, replace = FALSE, n)

# divide the random number by # of articles that should be read per person. 
# This creates balanced assignments for the first readers
articles_to_review$reader1 <- ceiling(random / (n / p))
# fix one being assigned to 8
articles_to_review$reader1[which(articles_to_review$reader1==8)] <- 1

# assign second reader among reader-spots that remain and aren't assigned to 
# reader 1
v2 <- articles_to_review$reader1
articles_to_review$reader2 <- 0
for (i in 1:n){
  # remove the one assigned from vector each time so not over-sampling any
  if(i!=1){
    v2 <- v2[-which(v2==articles_to_review$reader2[i-1])[1]]
  }
  # sample from remaining vector, and among values not equal to reader1
  articles_to_review$reader2[i] <- sample(x=v2[which(v2!=articles_to_review$reader1[i])]
                                , size=1)
}

review_assignments <- articles_to_review %>%
  left_join(data.frame(reader1=1:p,reader1init=reviewers),by="reader1" )%>%
  left_join(data.frame(reader2=1:p,reader2init=reviewers),by="reader2") 

review_assignments %>% count(reader1init)
review_assignments %>% count(reader2init)

# SAVE as permanent file so have record of review assignments
#saveRDS(review_assignments, file = "Data/review_assignments.Rds")


# ------------------------- Google sheets ---------------------- 

#review_assignments <- readRDS("Data/review_assignments.Rds")

# Create a separate google sheet for each reviewer to screen their set of
# assigned articles
for (i in 1:length(reviewers)){
  review_dat <- review_assignments %>%
    filter(reader1init==reviewers[i] | reader2init==reviewers[i]) %>%
    mutate(reviewed=NA) %>%
    select(-reader1,-reader2,-reader1init,-reader2init)
  
  # append reviewer initials to file name
  gs_file_name <- paste0("article_review_",reviewers[i])
  
  # create google sheet
  gs4_create(gs_file_name, sheets=review_dat)
}
