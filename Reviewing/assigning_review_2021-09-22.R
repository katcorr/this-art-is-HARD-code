# Fall 2021: new group of student interns will continue reviewing articles
# assign two reviewers to review each remaining article (that wasn't reviewed in summer)
# that was deemed eligible for inclusion
# output a separate google sheet for each reviewer with their assigned articles

# ------------------------- Assignments ---------------------- 

library(tidyverse)
library(googlesheets4)

# identify articles screened as eligible for inclusion
articles_screened <- readRDS(file = "Data/articles_screened.Rds")

# identify articles already reviewed to exclude
# some of these articles may still need second review; but for Fall 2021 semester,
# prioritize articles without any data entered
articles_reviewed <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1aZqj4n0It-bhIMRyOVb1oKalEX0EhOF5Q1w_oRQ0aKg/edit?usp=sharing") %>%
  janitor::clean_names() %>%
  select(doi, everything())

#length(unique(articles_reviewed$doi))

# Fall 2021 reviewer initials
reviewers <- c("AL","GR","KK","MJ","TH")

# extra check to ensure screeners match
#articles_screened %>% filter(eligible_s1 != eligible_s2)

articles_to_review <- articles_screened %>%
  # only keep articles screened by both screeners as eligible
  filter(eligible_s1=="Yes" & eligible_s2=="Yes") %>%
  # remove DOIs already reviewed
  filter(!(doi %in% articles_reviewed$doi)) %>%
  select(pmid, doi, journal, title, abstract)

set.seed(2021)

p <- length(reviewers)  # total number of reviewers
n <- nrow(articles_to_review)  # total number of articles

# create a random number for each row
random <- sample(1:n, replace = FALSE, n)

# divide the random number by # of articles that should be read per person. 
# This creates balanced assignments for the first readers
articles_to_review$reader1 <- ceiling(random / (n / p))

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
#saveRDS(review_assignments, file = "Data/review_assignments_2021-09-22.Rds")


# ------------------------- Google sheets ---------------------- 

#review_assignments <- readRDS("Data/review_assignments_2021-09-22.Rds")

# Create a separate google sheet for each reviewer to screen their set of
# assigned articles
for (i in 1:length(reviewers)){
  review_dat <- review_assignments %>%
    filter(reader1init==reviewers[i] | reader2init==reviewers[i]) %>%
    mutate(reviewed=NA) %>%
    select(-reader1,-reader2,-reader1init,-reader2init)
  
  # append reviewer initials to file name
  gs_file_name <- paste0("article_review_fall21_",reviewers[i])
  
  # create google sheet
  gs4_create(gs_file_name, sheets=review_dat)
}
