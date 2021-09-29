# assign two people to screen each article
# output a separate google sheet for each screener with their assigned articles

library(tidyverse)
library(googlesheets4)

# ------------------------- Assignments ---------------------- 

articles_identified <- readRDS(file = "Data/articles_identified.Rds") 
reviewers <- c("AB","GR","JW","KC","MJ","TH")

set.seed(2021)

p <- length(reviewers)  # total number of reviewers
n <- nrow(articles_identified)  # total number of articles

articles <- articles_identified %>%
  arrange(desc(year),desc(month),desc(day)) %>%
  select(pmid, doi, journal, title, abstract)

# create a random number for each row
random <- sample(1:n, replace = FALSE, n)

# divide the random number by # of articles that should be read per person. 
# This creates balanced assignments for the first readers
articles$reader1 <- ceiling(random / (n / p))

# For second readers, also want to randomly assign but ensure assignment
# isn't same as second reader, and that each reader is assigned similar number of articles
v2 <- articles$reader1

articles$reader2 <- 0
for (i in 1:n){
  # remove the one assigned from vector each time so not over-sampling any
  if(i!=1){
    v2 <- v2[-which(v2==articles$reader2[i-1])[1]]
  }
  # sample from remaining vector, and among values not equal to reader1
  articles$reader2[i] <- sample(x=v2[which(v2!=articles$reader1[i])]
                                , size=1)
}

screening_assignments <- articles %>%
  left_join(data.frame(reader1=1:6,reader1init=reviewers),by="reader1" )%>%
  left_join(data.frame(reader2=1:6,reader2init=reviewers),by="reader2") 

screening_assignments %>% count(reader1init)
screening_assignments %>% count(reader2init)

# SAVE as permanent file so have record of screening assignments
#saveRDS(screening_assignments, file = "Data/screening_assignments.Rds")


# ------------------------- Google sheets ---------------------- 

#screening_assignments <- readRDS("Data/screening_assignments.Rds")

# Create a separate google sheet for each reviewer to screen their set of
# assigned articles
for (i in 1:length(reviewers)){
  screen_dat <- screening_assignments %>%
    filter(reader1init==reviewers[i] | reader2init==reviewers[i]) %>%
    mutate(eligible = NA, reason_ineligible = NA, other_comments = NA) %>%
    select(-reader1,-reader2,-reader1init,-reader2init)
  
  # append reviewer initials to file name
  gs_file_name <- paste0("article_screening_",reviewers[i])
  
  # create google sheet
  gs4_create(gs_file_name, sheets=screen_dat)
}
