# wrangle review data for articles related to pregnancy complications
# (for summer, focused on subset of articles related to maternal morbidity and mortality)
# and output two permanent tables:
# one table with article-level data (one row per article)
# and one table with model-level data (one row per "model")

library(tidyverse)

# -------------------- initial wrangling ------------------------------------

articles_reviewed_pregcomp <- readRDS("Data/articles_reviewed_pregcomp.Rds")
  
articles_pregcomp <- articles_reviewed_pregcomp %>%
  arrange(doi, timestamp) %>%
  # **** !!! *****
  # only keep first data entry (assuming, in end, the two will match)
  group_by(doi) %>%
  slice(1) %>%
  ungroup()

# -------------------- article-level table ------------------------------------
# create article-level dataset, including abstract, journal, authors, etc.

# for now, just merge with data already have in 'articles_identified' dataset
# later, try to get all authors 

articles_identified <- readRDS("Data/articles_identified.Rds")

articles_table_pregcomp <- articles_pregcomp %>%
  select(-starts_with(c("m1_", "m2_", "m3_", "m4_", "m5_", "m6_", "m7_", "m8_", "m9_")))  %>%
  # now pull in meta-data (title, journal, abstract, authors, publication date, etc.)
  inner_join(articles_identified, by="doi") %>%
  select(flagged, pmid, doi, title, journal, jabbrv, abstract, year, month, day, keywords
         , lastname, firstname, everything())
  
  
# ------------------------- model-level table ---------------------------------
# create model-level dataset (one row per "model") with "effect" estimates

effects_table_pregcomp <- articles_pregcomp %>%
  select(flagged, doi, starts_with(c("m1_", "m2_", "m3_", "m4_", "m5_", "m6_", "m7_", "m8_", "m9_"))) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols=-c(flagged,doi), names_to="field", values_to="value") %>%
  separate(field, into=c("model","field2"), sep="_", extra="merge") %>%
  # only keep non-missing values
  filter(!is.na(value) & value != "NULL") %>%
  pivot_wider(id_cols = c(flagged, doi, model), names_from = field2, values_from = value) %>%
  pivot_longer(cols=-c(flagged, doi, model, stratified, stratgrp, subanalysis, subgrp
                       , outcome, ref, measure, measure_other, covariates)
                       , names_to = "field", values_to = "value") %>%
  mutate(field3 = str_replace_all(field, "[:digit:]", "")
         , comparison = parse_number(field)) %>%
  pivot_wider(id_cols = c(flagged, doi, model, stratified, stratgrp, subanalysis, subgrp
                         , outcome, ref, measure, measure_other, covariates, comparison)
              , names_from = field3, values_from = value) %>%
  filter(!is.na(compare)) %>%
  mutate(#strat_sub = case_when(stratified == "Yes" ~ "Stratified"
         #                       , subanalysis == "Yes" ~ "Subanalyis"
         #                      , TRUE ~ "Neither")
         #, strat_sub_grp = case_when(stratified == "Yes" ~ stratgrp
         #                   #, subanalysis == "Yes" ~ subgrp
         #                   , TRUE ~ "n/a")
         stratgrp = case_when(stratgrp=="NULL" ~ NA_character_
                                , TRUE ~ stratgrp)
         , measure_clean = case_when(measure == "Other" ~ measure_other
                                     , TRUE ~ measure)
         , measure_comments = case_when(measure != "Other" & !is.na(measure_other) ~ measure_other
                                        , TRUE ~ "")
         , point = parse_number(point)
         , lower = parse_number(lower)
         , upper = parse_number(upper)
         , outcome = tolower(outcome)
         , covariates = tolower(covariates)
         , model = case_when(model=="m1" ~ "m1"
                             , model=="m9" ~ "m2"
                             , model=="m7" ~ "m3"
                             , model=="m8" ~ "m4"
                             , model=="m6" ~ "m5")) %>%
  select(#-stratified, -subanalysis, -stratgrp, -subgrp
         -measure, -measure_other) %>%
  rename(measure = measure_clean) %>%
  select(flagged, doi, model, stratified, stratgrp, subanalysis, subgrp
         , outcome, measure, measure_comments, covariates, comparison, ref, compare
         , point, lower, upper) %>%
  arrange(doi, model)


# ---------------------  save permanent tables --------------------------------

#saveRDS(articles_table_pregcomp, file="Data/articles_table_pregcomp.Rds")
#saveRDS(effects_table_pregcomp, file="Data/effects_table_pregcomp.Rds")

