
set_of_bigrams <- c("material culture",
                    "resource management",
                    "environmental archaeology")

jobdata$topical_focus_of_position

jobdata$topic_bigram <-
str_match(jobdata$topical_focus_of_position,
          paste0(set_of_bigrams, collapse = "|"))[ , 1]

# check it
jobdata$topical_focus_of_position[1:5]


#----------------------------------------------------

lookup_table <-
  tribble(~"Category", ~"From the data",
"Africa", "Africa; sub-Saharan Africa; North Africa",
"Americas", "Western Hemisphere; New World",
"Northeastern US", "The Northeast; New York; Pennsylvania; Middle Atlantic; the mid-Atlantic; the Northeast or southeastern Canada; Arctic-North Atlantic region; the Atlantic world; the Chesapeake region; the greater Northeast (Quebec and neighboring regions, including the eastern Subarctic and Arctics, Atlantic provinces, northwestern United States, and lower Great Lakes); North Atlantic; The Northeastern United States; Northeast; the Northeastern U.S.; the Mid-Atlantic; the North Atlantic")

lookup_table_long <-
lookup_table %>%
  separate_longer_delim(`From the data`, ";") %>%
  mutate(`From the data` = str_squish(`From the data`))

jobdata$geographic_focus_of_position

# getting only the first match of geographic entity
jobdata$geo_first_match <-
str_match(jobdata$geographic_focus_of_position,
          paste0(lookup_table_long$`From the data`, collapse = "|"))[ , 1]


jobdata <-
left_join(jobdata,
          lookup_table_long,
          by = join_by(geo_first_match == `From the data`))
