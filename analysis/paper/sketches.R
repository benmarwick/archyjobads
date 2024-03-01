
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


#---------------------------------------------------


# what are the different sections? I reviewed every year to see what
# headings were used to indicate TT and non-TT jobs
tt_sections <- c("TENURE-TRACK POSITIONS",
                 "TENURE-TRACK OR TENURED / FULL-TIME POSITIONS",
                 "Tenure-Track or Tenured / Full-time Position",
                 "ASSISTANT PROFESSOR OR OPEN RANK",
                 "TENURE TRACK ASSISTANT PROFESSOR OR OPEN RANK",
                 "TENURED ASSOCIATE OR FULL PROFESSOR",
                 "ASSOCIATE OR FULL PROFESSOR")

non_tt_sections <- c("NON-TENURE-TRACK POSITIONS",
                     "VISITING POSITIONS / Limited-Term Appointments / Postdocs",
                     "Visiting Positions / Limited-Term Appointments / Postdocs",
                     "VISITING POSITIONS / LIMITED TERM APPOINTMENTS / POSTDOCS",
                     "VISITING POSITIONS / LIMITED-TERM APPOINTMENTS / POSTDOCS / PART-TIME POSITIONS",
                     "VISITING POSITIONS")

end_non_tt_sections <- c("DISCUSSION, RUMORS AND SPECULATION",
                         "DISCUSSION, RUMORS, SPECULATION",
                         "General Discussion, Rumors, and Speculation" )

# what to do about this?
# "COMPLETED SEARCHES"

# this is a function that extracts text from each section with a heading
# that matches those above, and counts how many positions are in each section
library(stringr)
get_counts_tt_non_tt <- function(x){

  tt_start      <- which(str_detect(x,
                                    paste(tt_sections,
                                          collapse = "|")))[1]
  non_tt_start   <- which(str_detect(x,
                                     paste(non_tt_sections,
                                           collapse = "|")))[1]
  non_tt_end    <- which(str_detect(x,
                                    paste(end_non_tt_sections,
                                          collapse = "|")))[1]
  n_tt_jobs     <- length( x[(tt_start + 1) : (non_tt_start - 1) ] )
  n_non_tt_jobs <- length( x[(non_tt_start + 1) : (non_tt_end  - 1) ] )

  return(list(n_tt_jobs = n_tt_jobs,
              n_non_tt_jobs = n_non_tt_jobs))
}

# calculate ratio
ratios_of_tt_to_non_tt_jobs <-
  map_df(urls_for_each_year_headers,
         get_counts_tt_non_tt) %>%
  mutate(ratio = n_tt_jobs / n_non_tt_jobs) %>%
  mutate(year = str_replace(str_sub( urls_for_each_year, -9),
                            "-", "-\n"))

#----------------------------------------

jobdata$title_of_position_tenure_track_jobs_only

we_care_about <-
  c("Assistant", "Asst.",
    "Associate",
    "Full",
    "Open rank"
    )

jobdata <-
jobdata %>%
  # simplify rank descriptions
  mutate(title_of_position_tenure_track_jobs_only = tolower(title_of_position_tenure_track_jobs_only)) %>%
  mutate(job_title_simple = case_when(
   str_detect(title_of_position_tenure_track_jobs_only,
              "assistant prof|asst. prof|asst prof") ~ "Assistant Professor",
   str_detect(title_of_position_tenure_track_jobs_only,
              "associate prof|assoc. prof") ~ "Associate Professor",
   str_detect(title_of_position_tenure_track_jobs_only,
              "full prof") ~ "Full Professor",
   str_detect(title_of_position_tenure_track_jobs_only,
              "assistant or associate prof|assistant/associate prof") ~ "Assistant or Associate Professor",
   str_detect(title_of_position_tenure_track_jobs_only,
              "open rank|open-rank|assistant, associate, or full prof|assistant prof, associate prof, or prof") ~ "Open Rank",
   .default = "other"))

# explore over time
jobdata %>%
  group_by(year_ad_posted) %>%
  count(job_title_simple) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot() +
  aes(year_ad_posted,
      prop,
      group = job_title_simple,
      colour = job_title_simple) +
  geom_line()

#-----------------------------------------------------------------------------

# topic by year





#-----------------------------------------------------------------------------

# geographic focus by year

library(googlesheets4)

geographic_foci <-
read_sheet("https://docs.google.com/spreadsheets/d/1AHq49pIyChcgJ7rawe6KMWkdIBXydCamvg8Jslob8Ec/edit#gid=0")

geographic_foci_clean <-
  map(
   str_split(geographic_foci$`From the data`, ";"),
   ~.x %>%
     str_squish() %>%
     stri_remove_empty())

jobdata_geo <-
  jobdata %>%
  select(geographic_focus_of_position)

jobdata_geo <-
  # add one column for each geo region in our categories
cbind(jobdata_geo,
      setNames( lapply(geographic_foci$Category, function(x) x=NA),
                geographic_foci$Category) )

for(i in 1:length(geographic_foci$Category)){

  this_location <- geographic_foci$Category[i]

  # create the pattern to search for
  x <- paste0(geographic_foci_clean[[i]], collapse = "|")

  # do the search through all the job ads for that pattern
  y <- str_detect(jobdata_geo$geographic_focus_of_position,
             x)

  # assign back to our data frame in the appropiate location column
  jobdata_geo[, this_location] <- y

}
























