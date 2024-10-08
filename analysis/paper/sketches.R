
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

1+1

jobdata %>%
  select(starts_with("documents_requested")) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  tally() %>%
  mutate(value = case_when(
    value == "Not requested in the job ad" ~ 0,
    value == "One" ~ 1,
    value == "Two (e.g. two syllabi)" ~ 2,
    value == "Three" ~ 3,
    .default = 0
  )) %>%
  ggplot() +
  aes(value,
      n) +
  geom_col() +
  facet_wrap( ~ name)


#-----------------------------------------------------------------------------

# Draw of map to show which states have done the most hiring in our sample

# get the text in parentheses after the university name that gives the
# state or country abb
uni_state_country <-
jobdata %>%
 select(name_of_hiring_university) %>%
  mutate(state_country = regmatches(name_of_hiring_university,
                                    gregexpr( "(?<=\\().+?(?=\\))",
                                              name_of_hiring_university,
                                              perl = T))) %>%
  unnest(state_country)

# tally to get counts:
uni_state_country_tally <-
uni_state_country %>%
  group_by(state_country) %>%
  tally(sort = TRUE)

# did we get all the job ads?
sum(uni_state_country_tally$n) # 547 most of them, there are 552 ads in our data

state_to_st <- function(x){
       c(state.abb, 'DC')[match(x, c(state.name, 'District of Columbia'))]
}

state_name_and_abb <-
enframe(state.name, value = 'state_name') %>%
       mutate(state_abbr = state_to_st(state_name))

# filter to get US states only
uni_state_country_tally_us <-
uni_state_country_tally %>%
  filter(state_country %in% state.abb) %>%
  select(state_abbr = state_country, n) %>%
  # make sure we have all states in the dataframe
  # even those with no jobs
  right_join(state_name_and_abb) %>%
  select(state = state_name, n, state_abbr) %>%
  mutate(state = tolower(state)) %>%
  mutate(n = ifelse(is.na(n), 0, n))

library(ggplot2)
library(fiftystater)
library(tidyverse)
library(ggrepel)

data("fifty_states")

ggplot(data= uni_state_country_tally_us,
       aes(map_id = state)) +
  geom_map(aes(fill = n),
           color= "black",
           linewidth = 0.1,
           map = fifty_states) +
  expand_limits(x = fifty_states$long,
                y = fifty_states$lat) +
  coord_map() +
  geom_text_repel(data = fifty_states %>%
              group_by(id) %>%
              summarise(lat = mean(c(max(lat), min(lat))),
                        long = mean(c(max(long), min(long)))) %>%
              mutate(state = id) %>%
              left_join(uni_state_country_tally_us,
                        by = "state"),
            aes(x = long,
                y = lat,
                label = n,
                bg.color = "white",
                bg.r = 0.1),
            force = 0,
            force_pull = 100)+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "",
       y = "") +
  theme(legend.position = "bottom",
        panel.background = element_blank()) +
  scale_fill_viridis_c()


ggsave(here("analysis",
            "figures",
            "fig-us-state-map.png"),
       bg ="white",
       h = 10, # experiment with h and w to get the right size and proportion
       w = 12,
       units = "in",
       dpi = 900) # make the image nice and crisp))
























