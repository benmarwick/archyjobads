
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


#------------------------------------------------------
# scrape wiki page to test claims about'dead by 2018'

library(tidyverse)

# starting from : ratio-tt-to-non-tt

base_url_to_2019 <- "https://academicjobs.fandom.com/wiki/Archaeology_Jobs_"
base_url_after_2020 <- "https://academicjobs.fandom.com/wiki/Archaeology_"

# starts at 2010-2011
# fix for 2021-22
# base UR

years_to_2019 <- map_chr(2012:2019, ~str_glue('{.x}-{.x +1}'))
years_after_2020 <- map_chr(2020:2022, ~str_glue('{.x}-{.x +1}'))
# though it seems to start at 2007-8: https://academicjobs.fandom.com/wiki/Archaeology_07-08

# make a set of URLs for each page for each year
urls_for_each_year <- c(str_glue('{base_url_to_2019}{years_to_2019}'),
                        str_glue('{base_url_after_2020}{years_after_2020}')) %>%
  str_replace("2021-2022", "2021-22")

# get the history of edits page "?action=history&offset=&limit=10000"

edits_page <- "?action=history&offset=&limit=10000"

edits_urls_for_each_year <-
  str_glue('{urls_for_each_year}',
           '{edits_page}' )

library(rvest)

# make a table of key edit variables
edits_urls_for_each_year_lst_tbl <-
map(edits_urls_for_each_year,
    ~{
      # get webpage
      pge <- read_html(.x)

      # extract key variables for each edit
      # into a table
      tibble(
      edit_date =  pge %>%
        html_elements(".mw-changeslist-date") %>%
        html_text2(),
      edit_name =  pge %>%
        html_elements(".mw-userlink bdi") %>%
        html_text2(),
      edit_size =  pge %>%
        html_elements(".mw-plusminus-neg , .mw-plusminus-null , .mw-plusminus-pos") %>%
        html_text2()
      )
    }
)

# handle getting the edit comment which tells us which section
# of the page was edited, it's blank for many edits, but there
# might be some interesting patterns in there
edits_urls_for_each_year_lst_section <-
  map(edits_urls_for_each_year,
      ~read_html(.x) %>%
        html_elements('#pagehistory li') %>%
        html_text2()
  )

edits_urls_for_each_year_lst_section_tbl <-
map(edits_urls_for_each_year_lst_section,
    ~{
      # Extract time and date
      edit_date <-
        stringr::str_extract(.x, "\\d{2}:\\d{2}, \\d{1,2} [A-Za-z]+ \\d{4}")

      edit_size <- str_extract(.x, "bytes\\s([+-]?\\d+)") |>
         str_remove("bytes\\s")


      # Extract text between → and undo
      edit_comment <- stringr::str_extract(.x, "→(.*?) undo")
      edit_comment <- stringr::str_remove_all(edit_comment, "→| undo")  # clean up

      # Combine into a data frame
      tibble(edit_date,
             edit_size,
             edit_comment)
    }
)

# join together key variables and edit comments
edits_urls_for_each_year_lst_tbl_with_comments <-
map2(edits_urls_for_each_year_lst_tbl,
     edits_urls_for_each_year_lst_section_tbl,
    ~ left_join(.x, .y,
                relationship = "many-to-many") %>%
      # deduplicate
      distinct()
)

names(edits_urls_for_each_year_lst_tbl_with_comments) <-
  c(years_to_2019,
    years_after_2020)

# combine list into one big data frame
edits_for_each_year_tbl <-
bind_rows(edits_urls_for_each_year_lst_tbl_with_comments,
          .id = "year_ad_posted") %>%
  mutate(
    edit_size_num = parse_number(str_replace(edit_size, "−", "-")),
    edit_time = parse_date_time(edit_date,
                                 orders = "HM, d B Y"),
    # round down to first day of month
    edit_month = floor_date(edit_time,
                            unit = "month"),
    edit_year = floor_date(edit_month,
                           unit = "year"),
    edit_year_only = year(edit_month),
    job_market_year = parse_number(str_sub(year_ad_posted,
                              -4L))
  ) %>%
  # delete edits that were made after the job market year
  filter(edit_year_only == job_market_year | edit_year == (job_market_year -1))


# visualisations ----------------------------------

ggplot(edits_for_each_year_tbl) +
  aes(as_date(edit_month)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "", y = "Number of edits") +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

ggplot(edits_for_each_year_tbl) +
  aes(month(edit_month,
            label = TRUE,
            abbr = FALSE)) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(~ edit_year_only,
             ncol = 1) +
  labs(x = "", y = "Number of edits") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

ggplot(edits_for_each_year_tbl) +
  aes(edit_month) +
  geom_bar() +
  theme_minimal() +
  facet_wrap(~ edit_year_only,
             ncol = 1,
             scales = "free") +
  labs(x = "", y = "Number of edits") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

# Number of ediyear_ad_posted# Number of edits per year
p_edits_per_year <-
ggplot(edits_for_each_year_tbl) +
  aes(as_date(edit_year)) +
  geom_bar() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(x = "", y = "Number of edits") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

# Number of unique editors per year
p_editors_per_year <-
edits_for_each_year_tbl %>%
  group_by(edit_year) %>%
  summarise(n_distinct_editors = n_distinct(edit_name)) %>%
ggplot() +
  aes(as_date(edit_year),
      n_distinct_editors) +
  geom_col() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(x = "", y = "Number of editors\n(distinct usernames or IP addresses)") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

# number of edits per editor
edits_for_each_year_tbl %>%
  group_by(edit_name) %>%
  tally(sort = TRUE) %>%
  ggplot() +
  aes(n) +
  geom_histogram() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Number of edits",
       y = "Number of editors") +
  theme_minimal()

# Editor activity over time
top_editors_per_year <-
edits_for_each_year_tbl %>%
  # get top editors per year
  group_by(edit_year,
           edit_name) %>%
  summarise(n_edits = n(), .groups = "drop") %>%   # count edits
  arrange(edit_year,
          desc(n_edits)) %>%            # sort within year
  group_by(edit_year) %>%
  slice_max(order_by = n_edits, n = 3) %>%         # take top per year
  ungroup()

edits_for_each_year_tbl %>%
  filter(edit_name %in% top_editors_per_year$edit_name) %>%
  group_by(edit_name, edit_year) %>%
  summarise(n_edits = n()) %>%
  ggplot() +
  aes(as_date(edit_year),
      y = n_edits,
      colour = edit_name) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(x = "Year", y = "Number of edits")

# life span of an editor, from first edit to last edit
edits_for_each_year_tbl_span <-
edits_for_each_year_tbl %>%
  group_by(edit_name) %>%
  summarise(first_edit = min(edit_month),
            last_edit =  max(edit_month),
            edit_span = last_edit - first_edit,
            edit_span_years = as.numeric(edit_span,
                                         units = "days") / (365.25)) %>%
  mutate(first_edit_year = year(first_edit),
         last_edit_year =  year(last_edit))

#  year of every editor's first edit
ggplot(edits_for_each_year_tbl_span) +
 aes(first_edit_year) +
  geom_bar() +
  theme_minimal() +
  labs(x = "",
       y = "Number of editors making their first edit")


# Most editors are only active for less that one year
p_editor_life_distr <-
ggplot(edits_for_each_year_tbl_span) +
  aes(edit_span_years) +
  geom_histogram() +
  theme_minimal() +
  scale_y_log10() +
  labs(x = "Years editing", y = "Number of editors")

# for those editors active for more than three months:
p_editor_life_distr_year <-
edits_for_each_year_tbl_span %>%
 filter(edit_span_years > 0.25) %>%
ggplot() +
  aes(as_date(first_edit_year),
      reorder(edit_name, first_edit_year)) +
  geom_segment(aes(x =   first_edit,
                   xend = last_edit),
               linewidth = 1,
               lineend = "butt") +
  theme_minimal() +
  labs(x = "", y = "") +
  theme(axis.text.y = element_text(size = 4))

# size of edits per year
p_edit_size_per_year <-
edits_for_each_year_tbl %>%
  group_by(edit_year) %>%
  summarise(edit_size_num = sum(edit_size_num)) %>%
  ggplot() +
  aes(as_date(edit_year),
      edit_size_num) +
  geom_col() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(x = "", y = "Sum of edits (Bytes)") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

# typical edit size per year
p_median_edit_size_per_year <-
edits_for_each_year_tbl %>%
  group_by(edit_year) %>%
  # median, because sometimes there are 1-2 very large
  # page reformatting edits
  summarise(edit_size_num = median(edit_size_num)) %>%
  ggplot() +
  aes(as_date(edit_year),
      edit_size_num) +
  geom_col() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(x = "", y = "Median edit size (Bytes)") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))


# which sections get the most edits?
section_edit_tally_tbl <-
edits_for_each_year_tbl %>%
  mutate(edit_comment = str_squish(edit_comment)) %>%
  filter(!is.na(edit_comment)) %>%
  # remove all caps text at the end of the position name, those are status changes
  # in the same position
  mutate(edit_comment = gsub("\\s+[A-Z]+(?:\\s+[A-Z]+)*$", "", edit_comment)) %>%
  filter(!edit_comment %in% c("Current Users",
                              "DISCUSSION, RUMORS, SPECULATION",
                              "DISCUSSION, RUMORS AND SPECULATION",
                              "General Discussion, Rumors, and Speculation",
                              "Rejection Etiquette",
                              "Current Users Edit",
                              "Description",
                              "RESEARCH",
                              "TENURE",
                              "NON-TENURE-TRACK",
                              "ASSISTANT",
                              "TENURE-TRACK OR TENURED / FULL-TIME",
                              "Tenure-Track or Tenured / Full-time Position",
                              "Posting Names of Job-Getters",
                              "VISITING POSITIONS / LIMITED TERM APPOINTMENTS / POSTDOCS")) %>%
  filter(!str_detect(edit_comment, "Updated:|Wiki")) %>%
  group_by(edit_year,
           edit_comment) %>%
  tally(sort = TRUE)

# edits per section for each year
library(ggbeeswarm)
p_edit_number_per_entry_per_year <-
ggplot(section_edit_tally_tbl) +
  aes(as_date(edit_year),
      group = as_date(edit_year),
      n) +
  geom_boxplot() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(x = "", y = "Number of edits per job ad") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

library(cowplot)
plot_grid(
  plot_grid(p_edits_per_year,
          p_editors_per_year,
          p_median_edit_size_per_year,
          p_edit_number_per_entry_per_year,
          labels = c("A", "B", "D", "E")),
          plot_grid(p_editor_life_distr_year,
                    p_editor_life_distr,
                    rel_widths = c(2),
                    ncol = 1,
                    labels = c("C", "F"))
          )

#---------------------------------------------
# want a table to show values for fig 1

prop_by_job_title_per_year_tbl_wide <-
prop_by_job_title_per_year_tbl %>%
  select(-prop) %>%
  pivot_wider(names_from = job_title_simple,
              values_from = n)

prop_by_job_title_per_year_us_only_tbl <-
jobdata_us_only %>%
  group_by(year_ad_posted) %>%
  count(job_title_simple) %>%
  mutate(prop = n / sum(n)) %>%
  select(-prop) %>%
  pivot_wider(names_from = job_title_simple,
              values_from = n)

# combine tables with total (US) numbers

# 1. Join the two tables by year, adding suffixes to distinguish columns
combined_data <- left_join(
  prop_by_job_title_per_year_tbl_wide,
  prop_by_job_title_per_year_us_only_tbl,
  by = "year_ad_posted",
  suffix = c("_wide", "_us") # Suffixes for columns from each table
)

# 2. Identify the original value column names (excluding the join key)
value_cols <- setdiff(names(prop_by_job_title_per_year_tbl_wide), "year_ad_posted")

# 3. Mutate to create the combined columns
#    Iterate through the original value column names
for (col_name in value_cols) {
  # Construct the names of the columns from the joined table
  col_wide_name <- paste0(col_name, "_wide")
  col_us_name <- paste0(col_name, "_us")

  # Use mutate to create a new column (overwriting the original name)
  # with the combined string "Value_Wide (Value_US)"
  # Handle NA values by replacing them with a placeholder like "-"
  combined_data <- combined_data %>%
    mutate(
      !!col_name := paste0(
        # Coalesce NA to "-" before pasting
        coalesce(as.character(!!sym(col_wide_name)), "-"),
        " (",
        coalesce(as.character(!!sym(col_us_name)), "-"),
        ")"
      )
    )
}

# 4. Select only the year column and the newly created combined columns
final_combined_table <- combined_data %>%
  select(year_ad_posted, all_of(value_cols))

# let's add TT and NTT columns to that
count_of_tt_jobs_per_year_from_our_form %>%
  select(year_ad_posted = year,
         TT = n_tt_jobs,
         NTT = n_non_tt_jobs) %>%
  left_join(final_combined_table) %>%
  relocate(TT, NTT, .after = last_col()) %>%
  relocate(`Full Professor`, .after = `Associate Professor`) %>%
  rename( `Year ad posted` = year_ad_posted) %>%
  # get numeric year, sort, then remove it
  mutate(year_num = parse_number(str_sub(`Year ad posted`, 6L))) %>%
  arrange(year_num) %>%
  select(-year_num)

#---------------------------------------------------------------
# can we get that table of 'current users'

# 2021-22 and 2022-2023
current_user_tbl <-
map(
urls_for_each_year[1:9],
~.x %>%
  read_html() %>%
  html_elements("p+ table td") %>%
  html_text2()
)

current_user_tbl_2021_22 <-
    urls_for_each_year[10]  %>%
      read_html() %>%
      html_elements("table:nth-child(1086) td") %>%
      html_text2()

current_user_tbl_2022_2023 <-
    urls_for_each_year[11] %>%
      read_html() %>%
      html_elements("h2+ .fandom-table td") %>%
      html_text2()

current_user_tbl_all <- append(current_user_tbl,
                        c(list(current_user_tbl_2021_22),
                          list(current_user_tbl_2022_2023)
                        ))

names(current_user_tbl_all) <- urls_for_each_year

names(current_user_tbl_all)[names(current_user_tbl_all) == "https://academicjobs.fandom.com/wiki/Archaeology_2021-22"] <- "https://academicjobs.fandom.com/wiki/Archaeology_2021-2022"

wiki_data_list <-
map(current_user_tbl_all,
    ~.x %>% .[1:40] %>%
    stringi::stri_remove_empty(.x))

# Use map_dfr to iterate through the list and its names, binding results
tidy_table <- map_dfr(wiki_data_list, ~{
  # Process one vector (.x) at a time
  tibble(raw = .x) %>%
    # Create a lagged column to easily access the previous element
    mutate(
      previous_raw = lag(raw),
      # Attempt to parse the current value as a number
      # suppressWarnings handles cases where it's clearly not numeric
      # !is.na checks if parsing was successful
      is_count = !is.na(suppressWarnings(readr::parse_number(raw))),
      # Check if the *previous* value looks like a number
      prev_is_count = !is.na(suppressWarnings(readr::parse_number(previous_raw)))
    ) %>%
    # Keep rows where:
    # 1. The current row IS a count (is_count == TRUE)
    # 2. The previous row exists (previous_raw is not NA)
    # 3. The previous row IS NOT a count (prev_is_count == FALSE)
    #    This ensures we grab the status text, not a preceding number
    filter(is_count & !is.na(previous_raw) & !prev_is_count) %>%
    # Select and rename the relevant columns, converting count to numeric
    transmute(
      status = previous_raw,
      count = readr::parse_number(raw) # Convert count string to number
    )
}, .id = "url") %>% # .id creates a column 'url' from the list names
  # Extract the year from the URL
  mutate(
    year = str_extract(basename(url), "\\d{4}-\\d{4}"), # Extracts YYYY-YYYY pattern
    .before = status # Place the year column before status
  ) %>%
  select(year, status, count) # Keep only the final desired columns

#  the resulting tidy table
tidy_table %>%
  filter(!status %in% c("I'm a lurker",
                        "I have a current but term-limited faculty job",
                        "I have some other teaching, research, or non-academic gig",
                        "Nope (independent scholar, on sabbatical from adjuncting, etc.)")) %>%
  mutate(year = str_replace(year, "-", "-\n")) %>%
  ggplot() +
  aes(year,
      count) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~ status,
             scales = "free") +
  xlab("") +
  theme(axis.text.x = element_text(size = 6))

#-----------------------------------------------
# can we get 'discussion' section

xpath_selector <- "//h2[span/@id='DISCUSSION,_RUMORS,_SPECULATION']/following-sibling::*[count(preceding-sibling::h2[span/@id='DISCUSSION,_RUMORS,_SPECULATION']) = 1]"

discussion_section <-
  map(
    urls_for_each_year,
    ~read_html(.x) %>%
      html_nodes(xpath = xpath_selector) %>%
      html_text2() %>%
      str_squish()
  )

names(discussion_section) <- urls_for_each_year

#----------------------------------------------
# can we geolocate IP addresses?















