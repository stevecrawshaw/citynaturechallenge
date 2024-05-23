pacman::p_load(
tidyverse,
janitor,
glue,
fs
)

# Load the data: read all cols as character
raw_inat_tbl <- fs::dir_map("data/inatfiles",
fun = \(x) read_csv(x,
  col_types = cols(.default = "c")
)
) %>%
bind_rows()

# change data types, create categories for licenses and make categories consistent
cleaned_inat_tbl <- raw_inat_tbl %>%
  filter(!is.na(license)) %>% # filter out obs with no licence
mutate(
  observed_on_string = parse_date_time(observed_on_string, orders = c("ymdHMS", "ymdHMSz")),
  observed_on = as.Date(observed_on),
  across(
    ends_with("at"),
    ~ parse_date_time(.x, orders = c("ymdHMS", "ymdHMSz"))
  ),
  across(starts_with("num"), as.integer),
  across(ends_with("tude"), as.numeric),
  across(ends_with("accuracy"), as.numeric),
  year_ons_on = year(observed_on),
  year_obs_time_at = year(time_observed_at),
  # if year_ons_on is NA, use year_obs_time_at, if that is NA, use created_at
  year_cons = coalesce(year_ons_on, year_obs_time_at, year(created_at)),
  license_cat = if_else(license == "CC-BY" | license == "CC0",
                        "OPEN (CC0 or CC-BY)",
                        "NOT OPEN"),
  captive_cultivated = toupper(captive_cultivated)) %>%
arrange(year_cons)



make_tabyl <- function(tbl = cleaned_inat_tbl,
                     filter_casual,
                     participants) {

  if (filter_casual) {
  
  out_tbl <- tbl %>%
    filter(quality_grade != "casual")
} else {
  out_tbl <- tbl
}

if (participants) {
  # count unique users and calculate percentage of open licences for those unique users
  out_tbl %>%
    group_by(year_cons, user_id) %>%
    summarise(count_open = sum(license_cat == "OPEN (CC0 or CC-BY)"),
              count_not_open = sum(license_cat == "NOT OPEN")) %>% 
    summarise(unique_user_count = n_distinct(user_id),
              perc_open = mean(count_open / (count_open + count_not_open) * 100) %>%
                round(1),
      participants_records = "participants",
      research_grade = if_else(filter_casual == FALSE,
        "all",
        "exclude_casual"
      ))
} else {
  out_tbl %>%
    tabyl(var1 = year_cons, var2 = license_cat) %>%
    clean_names() %>%
    mutate(
      perc_open = (open_cc0_or_cc_by / (open_cc0_or_cc_by + not_open) * 100) %>%
        round(1),
      participants_records = "records",
      research_grade = if_else(filter_casual == FALSE,
        "all",
        "exclude_casual"
      )
    )

}
}

# helper function to create filename from data in the tbl
writefile_name <- function(tbl) {
pr <- unique(tbl$participants_records)
rg <- unique(tbl$research_grade)
fname <- glue("data/summary_{pr}_{rg}_.csv")
write_csv(tbl, file = fname)
print(glue("Wrote file {fname}"))
}

make_tabyl_partial <- partial(make_tabyl, tbl = cleaned_inat_tbl)

filter_casual <- c(TRUE, FALSE)
participants <- c(TRUE, FALSE)
arg_tbl <- expand.grid(filter_casual = filter_casual, participants = participants)

outlist <- pmap(arg_tbl, make_tabyl_partial) 

map(outlist, ~writefile_name(.x))


# export ODS table

cleaned_inat_tbl %>%
select(-c(user_login,
          user_name, 
          oauth_application_id,
          private_latitude,
          private_longitude,
          year_ons_on, 
          year_obs_time_at)) %>%
write_csv(glue("data/cnc_{Sys.Date()}.csv"), na = "")
