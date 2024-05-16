pacman::p_load(
  tidyverse,
  janitor,
  scales,
  glue,
  fs
)


# Load the data: read all cols as character
raw_inat_tbl <- fs::dir_map("data",
  fun = \(x) read_csv(x,
    col_types = cols(.default = "c")
  )
) %>%
  bind_rows()

# change data types, create categories for licenses and make categories consistent
cleaned_inat_tbl <- raw_inat_tbl %>%
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
    year_cons = coalesce(year_ons_on, year_obs_time_at),
    license_cat = if_else(license == "CC-BY" | license == "CC0", "OPEN (CC0 or CC-BY)", "NOT OPEN"),
    captive_cultivated = toupper(captive_cultivated)
  ) %>%
  arrange(year_cons)

cleaned_inat_tbl %>% view()

make_tabyl <- function(tbl = cleaned_inat_tbl,
                       filter_casual = FALSE,
                       participants = FALSE) {
  if (filter_casual) {
    out_tbl <- tbl %>%
      filter(quality_grade != "casual")
  } else {
    out_tbl <- tbl
  }

  if (participants) {
    out_tbl %>%
      group_by(year_cons, license_cat) %>%
      count(user_id) %>%
      pivot_wider(
        id_cols = year_cons,
        names_from = license_cat,
        values_from = n, values_fn = sum
      ) %>%
      clean_names() %>%
      mutate(
        total = open_cc0_or_cc_by + not_open + na,
        perc_open = (open_cc0_or_cc_by * 100 / total) %>% round(1),
        participants_records = "participants",
        research_grade = if_else(filter_casual == FALSE,
          "all",
          "not_casual"
        )
      )
  } else {
    out_tbl %>%
      tabyl(var1 = year_cons, var2 = license_cat) %>%
      clean_names() %>%
      adorn_percentages("row") %>%
      adorn_pct_formatting() %>%
      mutate(
        participants_records = "records",
        research_grade = if_else(filter_casual == FALSE,
          "all",
          "not_casual"
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
}

# export summaries

cleaned_inat_tbl %>%
  make_tabyl(
    filter_casual = FALSE,
    participants = TRUE
  ) %>%
  writefile_name()

cleaned_inat_tbl %>%
  make_tabyl(
    filter_casual = FALSE,
    participants = FALSE
  )

# count no licence

cleaned_inat_tbl %>%
  tabyl(var1 = license_cat, var2 = year_cons)

# export ODS table

cleaned_inat_tbl %>%
  select(-c(user_login, user_name, oauth_application_id, private_latitude, private_longitude, year_ons_on, year_obs_time_at)) %>%
  filter(!is.na(license_cat)) %>% # filter out obs with no licence
  write_csv("data/cnc_2018-2024.csv", na = "")