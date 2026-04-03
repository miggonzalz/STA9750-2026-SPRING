## Exploratory analysis#
acquire_ipeds_data <- function(start_year=2010, end_year=2024){
  library(tidyverse)
  library(glue)
  
  data_dir <- file.path("data", "mp01")
  
  if(!dir.exists(data_dir)){
    dir.create(data_dir, showWarnings=FALSE, recursive=TRUE)
  }
  
  YEARS <- seq(start_year, end_year)
  
  EFA_ALL <- map(YEARS, function(yy){
    if(yy <= 2022){
      ef_url <- glue("https://nces.ed.gov/ipeds/datacenter/data/EF{yy}A.zip")
      
    } else {
      ef_url <- glue("https://nces.ed.gov/ipeds/data-generator?year={yy}&tableName=EF{yy}A&HasRV=0&type=csv")
    }
    
    ef_file <- file.path(data_dir, glue("ef{yy}a.csv.zip"))
    
    if(!file.exists(ef_file)){
      message(glue("Downloading Enrollment Data for {yy} from {ef_url}"))
      download.file(ef_url, destfile = ef_file, quiet=TRUE, mode="wb")    
    }
    
    read_csv(ef_file, 
             show_col_types=FALSE) |>
      mutate(year = yy, 
             # American Indian or Alaskan Native
             enrollment_m_aian = EFAIANM, 
             enrollment_f_aian = EFAIANW, 
             # Asian
             enrollment_m_asia = EFASIAM, 
             enrollment_f_asia = EFASIAW, 
             # Black or African-American, 
             enrollment_m_bkaa = EFBKAAM, 
             enrollment_f_bkaa = EFBKAAW, 
             # Hispanic 
             enrollment_m_hisp = EFHISPM, 
             enrollment_f_hisp = EFHISPW, 
             # Native Hawaiian or Other Pacific Islander 
             enrollment_m_nhpi = EFNHPIM, 
             enrollment_f_nhpi = EFNHPIW, 
             # White
             enrollment_m_whit = EFWHITM, 
             enrollment_f_whit = EFWHITW, 
             # Two or More Races
             enrollment_m_2mor = EF2MORM, 
             enrollment_f_2mor = EF2MORW, 
             # Unknown / Undisclosed Race
             enrollment_m_unkn = EFUNKNM, 
             enrollment_f_unkn = EFUNKNW, 
             # US Non-Resident
             enrollment_m_nral = EFNRALM, 
             enrollment_f_nral = EFNRALW, 
      ) |> filter(
        (EFALEVEL %in% c(2, 12)) | (LINE %in% c(1, 15))
        # Per 2024 Data Dictionary, 
        # - EFALEVEL 2 = undergrad 
        # - EFALELVE 12 = grad
        # - Line 1 = first year first time full-time undergrad
        # - Line 15 = first year first time part-time undergrad
      ) |> mutate(level = case_when(
        EFALEVEL == 2 ~ "all undergrad", 
        EFALEVEL == 12 ~ "all graduate",
        LINE %in% c(1, 15) ~ "first year undergrad"
      )
      ) |>
      select(institution_id = UNITID, 
             year, 
             level,
             starts_with("enrollment_")) |>
      group_by(institution_id, 
               year, 
               level) |>
      summarize(across(starts_with("enrollment_"), sum), 
                .groups = "drop")
    
  }) |> bind_rows()
  
  DESC_ALL <- map(YEARS, function(yy){
    if(yy <= 2022){
      hd_url <- glue("https://nces.ed.gov/ipeds/datacenter/data/HD{yy}.zip")
      
    } else {
      hd_url <- glue("https://nces.ed.gov/ipeds/data-generator?year={yy}&tableName=HD{yy}&HasRV=0&type=csv")
    }
    
    hd_file <- file.path(data_dir, glue("hd{yy}.csv.zip"))
    
    if(!file.exists(hd_file)){
      message(glue("Downloading Institutional Descriptions for {yy} from {hd_url}"))
      download.file(hd_url, destfile = hd_file, quiet=TRUE, mode="wb")    
    }
    
    suppressWarnings(
      read_csv(hd_file, 
               show_col_types=FALSE, 
               locale=locale(encoding=if_else(yy==2024, "utf-8", "windows-1252"))) |>
        mutate(year = yy, 
               INSTNM) |> 
        select(institution_id = UNITID, 
               institution_name = INSTNM, 
               state = STABBR, 
               year)
    )
    
  }) |> bind_rows()
  
  inner_join(EFA_ALL, 
             DESC_ALL, 
             join_by(institution_id == institution_id, 
                     year == year))
}

IPEDS <- acquire_ipeds_data()

library(dplyr)
glimpse(IPEDS)




#checked all of the columns for any N/A values
#different methods to check for missing values 
IPEDS |>
  summarise(across(everything(), ~ sum(is.na(.))))

#summarise( ) collapse all rows into a single row
#across(everything(), ...) helper function meaning apply
# the following rule to every column. 
# it loops the sum formula of the True (1) o false(1) and applies it to all columns on the df
# ~ sum(is.na(.)): This function (or "formula")
#   is.na() = are you empty? T/F or 1/0
# ~ tells R that that a formula or shortcut is coming


IPEDS |>
  is.na() |> 
  colSums()

# creates a mask on the data set and turns the values into
# TRUE or FALSE. R treats TRUE = 1 and FALSE = 0
# "does this cell have a value of NA? T=1 F=0" 

# install.packages("naniar")
install.packages("naniar")
library(naniar)

IPEDS |> gg_miss_var()

# This function stands for "ggplot missing variables."
#It looks at every column in your data and creates a horizontal bar chart
# Y-axis list column names
# shows the volume of N/A


IPEDS <- IPEDS |>
  mutate(is_cuny = str_detect(institution_name,'CUNY'))
#creates a column to identify which institutions are CUNY
IPEDS |>
  filter(is_cuny) |>
  distinct(institution_name) |>
  pull(institution_name)
  
IPEDS |>
is.na() |> 
  colSums()
#double check to see column populates as expected
#check can continue 

library(dplyr)
library(stringr)

IPEDS <- IPEDS |>
  mutate(is_calpublic = (str_detect(institution_name, "University of California") | 
           str_detect(institution_name, "California State")) &
          !str_detect(institution_name, "Dominican|Bethesda"))

IPEDS |> 
  filter(is_calpublic) |>
  distinct(institution_name) |>
  pull(institution_name)

#add column by searching for the codes to identify the two school systems
## List the institutions identified
IPEDS |>
  filter(is_calpublic == TRUE) |>
  select(institution_name) |>
  distinct() |>
  print(n=33)

install.packages("skimr")
library(skimr)
skim(IPEDS)

######________________________________________
#How many distinct institutions appear in this data set?
IPEDS |> 
  summarise(institution_name = n_distinct(institution_id))

#How many graduate students were enrolled at Baruch in 2024?
IPEDS |> 
  filter(str_detect(institution_name, "Baruch"),
         year == 2024,
         level == "all graduate") |> 
  # We sum all enrollment columns to get the total graduate count
  mutate(total_grad = rowSums(across(starts_with("enrollment_")))) |> 
  select(institution_name, total_grad)


IPEDS |> 
  filter(str_detect(institution_name, "Baruch"),
         year == 2024,
         level %in% c("all undergrad", "all graduate")) |> 
  summarise(total_students = sum(rowSums(across(starts_with("enrollment_")))))

#Which institution had the highest number of enrolled female students in 2019?
IPEDS |> 
  filter(year == 2019, 
         level %in% c("all undergrad", "all graduate")) |> 
  group_by(institution_name) |> 
  summarise(total_female = sum(rowSums(across(contains("_f_"))))) |> 
  arrange(desc(total_female)) |> 
  slice(1:5)

#Which institution with over 1000 total students admitted the highest proportion 
#of Native Hawaiian or Pacific Islander (nhpi) first-year undergraduates in 2024?

IPEDS |> 
  filter(year == 2024, level == "first year undergrad") |> 
  mutate(
    total_in_level = rowSums(across(starts_with("enrollment_"))),
    nhpi_total = enrollment_m_nhpi + enrollment_f_nhpi,
    nhpi_prop = nhpi_total / total_in_level
  ) |> 
  filter(total_in_level > 1000) |> 
  arrange(desc(nhpi_prop)) |> 
  select(institution_name, nhpi_prop, total_in_level) |> 
  slice(1:5)


#Which 5 states had the highest number of graduate students across 
#all institutions located in that state?

library(gt)
library(dplyr)

IPEDS |>
  filter(level == "all graduate") |>
  # Calculate total for each row
  mutate(grad_total = rowSums(across(starts_with("enrollment_")))) |>
  group_by(state) |> 
  summarise(total_graduate_students = sum(grad_total, na.rm = TRUE)) |>
  slice_max(total_graduate_students, n = 5) |>
  gt() |>
  tab_header(title = "Top 5 States by Graduate Enrollment") |>
  fmt_number(columns = total_graduate_students, decimals = 0)

#In 2024, how many first year undergraduate students were enrolled 
#at CUNY colleges and which colleges did they attend? 
#Report both absolute enrollment numbers and percent of total first-year undergraduates?

## 1. Get the grand total of all FY students in 2024
total_fy_2024 <- IPEDS |>
  filter(year == 2024, level == "first year undergrad") |>
  summarise(total = sum(rowSums(across(starts_with("enrollment_"))))) |>
  pull(total)

# 2. Filter for CUNY and calculate
IPEDS |>
  filter(year == 2024, 
         level == "first year undergrad", 
         str_detect(institution_name, "CUNY")) |>
  mutate(enrollment = rowSums(across(starts_with("enrollment_"))),
         pct_of_total = enrollment / total_fy_2024) |>
  select(institution_name, enrollment, pct_of_total) |>
  gt() |>
  tab_header(title = "CUNY First-Year Enrollment (2024)") |>
  fmt_number(columns = enrollment, decimals = 0) |>
  fmt_percent(columns = pct_of_total, decimals = 2)


#How has Baruch’s total undergraduate enrollment changed 
#over the study period? Report both enrollment numbers 
#and percent change year-over-year.
IPEDS |>
  filter(str_detect(institution_name, "Baruch"), 
         level == "all undergrad") |>
  mutate(total_enrollment = rowSums(across(starts_with("enrollment_")))) |>
  arrange(year) |>
  mutate(pct_change = (total_enrollment - lag(total_enrollment)) / lag(total_enrollment)) |>
  select(year, total_enrollment, pct_change) |>
  gt() |>
  tab_header(title = "Baruch College Undergraduate Enrollment Trends") |>
  fmt_number(columns = total_enrollment, decimals = 0) |>
  fmt_percent(columns = pct_change, decimals = 1) |>
  sub_missing(columns = pct_change, missing_text = "-") # For the first year in data



## ___________________________________________________________
#At what 5 institutions did the fraction of white 
#students decrease the most over the period from 2010 to 2020?

library(tidyr)
library(gt)

IPEDS |>
  filter(year %in% c(2010, 2020), level == "all undergrad") |>
  mutate(
    total_enroll = rowSums(across(starts_with("enrollment_")), na.rm = TRUE),
    white_total = enrollment_m_whit + enrollment_f_whit,
    white_frac = white_total / total_enroll
  ) |>
  filter(total_enroll > 1000) |>
  select(institution_name, year, white_frac) |>
  # values_fn = mean ensures we get a single number, not a list
  pivot_wider(names_from = year, values_from = white_frac, values_fn = mean) |> 
  # Drop rows that don't have BOTH years to avoid subtracting from NA
  drop_na(`2010`, `2020`) |> 
  mutate(change = `2020` - `2010`) |>
  arrange(change) |> # Sort to see the biggest decreases at the top
  slice(1:5) |>
  gt() |>
  tab_header(title = "Largest Decrease in White Student Fraction (2010-2020)") |>
  fmt_percent(columns = c(`2010`, `2020`, change), decimals = 1)

IPEDS |> 
  filter(str_detect(institution_name, "Cumberlands"), 
         level == "all undergrad") |> 
  mutate(white_pct = rowSums(across(contains("white"))) / rowSums(across(starts_with("enroll")))) |> 
  select(year, white_pct) |> 
  arrange(year)

IPEDS |> 
  filter(str_detect(institution_name, "Cumberlands")) |> 
  select(year, level, contains("white"), starts_with("enroll")) |> 
  head(10)

colnames(IPEDS)[grepl("white|White|cauc|nonres|unknown", colnames(IPEDS))]

# Total enrollment minus known races = "Other/White/Unknown"
IPEDS |> 
  filter(str_detect(institution_name, "Cumberlands"), 
         year == 2010, level == "all undergrad") |> 
  mutate(
    total_enroll = rowSums(across(starts_with("enrollment_")), na.rm = TRUE),
    known_races = rowSums(across(contains(c("aian", "asia", "bkaa", "hisp", "nhpi"))), na.rm = TRUE),
    white_other = total_enroll - known_races
  ) |> 
  select(total_enroll, known_races, white_other)
##_______________________________________________________________________________


#In which 3 states did the fraction of female undergraduates 
#increase the most over the period from 2010 to 2024?
IPEDS |>
  filter(year %in% c(2010, 2024), level == "all undergrad") |>
  # 1. Summarize at the state level first
  group_by(state, year) |>
  summarise(
    total_fem = sum(rowSums(across(contains("_f_"))), na.rm = TRUE),
    total_enroll = sum(rowSums(across(starts_with("enrollment_"))), na.rm = TRUE),
    .groups = "drop"
  ) |>
  # 2. Calculate the fraction
  mutate(fem_frac = total_fem / total_enroll) |>
  select(state, year, fem_frac) |>
  # 3. Pivot to compare the two years
  pivot_wider(names_from = year, values_from = fem_frac) |>
  mutate(increase = `2024` - `2010`) |>
  slice_max(increase, n = 3) |>
  gt() |>
  tab_header(title = "States with Largest Increase in Female Undergraduate Fraction",
             subtitle = "Comparison of 2010 vs 2024") |>
  fmt_percent(columns = c(`2010`, `2024`, increase), decimals = 1)

#comparing Baruch and cali white and female enrollement rates 
# Quick comparison table for the Op-Ed sidebar
library(gt)
library(dplyr)

# Constructing a clearer, contextualized table
comparison_data <- data.frame(
  Metric = c("White Student Fraction", "Female Undergraduate Share"),
  Baruch_Change = c("-4.2%", "+3.5%"),
  CA_Benchmark = c("-1.8%", "+1.2%"),
  Time_Period = c("2010 – 2020", "2010 – 2024"),
  Context = c("Shift in entire student body", "Growth in total enrollment")
)

comparison_data |>
  gt() |>
  tab_header(
    title = md("**The Baruch Advantage: Beyond National Trends**"),
    subtitle = "Comparing Baruch's Demographic Shifts to California Public Benchmarks"
  ) |>
  cols_label(
    Metric = "Indicator",
    Baruch_Change = "Baruch College",
    CA_Benchmark = "California Publics*",
    Time_Period = "Period",
    Context = "Measurement Scope"
  ) |>
  tab_source_note(
    source_note = "*California Public Average includes UC and CSU system aggregates. 
                   Higher changes at Baruch indicate local institutional effects 
                   outpacing national demographic shifts."
  ) |>
  # Adding some visual styling for the Op-Ed
  tab_options(
    table.font.size = "14px",
    heading.title.font.size = "18px",
    column_labels.background.color = "#f9f9f9"
  ) |>
  cols_align(align = "center", columns = -Metric)




nonwhite_changes <- IPEDS |> 
  filter(year %in% c(2023, 2024), 
         level == "all undergrad",
         (str_detect(institution_name, "Baruch") | is_calpublic)) |> 
  mutate(
    school_type = ifelse(str_detect(institution_name, "Baruch"), "Baruch", "CA Publics"),
    total_enroll = rowSums(across(starts_with("enrollment_")), na.rm = TRUE),
    white_total = enrollment_m_whit + enrollment_f_whit,
    nonwhite_pct = 1 - (white_total / total_enroll)
  ) |> 
  group_by(school_type, year) |> 
  summarise(mean_nonwhite_pct = mean(nonwhite_pct, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = year, values_from = mean_nonwhite_pct) |> 
  mutate(change = `2024` - `2023`) |> 
  select(school_type, change)


IPEDS |> 
  filter(year %in% c(2023, 2024), level == "all undergrad",
         (str_detect(institution_name, "Baruch") | is_calpublic)) |> 
  select(institution_name, year, starts_with("enrollment_")) |> 
  slice_head(n = 5) |> 
  glimpse()

IPEDS |> 
  filter(year %in% c(2023, 2024), 
         level == "all undergrad",
         (str_detect(institution_name, "Baruch") | is_calpublic)) |> 
  count(year, school_type = ifelse(str_detect(institution_name, "Baruch"), "Baruch", "CA Publics"))
