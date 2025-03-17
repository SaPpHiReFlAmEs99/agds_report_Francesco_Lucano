library(dplyr)
library(tidyr)
library(stringr)

data("starwars")


# 1) How many pale characters come from the planets Ryloth and Naboo?
pale_count <- starwars |>
  # Keep only rows matching homeworld in Ryloth or Naboo AND skin_color == "pale"
  filter(homeworld %in% c("Ryloth", "Naboo"), skin_color == "pale") |>
  # Count the number of rows left
  nrow()

pale_count


# 2) Who is the oldest among the tallest thirty characters?
oldest_among_tallest30 <- starwars |>
  # Remove rows with missing height
  filter(!is.na(height)) |>
  # Sort by height from largest to smallest
  arrange(desc(height)) |>
  # Keep only the first 30 (the tallest)
  slice(1:30) |>
  # Remove rows that have missing birth_year
  filter(!is.na(birth_year)) |>
  # Sort by birth_year descending so the biggest (oldest) is first
  arrange(desc(birth_year)) |>
  # Keep the very first row
  slice(1) |>
  # Pull only the name
  pull(name)

oldest_among_tallest30


# 3) What is the name of the smallest character and their starship in "Return of the Jedi"?
smallest_in_rotj <- starwars |>
  # Keep only rows where "Return of the Jedi" is in the 'films' column
  filter(str_detect(films, "Return of the Jedi")) |>
  # Exclude missing height
  filter(!is.na(height)) |>
  # Sort ascending by height
  arrange(height) |>
  # Keep the first row -> the smallest
  slice(1)

smallest_in_rotj


library(purrr)    # for map_lgl()
smallest_in_rotj <- starwars |>
  # For each row, map_lgl() checks if "Return of the Jedi" is among the films
  filter(
    map_lgl(films, ~ any(str_detect(.x, "Return of the Jedi")))
  ) |>
  filter(!is.na(height)) |>
  arrange(height) |>
  slice(1)

smallest_in_rotj

# map_lgl(films, ~ any(str_detect(.x, "Return of the Jedi"))):
    # For each row, .x is that rowʼs vector of film titles.
    # We do str_detect(.x, "Return of the Jedi") to get a logical vector of matches.
    # Then any(...) checks if at least one film title matches.
    # map_lgl(...) turns this into a single TRUE/FALSE for each row, which filter() then uses to keep or discard rows.



#####################################################################################################################################
#Aggregating
##You have learned about aggregating in the {tidyverse}. Let’s put it in practice.

###Reuse the code in the tutorial to read, reduce, and aggregate the half_hourly_fluxes dataset to the daily scale, calculating the following metrics across half-hourly VPD_F values within each day: mean, 25% quantile, and 75% quantile.

library(readr)
library(dplyr)
library(lubridate)

half_hourly_fluxes <- readr::read_csv("data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")


##############################################
# 1. Parse TIMESTAMP_START as a date-time
##############################################
# If TIMESTAMP_START is numeric, e.g. 200401010000 (YYYYMMDDhhmm),
# convert it to character, then parse with ymd_hm().
# This turns "200401010000" into POSIXct date-time like 2004-01-01 00:00:00 UTC.
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(
    # First, ensure TIMESTAMP_START is character:
    TIMESTAMP_START = as.character(TIMESTAMP_START),
    # Next, parse as date-time using lubridate:
    TIMESTAMP_START = ymd_hm(TIMESTAMP_START)
  )

# a. Extract just the date
# We only need the day-level resolution, so we create a new column "date".
half_hourly_fluxes <- half_hourly_fluxes %>%
  mutate(date = as_date(TIMESTAMP_START))

# b. Aggregate half-hourly data to daily
# We group by "date" and then calculate:
# - mean VPD (mean_vpd)
# - 25% quantile (q25_vpd)
# - 75% quantile (q75_vpd)
daily_vpd <- half_hourly_fluxes %>%
  group_by(date) %>%
  summarise(
    mean_vpd = mean(VPD_F, na.rm = TRUE),
    q25_vpd  = quantile(VPD_F, probs = 0.25, na.rm = TRUE),
    q75_vpd  = quantile(VPD_F, probs = 0.75, na.rm = TRUE),
    # .groups = "drop" un-groups the data frame after summarising
    .groups = "drop"
  )


##############################################
# 2. Identify upper & lower 10% days of mean VPD
##############################################
# We find the 90th and 10th percentile of mean_vpd.
upper_cutoff <- quantile(daily_vpd$mean_vpd, 0.90, na.rm = TRUE)
lower_cutoff <- quantile(daily_vpd$mean_vpd, 0.10, na.rm = TRUE)

# Filter the days that have mean_vpd >= upper_cutoff OR <= lower_cutoff
extreme_days <- daily_vpd %>%
  filter(mean_vpd >= upper_cutoff | mean_vpd <= lower_cutoff)


##############################################
# 63 Calculate average 25% & 75% quantiles for these extreme days
##############################################
# Summarise across all extreme days:
extreme_summary <- extreme_days %>%
  summarise(
    mean_of_q25 = mean(q25_vpd, na.rm = TRUE),
    mean_of_q75 = mean(q75_vpd, na.rm = TRUE)
  )

# Print results
extreme_summary



#########################################################################################################################################
library(readr)
library(dplyr)
library(lubridate)

flux_raw <- readr::read_csv("data/FLX_CH-Lae_FLUXNET2015_FULLSET_HH_2004-2006.csv")

flux_raw <- flux_raw %>%
  mutate(
    # convert numeric -> character -> date-time
    TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)),
    # extract hour of day (0 to 23)
    hour_of_day = hour(TIMESTAMP_START)
  )


###############################################################
#Investigate data quality by hour
###############################################################
# NEE_VUT_REF_QC typically indicates:
#  0 = measured data
#  1 = good quality gap-filled
#  2 = medium quality gap-filled
#  3 = poor quality gap-filled
# We count how many records fall into each category, per hour.
df_qc <- flux_raw %>%
  group_by(hour_of_day) %>%
  summarise(
    total   = n(),
    count_0 = sum(NEE_VUT_REF_QC == 0, na.rm = TRUE),
    count_1 = sum(NEE_VUT_REF_QC == 1, na.rm = TRUE),
    count_2 = sum(NEE_VUT_REF_QC == 2, na.rm = TRUE),
    count_3 = sum(NEE_VUT_REF_QC == 3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    frac_0 = count_0 / total,
    frac_1 = count_1 / total,
    frac_2 = count_2 / total,
    frac_3 = count_3 / total
  )

df_qc

library(ggplot2)

ggplot(df_qc, aes(x = hour_of_day, y = frac_0)) +
  geom_line() +
  geom_point() +
  labs(title = "Fraction of Measured (QC=0) by Hour",
       x = "Hour of Day", y = "Fraction QC=0") +
  theme_classic()


####################################################################
#Compare daily mean GPP (unmodified vs. measured-only)
####################################################################

###Unmodified daily GPP
# We create a new data frame `daily_gpp_unmodified` that includes ALL half-hourly
# GPP records (gap-filled or measured). We then compute the mean GPP per day.
daily_gpp_unmodified <- flux_raw %>%
  mutate(date = as_date(TIMESTAMP_START)) %>%  # extract just the day
  group_by(date) %>%
  summarise(
    daily_gpp = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate the overall mean GPP across all days
mean_unmodified <- mean(daily_gpp_unmodified$daily_gpp, na.rm = TRUE)

### 3. Measured-only daily GPP
# Now we FILTER out any half-hourly data that is not measured (QC=0).
# That means we keep only half-hourly records with NEE_VUT_REF_QC == 0.
daily_gpp_measured <- flux_raw %>%
  filter(NEE_VUT_REF_QC == 0) %>%            # only measured data
  mutate(date = as_date(TIMESTAMP_START)) %>%
  group_by(date) %>%
  summarise(
    daily_gpp = mean(GPP_NT_VUT_REF, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate the overall mean GPP across these measured-only days
mean_measured <- mean(daily_gpp_measured$daily_gpp, na.rm = TRUE)

#measured-only GPP ends up higher because the missing or low-quality data 
#(requiring gap-filling) often occur in periods or conditions that have relatively lower photosynthetic flux. 
#Consequently, when you include those gap-filled data, the overall mean is pulled down 
#(unmodified dataset ≈ 4.2), whereas when you exclude them and keep only measured high-quality data, 
#you capture more of the higher daytime flux (measured-only dataset ≈ 7.1).

