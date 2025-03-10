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

