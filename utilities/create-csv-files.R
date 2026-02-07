
# Start with this and edit it .
# I don't feel like doing it right now, but the idea is pretty straightforward

library(tidyverse)
library(here)
library(arrow)

rm(list=ls())

i_am("utilities/create-csv-files.R")

# read the arrest feather data, and remove some columns we're not going to export

all_arrests <- read_feather(here("data/bln_arrest_recipe_data.arrow")) |>
  select (-c(bln_charge_code, bln_charge_as_of_date,
                aor_states, orig_apprehension_state, detention_facility_type
              )) |>
  # feather always keeps date-time, not just date, so simplify it
  mutate ( across ( c(apprehension_date, final_order_date, departed_date) ,
              \(x) as_date (x) )
      )


# Get the state abbreviations for the state names

state_abbr <- tidycensus::fips_codes |>
  distinct ( state_abbr = state, state_name) |>
  mutate ( state_name = str_to_upper(state_name))


# Get the list of states in the original data, and
# remove the territories and mistakes.
# convert the state abbreviation to lower case for the csv file name

states <- count( all_arrests, bln_arrest_state, name="rows") |>
  filter ( str_detect ( bln_arrest_state,
            "\\b(GUAM|ARMED|MARIANA|FEDERATED|TAMAU|ISLANDS|PUERTO)",
            negate=TRUE)) |>
  filter ( ! bln_arrest_state == "MEXICO") |>
  left_join ( state_abbr, join_by ( bln_arrest_state == state_name)) |>
  mutate ( state_abbr = str_to_lower(state_abbr))

# create a lookup vector for the state abbreviation
lkp_state <- states |>  pull ( state_abbr, name=bln_arrest_state)


# Create the variables to hold the path
folder_path = here("data/arrest_csvs")
file_root = "_ice_arrests.csv"

# Function to write the csv
write_state_csv <- function ( current_state_name) {
    # filter for the state

    # get the file name and path
    current_abbr  <- unname ( lkp_state[current_state_name] )
    current_file_name <- str_c (current_abbr, file_root, sep="")
    # print ( current_file_name)

    # filter the rows to the state
    current_state_data <- all_arrests |>
          filter ( bln_arrest_state == current_state_name )

    # write the csv
    write_csv( current_state_data,
                file=str_c(folder_path, current_file_name, sep="/" ) ,
              na  = "")

    # save info from the state for later
    save_info <- tibble (
        file_name = current_file_name,
        row_count = nrow(current_state_data) ,
        state_name = current_state_name
    )


}

list_of_states <- map (  states$bln_arrest_state, write_state_csv)

# combine the list that contains row count and abbreviations into a data frame.
summary_df <- list_rbind( list_of_states)


## AREA OF RESPONSIBILITY

# here, I have to create a lookup for the AOR's by hand. There are not that many of them
# I saved abbreviations with the state inclusions in the Google lookup sheets
# https://docs.google.com/spreadsheets/d/1iYuww_sNv6MizPh_ysqMWjYvyV72u3OXUbkvMCyIlWw/edit?usp=sharing

aor_states <-
  googlesheets4::read_sheet("1iYuww_sNv6MizPh_ysqMWjYvyV72u3OXUbkvMCyIlWw", sheet="aor_list") |>
  summarize ( file_abbrev = first(abbrev),
              states = str_c ( apprehension_state, collapse=", ") ,
      .by = apprehension_aor)

lkp_aor <- aor_by_state |> pull(file_abbrev, name = apprehension_aor)

file_root = "_aor_ice_arrests.csv"

# I could probably fix the original function to be more dynamic,
# but it seems just as easy to do a new one based on aor.

write_aor_csv <- function(current_aor_name) {
  # get the file name and path
  current_abbr <- unname(lkp_aor[current_aor_name])
  current_file_name <- str_c(current_abbr, file_root, sep = "")
  # print ( current_file_name)

  # filter for the aor
  current_aor_data <- all_arrests |>
    filter(apprehension_aor == current_aor_name)

  # write the csv
  write_csv(
    current_aor_data,
    file = str_c(folder_path, current_file_name, sep = "/"),
    na = ""
  )

  # save info from the state for later
  save_info <- tibble(
    file_name = current_file_name,
    row_count = nrow(current_aor_data),
    aor_name = current_aor_name
  )
}

list_of_aors <-  map(aor_by_state$apprehension_aor, write_aor_csv)
aor_summary <- list_rbind(list_of_aors) |>
  left_join ( aor_by_state |> select ( apprehension_aor, states),
              join_by ( aor_name == apprehension_aor )
)



## Save the summaries to the data folder

write_csv(aor_summary, here("data/aor_csv_summary.csv"))
write_csv(summary_df, here("data/state_csv_summary.csv"))
