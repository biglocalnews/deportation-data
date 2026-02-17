library(tidyverse)
library(here)
library(arrow)

# setup the environment
rm(list = ls())
here::i_am("utilities/make-record-layouts.R")


# read the arrow file, and remove some columns for easier import to Google sheets
# convert date-time to just date -- we're not dealing with time, so
# it's easier to import dates alone correctly into Google sheets.

arrests <- read_feather(here("data/bln_arrest_recipe_data.arrow")) |>
  select ( -c ( bln_charge_as_of_date, bln_charge_code,
                orig_apprehension_state, aor_states,
                detention_facility_type)) |>
  mutate ( across ( where(is.POSIXct ) , \(x) as_date(x) ) )


# get the names of the columns, their types, and the number of missing values
# fix the typo in the bln_charge_grouop !
# if it's fixed in the original, this will just do nothing.
varnames <- names(arrests) |> str_replace("bln_charge_grouop", "bln_charge_group")
vartypes <- map_chr ( arrests, \(x) class (x)[[1]] )
missing_values <- map_int ( arrests, \(x) sum ( is.na (x)))

# create descriptions for all of the columns. Some columns won't
# exist in the pared-down data, but that's ok

descriptions =
  c(
    "arrest_id" = "Original row number from the FOIA spreadsheet",
    "bln_person_id" = "BLN's person identifier for arrests, the first 10 characters of the original unique identifier if it exists, or a combination of birth year and row number for cases where it doesn't",
    "apprehension_date" = "orignal recorded date of arrest",
    "is_last_arrest" = "TRUE if it is the most recent arrest for the person ID",
    "apprehension_aor" = "ICE Enforcement and Removal Operations area of responsibility",
    "bln_arrest_state" = "BLN's best estimate of state. See methodology for more detail",
    "apprehension_method_recoded" = "At-large or Custodial, see [common definitions](app-common-defs.qmd)",
    "apprehension_criminality" = "Original criminality recorded in the arrest table, either no conviction, pending charges or other immigration violator",
    "bln_arrest_charge_code" = "See [common definitions](app-common-defs.qmd) for details",
    "bln_charge" = "BLN charge description",
    "bln_charge_group_code" = "2-digit crime code, to make for fewer categories",
    "bln_charge_group" = "Description of the 2-digit crime code",
    "bln_charge_special" = "Special types of charges",
    "birth_year" = NA_character_,
    "citizenship_country" = NA_character_,
    "gender" = NA_character_,
    "case_status_recoded" = "See see [common definitions](app-common-defs.qmd) ",
    "departed_date" = NA_character_,
    "departure_country" = NA_character_,
    "final_order_date" = "Date of the removal order from immigration court",
    "orig_apprehension_state" = NA_character_,
    "aor_states" = NA_character_,
    "detention_facility" = "Name of the detention facility matching this arrest",
    "detention_state" = NA_character_,
    "detention_city" = NA_character_,
    "detention_county" = NA_character_,
    "detention_days_after" = "# of days after the arrest that the detention began. Negative numbers reflect detentions that are recorded as starting just before the arrest",
    "detainer_state" = "State of the most recent detainer",
    "detainer_facility" = "Name of the most recent detainer facility ",
    "detainer_city" = NA_character_,
    "detainer_county" = "Detainer county based on geocoding of the facility name and city -- approximate",
    "detainer_days_before" = "# of days before the arrest that the detainer was sent. Negative numbers reflect detainers apparently entered up to several days before the arrest"
  ) |>
  enframe(name = "varname", value = "description")



arrest_record_layout<-
  tibble(name = varnames, type = vartypes, missing = missing_values) |>
  rowid_to_column(var = "position") |>
  left_join ( descriptions, join_by (name==varname))

write_csv(arrest_record_layout, here('data/arrest_record_layout.csv'), na="")
