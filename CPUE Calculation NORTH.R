
### CPUE CALCULATION ACROSS THE WHOLE kobo_NORTH_clean DATASET

kobo_NORTH_clean <- read.csv("Data/kobo_NORTH_clean.csv", header = TRUE)


library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(stringdist)
library(purrr)
library(lubridate)
library(hms)



# STEP 1. CREATE NEW FILE CPUE_SOUTH_ALL. 
CPUE_North_All <- kobo_NORTH_clean %>%
  dplyr::select(
    Start_entry_date, Start_entry_time, End_entry_date, End_entry_time, Did_you_collect, Collector_name, 
    Departure_time, Return_time, Departure_date, Return_date, Overnight, Town, Fishing_spot, Crew, Number_crew_split_catch, 
    Total_Crew, Total_Men, Total_Women, Organisms_harvested, 
    starts_with("Gear_"),
    starts_with("Total_Number_"),
    starts_with("Total_Kilos_"),
    starts_with("Total_Avg_"),
    starts_with("Total_Med_"),
    starts_with("Avg_"),
    starts_with("Med_"),
  ) 



# STEP 2. ADD A REGION COLUMN

CPUE_North_All <- CPUE_North_All %>%
  mutate(Region = "Diana")


# STEP 3. ADD SEASON COLUMN. Say that all dates in Nov-Apr are the hot rainy season, and May-Oct cooler dry season. 

CPUE_North_All <- CPUE_North_All %>%
  mutate(Season = case_when(
    month(Departure_date) %in% c(11, 12, 1, 2, 3, 4) ~ "Rainy Season",
    month(Departure_date) %in% c(5, 6, 7, 8, 9, 10) ~ "Dry Season",
    TRUE ~ NA_character_
  ))


# STEP 4. ADD A RESERVE COLUMN
# This is answering - is there a government-gazetted reserve in each of those villages

CPUE_North_All <- CPUE_North_All %>%
  mutate(Government_Gazetted_Reserve = case_when(
    Town %in% c("Ambavarano", "Ivovona", "Ambodivahibe") ~ "Yes",
    Town %in% c("Ambatoloaka", "Ramena", "Antaravy", "Nosy Sakatia") ~ "No",
    TRUE ~ NA_character_  # Assign NA if Town is not listed
  ))



# STEP 5. CHECK VALUES FOR DID_YOU_COLLECT

unique(CPUE_North_All$Did_you_collect)
# Check rows where Did_you_collect is an empty string
empty_string_rows <- CPUE_North_All %>% filter(Did_you_collect == "")

# Update Did_you_collect to "Yes" where it's an empty string, but both departure_time and return_time have values (not NA or empty string)
CPUE_North_All <- CPUE_North_All %>%
  mutate(Did_you_collect = if_else(
    Did_you_collect == "" & !is.na(Departure_time) & Departure_time != "" &
      !is.na(Return_time) & Return_time != "",
    "Yes", Did_you_collect
  ))



# STEP 6. DELETE ROWS WHERE EITHER DEPARTURE OR RETURN TIMES ARE MISSING (can't calculate CPUE for that), but keeping those where both are missing.

# Get rid of white spaces first and then we can filter to get rid of rows were departure or return are missing when Did_you_collect is YES. Can't calculate CPUE without that

CPUE_North_All <- CPUE_North_All %>%
  filter(!(Did_you_collect == "Yes" & 
             (is.na(Departure_time) | Departure_time == "" | 
                is.na(Return_time) | Return_time == "")))



CPUE_North_All <- CPUE_North_All %>%
  filter(!(Did_you_collect == "Yes" & Total_Crew == 0))

# STEP 7. CREATE A FISHING TRIP ID FOR EACH INDIVIDUAL FISHING TRIP

  # We are saying that if Start, End, Town, Fishing_spot, Departure_time, Return_time are identical, then the fishing trip ID should be the same. 
    # Identify unique trips and assign unique IDs


unique_trips <- CPUE_North_All %>%
  distinct(Start_entry_date, Start_entry_time, End_entry_date, End_entry_time,
           Town, Collector_name, Fishing_spot, Departure_date, Return_date,
           Departure_time, Return_time, .keep_all = FALSE) %>%
  mutate(Trip_ID = paste0("DI_", row_number()))  


# Join the IDs back to the original dataset
CPUE_North_All <- CPUE_North_All %>%
  left_join(unique_trips, by = c("Start_entry_date", "Start_entry_time", "End_entry_date", "End_entry_time", "Town", "Collector_name", "Fishing_spot", "Departure_date", "Return_date", "Departure_time", "Return_time"))

# Reorder columns to place TRIP ID as the first column
CPUE_North_All <- CPUE_North_All %>%
  dplyr::select(Trip_ID, Region, everything())



# STEP 8. SUM THE COLUMNS KILOS_ TO HAVE TOTAL CATCH IN KILOS FOR EACH FISHING TRIP  

#CPUE_North_All <- CPUE_North_All %>%
#  mutate(across(starts_with("Total_kilos_"), as.numeric, .names = "numeric_{col}")) %>%
#  mutate(`Total_Catch_Kilos` = rowSums(dplyr::select(., starts_with("numeric_Total_kilos_")), na.rm = TRUE))

# CPUE_North_All <- CPUE_North_All %>%
#  mutate(across(starts_with("Total_kilos_"), as.numeric, .names = "numeric_{col}")) %>%
#  rowwise() %>%
#  mutate(`Total_Catch_Kilos` = sum(c_across(starts_with("numeric_Total_kilos_")), na.rm = TRUE)) %>%
#  ungroup()


CPUE_North_All <- CPUE_North_All %>%
  mutate(
    across(starts_with("Total_Kilos_"), ~ replace_na(as.numeric(.), 0)),
    across(starts_with("Total_Number_"), ~ replace_na(as.numeric(.), 0))
  )


CPUE_North_All <- CPUE_North_All %>%
  mutate(across(starts_with("Total_Kilos_"), as.numeric)) %>%
  rowwise() %>%
  mutate(Total_Catch_Kilos = sum(c_across(starts_with("Total_Kilos_")), na.rm = TRUE)) %>%
  ungroup()

# STEP 9. SUM THE COLUMNS NUMBER_ TO HAVE TOTAL CATCH IN KILOS FOR EACH FISHING TRIP

#CPUE_North_All <- CPUE_North_All %>%
#  mutate(across(starts_with("Total_number_"), as.numeric, .names = "numeric_{col}")) %>%
#  mutate(`Total Catch Nb Ind` = rowSums(dplyr::select(., starts_with("numeric_Total_number_")), na.rm = TRUE))


#CPUE_North_All <- CPUE_North_All %>%
#  mutate(across(starts_with("Total_number_"), as.numeric, .names = "numeric_{col}")) %>%
#  rowwise() %>%
#  mutate(`Total Catch Nb Ind` = sum(c_across(starts_with("numeric_Total_number_")), na.rm = TRUE)) %>%
#  ungroup()

# STEP 10. CALCULATE FISHING EFFORT BASED ON DEPARTURE AND RETURN DATES AND TIMES.

CPUE_North_All <- CPUE_North_All %>%
  mutate(
    Departure_time_clean = str_remove(Departure_time, "\\+.*$"),
    Return_time_clean = str_remove(Return_time, "\\+.*$")
  ) %>%
  mutate(
    Start_datetime = as.POSIXct(paste(Departure_date, Departure_time_clean), format = "%Y-%m-%d %H:%M:%S"),
    End_datetime = as.POSIXct(paste(Return_date, Return_time_clean), format = "%Y-%m-%d %H:%M:%S"),
    Fishing_effort_hours = as.numeric(difftime(End_datetime, Start_datetime, units = "hours"))
  )


# Check for negative fishing effort values

negative_effort <- CPUE_North_All %>%
  filter(Fishing_effort_hours < 0)

## We have 56 negative fishing effort values.

# Process data to clean time and calculate fishing effort, adding 12 hours if conditions are met
CPUE_North_All <- CPUE_North_All %>%
  # Clean the departure and return times by removing timezone
  mutate(
    Departure_time_clean = str_remove(Departure_time, "\\+.*$"),
    Return_time_clean = str_remove(Return_time, "\\+.*$")
  ) %>%
  # Create Start and End datetime columns
  mutate(
    Start_datetime = as.POSIXct(paste(Departure_date, Departure_time_clean), format = "%Y-%m-%d %H:%M:%S"),
    End_datetime = as.POSIXct(paste(Return_date, Return_time_clean), format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  # Calculate initial fishing effort
  mutate(
    Fishing_effort_hours = as.numeric(difftime(End_datetime, Start_datetime, units = "hours"))
  ) %>%
  # Adjust the End_datetime if Fishing_effort_hours is negative and meets the conditions
  mutate(
    Return_time_clean = if_else(
      Fishing_effort_hours < 0 & 
        format(as.POSIXct(Departure_time_clean, format = "%H:%M:%S"), "%H") < 14 &
        format(as.POSIXct(Return_time_clean, format = "%H:%M:%S"), "%H") < 16,
      format(as.POSIXct(Return_time_clean, format = "%H:%M:%S") + hours(12), "%H:%M:%S"),
      Return_time_clean
    )
  ) %>%
  # Recalculate the End_datetime and Fishing_effort_hours after adjusting the return time
  mutate(
    End_datetime = as.POSIXct(paste(Return_date, Return_time_clean), format = "%Y-%m-%d %H:%M:%S"),
    Fishing_effort_hours = as.numeric(difftime(End_datetime, Start_datetime, units = "hours"))
  )


# Check for negative fishing effort values
negative_effort <- CPUE_North_All %>%
  filter(Fishing_effort_hours < 0)


# Still 28 negative rows for overnight trips where departure and return dates are the same BUT they say they didn't stay overnight??
CPUE_North_All <- CPUE_North_All %>%
  # Adjust the return date by adding 1 day if the fishing effort is negative and the time conditions are met
  mutate(
    Return_date = if_else(
      Fishing_effort_hours < 0 &
        format(as.POSIXct(Departure_time_clean, format = "%H:%M:%S"), "%H") > 13 &
        format(as.POSIXct(Return_time_clean, format = "%H:%M:%S"), "%H") < 18,
      as.Date(Return_date) + 1,  # Add 1 day to Return_date
      as.Date(Return_date)       # Keep Return_date unchanged
    )
  ) %>%
  # Recalculate the End_datetime and Fishing_effort_hours after adjusting the return date
  mutate(
    End_datetime = as.POSIXct(paste(Return_date, Return_time_clean), format = "%Y-%m-%d %H:%M:%S"),
    Fishing_effort_hours = as.numeric(difftime(End_datetime, Start_datetime, units = "hours"))
  )


# Check for negative fishing effort values
negative_effort <- CPUE_North_All %>%
  filter(Fishing_effort_hours < 0) ### Still have 2 negative values, not quite sure what to do with these.

## Get rid of the 1 row that still has negative values, can't figure out if the departure or return times are wring or dates
# Remove rows where Fishing_hours are negative
CPUE_North_All <- CPUE_North_All %>%
  filter(Fishing_effort_hours >= 0 | is.na(Fishing_effort_hours))



# STEP 11. CALCULATE CPUE USING KILOS AND CPUE USING NB OF INDIVIDUALS ONCE FISHING EFFORT VALUES ARE NO LONGER NEGATIVE.

  # Remove rows where CREW is NA only when Did_you_collect is YES, because we need this information to calculate CPUE.

CPUE_North_All <- CPUE_North_All %>%
  filter(!(Did_you_collect == "YES" & (is.na(Crew) | trimws(Crew) == "")))


# Calculate CPUE per person using the number of crew that split the catch column


CPUE_North_All <- CPUE_North_All %>%
  mutate(
    `Total_Catch_Kilos` = as.numeric(gsub(",", "", `Total_Catch_Kilos`)),
   # `Total Catch Nb Ind` = as.numeric(gsub(",", "", `Total Catch Nb Ind`)),
    Fishing_effort_hours = as.numeric(gsub(",", "", Fishing_effort_hours)),
    Total_Crew = as.numeric(gsub(",", "", Total_Crew))
  )
    

CPUE_North_All <- CPUE_North_All %>%
  mutate(
    CPUE_Kilos = case_when(
      Crew == "No" ~ `Total_Catch_Kilos` / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (`Total_Catch_Kilos` / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ))
    
#CPUE_North_All <- CPUE_North_All %>%
#  mutate(
#    CPUE_NbIND = case_when(
#      Crew == "No" ~ `Total Catch Nb Ind` / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (`Total Catch Nb Ind` / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    )
#  )



# Check for negative CPUE_Kilos
negative_CPUE <- CPUE_North_All %>%
  filter(CPUE_Kilos < 0)  



# STEP 12. CHECK FOR DUPLICATES.

duplicates <- CPUE_North_All[duplicated(CPUE_North_All) | duplicated(CPUE_North_All, fromLast = TRUE), ]



# STEP 13. REMOVE THE TIMEZONE INFORMATION FROM THE DEPARTURE AND RETURN TIME COLUMNS


CPUE_North_All <- CPUE_North_All %>%
  mutate(Departure_time = str_remove(Departure_time, "\\+\\d{2}:\\d{2}"),
         Return_time = str_remove(Return_time, "\\+\\d{2}:\\d{2}"))


CPUE_North_All <- CPUE_North_All %>%
  mutate(
    Departure_time = ifelse(!grepl("^\\d{2}:\\d{2}:\\d{2}(\\.\\d{1,3})?$", Departure_time), NA, Departure_time),
    Return_time = ifelse(!grepl("^\\d{2}:\\d{2}:\\d{2}(\\.\\d{1,3})?$", Return_time), NA, Return_time)
  ) %>%
  mutate(
    Departure_time = hms::as_hms(Departure_time),
    Return_time = hms::as_hms(Return_time)
  )


# STEP 14. REMOVE ROWS WHERE CPUE VALUE IS INF. THIS MEANS THAT FISHING EFFORT VALUE IS 0 - probably a mistake from collectors because there is catch for most of these rows

CPUE_North_All %>%
  filter(is.infinite(CPUE_Kilos))

CPUE_North_All <- CPUE_North_All %>%
  filter(!is.infinite(CPUE_Kilos))


# STEP 15. CHECKING FOR ALL ROWS WHERE TOWN IS MISSING

missing_town <- CPUE_North_All %>%
  filter(is.na(Town) | Town == "") %>%
  nrow()



# STEP 16. REMOVE ALL ROWS WHERE DEPARTURE AND RETURN TIME, AND COLLECTOR NAME AND TOWN ARE MISSING
CPUE_North_All <- CPUE_North_All %>%
  filter(!(is.na(Departure_time) & is.na(Return_time) & 
             is.na(Collector_name) & is.na(Town)) & 
           !(trimws(Collector_name) == "" & trimws(Town) == ""))

CPUE_North_All <- CPUE_North_All %>%
  filter(!(is.na(Town) | trimws(Town) == "") | Did_you_collect != "Yes")




summary(CPUE_North_All$`Total_Catch_Kilos`)
#summary(CPUE_North_All$`Total Catch Nb Ind`)


## STEP 17. CHECK FOR UNREALISTICALLY LOW NUMBER OF FISHING HOURS (EG. 0.03HRS)

CPUE_North_filtered <- CPUE_North_All %>%
  filter(Fishing_effort_hours >= 1, Fishing_effort_hours <= 3)

CPUE_North_filtered2 <- CPUE_North_All %>%
  filter(Fishing_effort_hours < 1)

CPUE_North_filtered3 <- CPUE_North_All %>%
  filter(CPUE_Kilos > 20)

  # These are the rules we have set for Paper 1:
CPUE_North_All <- CPUE_North_All %>%
  filter(Fishing_effort_hours >= 1)

CPUE_North_All <- CPUE_North_All %>%
  filter(`Total_Catch_Kilos` < 300)



# STEP 18. CALCULATE CPUE FOR EACH ORGANISM USING NUMBER OF INDIVIDUALS

#CPUE_North_All <- CPUE_North_All %>%
 # mutate(
#    across(starts_with("Total_Number_"), ~ as.numeric(gsub(",", "", .x))),
#    Fishing_effort_hours = as.numeric(gsub(",", "", Fishing_effort_hours)),
#    Total_Crew = as.numeric(gsub(",", "", Total_Crew))
#  )

#CPUE_North_All <- CPUE_North_All %>%
#  mutate(
#    IND_CPUE_Sea_Cuc = case_when(
#      Crew == "No" ~ Total_Number_Sea_Cuc / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Sea_Cuc / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Fish = case_when(
#      Crew == "No" ~ Total_Number_Fish / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Fish / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Shark = case_when(
#      Crew == "No" ~ Total_Number_Shark / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Shark / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Octopus = case_when(
#      Crew == "No" ~ Total_Number_Octopus / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Octopus / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Sea_Horse = case_when(
#      Crew == "No" ~ Total_Number_Sea_Horse / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Sea_Horse / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Dolphin = case_when(
#      Crew == "No" ~ Total_Number_Dolphin / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Dolphin / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Ray = case_when(
#      Crew == "No" ~ Total_Number_Ray / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Ray / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Shrimp = case_when(
#      Crew == "No" ~ Total_Number_Shrimp / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Shrimp / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Turtle = case_when(
#      Crew == "No" ~ Total_Number_Turtle / Fishing_effort_hours,
#     Crew == "Yes" & !is.na(Total_Crew) ~ 
#       (Total_Number_Turtle / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Shell = case_when(
#      Crew == "No" ~ Total_Number_Shell / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Shell / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Lobster = case_when(
#      Crew == "No" ~ Total_Number_Lobster / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Lobster / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Squid = case_when(
#      Crew == "No" ~ Total_Number_Squid / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Squid / Fishing_effort_hours) / Total_Crew,
#     TRUE ~ NA_real_
#    ),
#    IND_CPUE_Eel = case_when(
#      Crew == "No" ~ Total_Number_Eel / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Eel / Fishing_effort_hours) / Total_Crew,
#      TRUE ~ NA_real_
#    ),
#    IND_CPUE_Crab = case_when(
#     Crew == "No" ~ Total_Number_Crab / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Crab / Fishing_effort_hours) / Total_Crew,
#     TRUE ~ NA_real_
#    ),
#    IND_CPUE_Urchin = case_when(
#      Crew == "No" ~ Total_Number_Urchin / Fishing_effort_hours,
#      Crew == "Yes" & !is.na(Total_Crew) ~ 
#        (Total_Number_Urchin / Fishing_effort_hours) / Total_Crew,
#     TRUE ~ NA_real_
#    )
#  )


#CPUE_North_All <- CPUE_North_All %>%
#  mutate(
#    IND_CPUE_Sea_Cuc = if_else(is.na(IND_CPUE_Sea_Cuc) & Did_you_collect == "Yes", 0, IND_CPUE_Sea_Cuc),
#    IND_CPUE_Fish = if_else(is.na(IND_CPUE_Fish) & Did_you_collect == "Yes", 0, IND_CPUE_Fish),
#    IND_CPUE_Shark = if_else(is.na(IND_CPUE_Shark) & Did_you_collect == "Yes", 0, IND_CPUE_Shark),
#    IND_CPUE_Octopus = if_else(is.na(IND_CPUE_Octopus) & Did_you_collect == "Yes", 0, IND_CPUE_Octopus),
#    IND_CPUE_Sea_Horse = if_else(is.na(IND_CPUE_Sea_Horse) & Did_you_collect == "Yes", 0, IND_CPUE_Sea_Horse),
#    IND_CPUE_Dolphin = if_else(is.na(IND_CPUE_Dolphin) & Did_you_collect == "Yes", 0, IND_CPUE_Dolphin),
#    IND_CPUE_Ray = if_else(is.na(IND_CPUE_Ray) & Did_you_collect == "Yes", 0, IND_CPUE_Ray),
#    IND_CPUE_Shrimp = if_else(is.na(IND_CPUE_Shrimp) & Did_you_collect == "Yes", 0, IND_CPUE_Shrimp),
#    IND_CPUE_Turtle = if_else(is.na(IND_CPUE_Turtle) & Did_you_collect == "Yes", 0, IND_CPUE_Turtle),
#    IND_CPUE_Shell= if_else(is.na(IND_CPUE_Shell) & Did_you_collect == "Yes", 0, IND_CPUE_Shell),
#   IND_CPUE_Lobster = if_else(is.na(IND_CPUE_Lobster) & Did_you_collect == "Yes", 0, IND_CPUE_Lobster),
#    IND_CPUE_Squid = if_else(is.na(IND_CPUE_Squid) & Did_you_collect == "Yes", 0, IND_CPUE_Squid),
#    IND_CPUE_Eel = if_else(is.na(IND_CPUE_Eel) & Did_you_collect == "Yes", 0, IND_CPUE_Eel),
#    IND_CPUE_Crab = if_else(is.na(IND_CPUE_Crab) & Did_you_collect == "Yes", 0, IND_CPUE_Crab),
#    IND_CPUE_Urchin = if_else(is.na(IND_CPUE_Urchin) & Did_you_collect == "Yes", 0, IND_CPUE_Urchin)
#  )



# STEP 19. CALCULATE CPUE FOR EACH ORGANISM USING WEIGHT

CPUE_North_All <- CPUE_North_All %>%
  mutate(
    across(starts_with("Total_Kilos_"), ~ as.numeric(gsub(",", "", .x))),
    Fishing_effort_hours = as.numeric(gsub(",", "", Fishing_effort_hours)),
    Total_Crew = as.numeric(gsub(",", "", Total_Crew))
  )

CPUE_North_All <- CPUE_North_All %>%
  mutate(
    CPUE_Sea_Cuc_kg = case_when(
      Crew == "No" ~ Total_Kilos_Sea_Cuc / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Sea_Cuc / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Fish_kg = case_when(
      Crew == "No" ~ Total_Kilos_Fish / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Fish / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Shark_kg = case_when(
      Crew == "No" ~ Total_Kilos_Shark / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Shark / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Octopus_kg = case_when(
      Crew == "No" ~ Total_Kilos_Octopus / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Octopus / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Sea_Horse_kg = case_when(
      Crew == "No" ~ Total_Kilos_Sea_Horse / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Sea_Horse / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Ray_kg = case_when(
      Crew == "No" ~ Total_Kilos_Ray / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Ray / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Shrimp_kg = case_when(
      Crew == "No" ~ Total_Kilos_Shrimp / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Shrimp / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Turtle_kg = case_when(
      Crew == "No" ~ Total_Kilos_Turtle / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Turtle / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Shell_kg = case_when(
      Crew == "No" ~ Total_Kilos_Shell / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Shell / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Lobster_kg = case_when(
      Crew == "No" ~ Total_Kilos_Lobster / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Lobster / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Squid_kg = case_when(
      Crew == "No" ~ Total_Kilos_Squid / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Squid / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Eel_kg = case_when(
      Crew == "No" ~ Total_Kilos_Eel / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Eel / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Crab_kg = case_when(
      Crew == "No" ~ Total_Kilos_Crab / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Crab / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    ),
    CPUE_Urchin_kg = case_when(
      Crew == "No" ~ Total_Kilos_Urchin / Fishing_effort_hours,
      Crew == "Yes" & !is.na(Total_Crew) ~ 
        (Total_Kilos_Urchin / Fishing_effort_hours) / Total_Crew,
      TRUE ~ NA_real_
    )
  )



# Replace NA values in CPUE columns by 0 when Did_you_collect is Yes
CPUE_North_All <- CPUE_North_All %>%
  mutate(
    CPUE_Sea_Cuc_kg = if_else(is.na(CPUE_Sea_Cuc_kg) & Did_you_collect == "Yes", 0, CPUE_Sea_Cuc_kg),
    CPUE_Fish_kg = if_else(is.na(CPUE_Fish_kg) & Did_you_collect == "Yes", 0, CPUE_Fish_kg),
    CPUE_Shark_kg = if_else(is.na(CPUE_Shark_kg) & Did_you_collect == "Yes", 0, CPUE_Shark_kg),
    CPUE_Octopus_kg = if_else(is.na(CPUE_Octopus_kg) & Did_you_collect == "Yes", 0, CPUE_Octopus_kg),
    CPUE_Sea_Horse_kg = if_else(is.na(CPUE_Sea_Horse_kg) & Did_you_collect == "Yes", 0, CPUE_Sea_Horse_kg),
    CPUE_Ray_kg = if_else(is.na(CPUE_Ray_kg) & Did_you_collect == "Yes", 0, CPUE_Ray_kg),
    CPUE_Shrimp_kg = if_else(is.na(CPUE_Shrimp_kg) & Did_you_collect == "Yes", 0, CPUE_Shrimp_kg),
    CPUE_Turtle_kg = if_else(is.na(CPUE_Turtle_kg) & Did_you_collect == "Yes", 0, CPUE_Turtle_kg),
    CPUE_Shell_kg= if_else(is.na(CPUE_Shell_kg) & Did_you_collect == "Yes", 0, CPUE_Shell_kg),
    CPUE_Lobster_kg = if_else(is.na(CPUE_Lobster_kg) & Did_you_collect == "Yes", 0, CPUE_Lobster_kg),
    CPUE_Squid_kg = if_else(is.na(CPUE_Squid_kg) & Did_you_collect == "Yes", 0, CPUE_Squid_kg),
    CPUE_Eel_kg = if_else(is.na(CPUE_Eel_kg) & Did_you_collect == "Yes", 0, CPUE_Eel_kg),
    CPUE_Crab_kg = if_else(is.na(CPUE_Crab_kg) & Did_you_collect == "Yes", 0, CPUE_Crab_kg),
    CPUE_Urchin_kg = if_else(is.na(CPUE_Urchin_kg) & Did_you_collect == "Yes", 0, CPUE_Urchin_kg)
  )


CPUE_North_All <- CPUE_North_All %>%
  mutate(
    CPUE_Sea_Horse_kg = ifelse(is.na(CPUE_Sea_Horse_kg) & Did_you_collect == "Yes", 0, CPUE_Sea_Horse_kg))

CPUE_North_All <- CPUE_North_All %>%
  mutate(across(starts_with("CPUE_"), 
                ~ ifelse(is.na(.) & Did_you_collect == "Yes", 0, .)))



CPUE_North_All <- CPUE_North_All %>%
  dplyr::select(
    Trip_ID, Region, Start_entry_date, Start_entry_time, End_entry_date, End_entry_time, Did_you_collect, Collector_name, 
    Departure_date, Return_date, Departure_time, Return_time, Overnight, Town, Fishing_spot, Crew, Number_crew_split_catch, Total_Crew, 
    Total_Men, Total_Women,
    Season, Government_Gazetted_Reserve,
    Organisms_harvested, Fishing_effort_hours, `Total_Catch_Kilos`,  
    starts_with("CPUE_"), starts_with("Total_Kilos_"), starts_with("Gear"), everything())


# STEP 20. Save new data 
write.csv(CPUE_North_All, "Data/CPUE_North_All.csv", row.names = FALSE)



avg_perkg_summary_NORTH <- CPUE_North_All %>%
  select(starts_with("Avg_")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Min = if (all(is.na(Value))) NA_real_ else round(min(Value, na.rm = TRUE), 2),
    Q1 = if (all(is.na(Value))) NA_real_ else round(quantile(Value, 0.25, na.rm = TRUE), 2),
    Median = if (all(is.na(Value))) NA_real_ else round(median(Value, na.rm = TRUE), 2),
    Mean = if (all(is.na(Value))) NA_real_ else round(mean(Value, na.rm = TRUE), 2),
    Q3 = if (all(is.na(Value))) NA_real_ else round(quantile(Value, 0.75, na.rm = TRUE), 2),
    Max = if (all(is.na(Value))) NA_real_ else round(max(Value, na.rm = TRUE), 2),
    N_NA = sum(is.na(Value)),
    N_Total = length(Value),
    .groups = "drop"
  ) %>%
  arrange(Variable)

# View the table
avg_perkg_summary_NORTH
write.csv(avg_perkg_summary_NORTH, "Data/avg_perkg_summary_NORTH.csv", row.names = FALSE)




# Create a long table of suspicious values
suspect_prices <- CPUE_North_All %>%
  select(Trip_ID, Region, Town, CPUE_Kilos, starts_with("Total_Kilos"), starts_with("Total_Avg"), starts_with("Avg_")) %>%
  pivot_longer(cols = starts_with("Avg_"), names_to = "Variable", values_to = "Value") %>%
  filter(str_detect(Variable, "_Per_kg$"), !is.na(Value), Value < 100)

# View the rows with suspiciously low per kg prices
suspect_prices



ranges_prices <- CPUE_North_All %>%
  select(starts_with("Avg_"), ends_with("_Per_kg")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(Bin = cut(Value,
                   breaks = c(0, 100, 200, 300, 400, 500, 1000, 5000, Inf),
                   labels = c("<100", "100–200", "200–300", "300–400", "400–500", "500–1000", "1000–5000", ">5000"),
                   right = FALSE)) %>%
  count(Bin, sort = FALSE)

ranges_prices
