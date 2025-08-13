
# Raw data file directly downloaded from R into csv format, not touched in Excel. Catch data for the South. Downloaded 06/09/2024

Kobo_fishing_log_NORTH_27092024 <- read.csv("Data/Kobo_fishing_log_NORTH_27092024.csv", header = TRUE)

library(tidyverse)
library(stringdist)
library(lubridate)


# CHANGE COLUMN NAMES
#when changing column names, clear environment first before running the updated code!

colnames(Kobo_fishing_log_NORTH_27092024) <- c('Start_entry_date','Start_entry_time','End_entry_date','End_entry_time','Town','Town_other','Collector_name',
                                               'Month','Day','Year','Did_you_collect','Why_no_collect','Why_no_collect_other',
                                               'Fisher_name','Gender','Gender_other','Age','Fishing_spot','Departure_time','Return_time',
                                               'Overnight','Number_nights', 'Crew','Number_crew_split_catch','Name_crew','Additional_males',
                                               'Additional_females','Organisms_harvested','Gear_sea_cuc','Number_species_sea_cuc',
                                               'Sp1_Sea_cuc','Number_Sp1_Sea_cuc','Kilos_Sp1_Sea_cuc','Purchase_buy_Sp1_Sea_cuc','Price_Sp1_Sea_cuc',
                                               'Sp2_Sea_cuc','Number_Sp2_Sea_cuc','Kilos_Sp2_Sea_cuc','Purchase_buy_Sp2_Sea_cuc','Price_Sp2_Sea_cuc',
                                               'Sp3_Sea_cuc','Number_Sp3_Sea_cuc','Kilos_Sp3_Sea_cuc','Purchase_buy_Sp3_Sea_cuc','Price_Sp3_Sea_cuc',
                                               'Sp4_Sea_cuc','Number_Sp4_Sea_cuc','Kilos_Sp4_Sea_cuc','Purchase_buy_Sp4_Sea_cuc','Price_Sp4_Sea_cuc',
                                               'Sp5_Sea_cuc','Number_Sp5_Sea_cuc','Kilos_Sp5_Sea_cuc','Purchase_buy_Sp5_Sea_cuc','Price_Sp5_Sea_cuc',
                                               'Gear_fish','Number_species_fish',
                                               'Sp1_fish','Number_Sp1_fish','Kilos_Sp1_fish','Purchase_buy_Sp1_fish','Price_Sp1_fish',
                                               'Sp2_fish','Number_Sp2_fish','Kilos_Sp2_fish','Purchase_buy_Sp2_fish','Price_Sp2_fish',
                                               'Sp3_fish','Number_Sp3_fish','Kilos_Sp3_fish','Purchase_buy_Sp3_fish','Price_Sp3_fish',
                                               'Sp4_fish','Number_Sp4_fish','Kilos_Sp4_fish','Purchase_buy_Sp4_fish','Price_Sp4_fish',
                                               'Sp5_fish','Number_Sp5_fish','Kilos_Sp5_fish','Purchase_buy_Sp5_fish','Price_Sp5_fish',
                                               'Gear_shell','Number_species_shell',
                                               'Sp1_shell','Number_Sp1_shell','Kilos_Sp1_shell','Purchase_buy_Sp1_shell','Price_Sp1_shell',
                                               'Sp2_shell','Number_Sp2_shell','Kilos_Sp2_shell','Purchase_buy_Sp2_shell','Price_Sp2_shell',
                                               'Sp3_shell','Number_Sp3_shell','Kilos_Sp3_shell','Purchase_buy_Sp3_shell','Price_Sp3_shell',
                                               'Sp4_shell','Number_Sp4_shell','Kilos_Sp4_shell','Purchase_buy_Sp4_shell','Price_Sp4_shell',
                                               'Sp5_shell','Number_Sp5_shell','Kilos_Sp5_shell','Purchase_buy_Sp5_shell','Price_Sp5_shell',
                                               'Gear_lobster','Number_species_lobster',
                                               'Sp1_lobster','Number_Sp1_lobster','Kilos_Sp1_lobster','Purchase_buy_Sp1_lobster','Price_Sp1_lobster',
                                               'Sp2_lobster','Number_Sp2_lobster','Kilos_Sp2_lobster','Purchase_buy_Sp2_lobster','Price_Sp2_lobster',
                                               'Gear_octopus','Number_octopus','Kilos_octopus','Purchase_buy_octopus','Price_octopus',
                                               'Gear_ray','Number_species_ray',
                                               'Sp1_ray','Number_Sp1_ray','Kilos_Sp1_ray','Purchase_buy_Sp1_ray','Price_Sp1_ray',
                                               'Sp2_ray','Number_Sp2_ray','Kilos_Sp2_ray','Purchase_buy_Sp2_ray','Price_Sp2_ray',
                                               'Gear_shrimp','Number_species_shrimp',
                                               'Sp1_shrimp','Number_Sp1_shrimp','Kilos_Sp1_shrimp','Purchase_buy_Sp1_shrimp','Price_Sp1_shrimp',
                                               'Sp2_shrimp','Number_Sp2_shrimp','Kilos_Sp2_shrimp','Purchase_buy_Sp2_shrimp','Price_Sp2_shrimp',
                                               'Gear_squid','Number_species_squid',
                                               'Sp1_squid','Number_Sp1_squid','Kilos_Sp1_squid','Purchase_buy_Sp1_squid','Price_Sp1_squid',
                                               'Sp2_squid','Number_Sp2_squid','Kilos_Sp2_squid','Purchase_buy_Sp2_squid','Price_Sp2_squid',
                                               'Sp3_squid','Number_Sp3_squid','Kilos_Sp3_squid','Purchase_buy_Sp3_squid','Price_Sp3_squid',
                                               'Gear_eel','Number_species_eel',
                                               'Sp1_eel','Number_Sp1_eel','Kilos_Sp1_eel','Purchase_buy_Sp1_eel','Price_Sp1_eel',
                                               'Sp2_eel','Number_Sp2_eel','Kilos_Sp2_eel','Purchase_buy_Sp2_eel','Price_Sp2_eel',
                                               'Sp3_eel','Number_Sp3_eel','Kilos_Sp3_eel','Purchase_buy_Sp3_eel','Price_Sp3_eel',
                                               'Gear_shark','Number_species_shark',
                                               'Sp1_shark','Number_Sp1_shark','Kilos_Sp1_shark','Purchase_buy_Sp1_shark','Price_Sp1_shark',
                                               'Sp2_shark','Number_Sp2_shark','Kilos_Sp2_shark','Purchase_buy_Sp2_shark','Price_Sp2_shark',
                                               'Sp3_shark','Number_Sp3_shark','Kilos_Sp3_shark','Purchase_buy_Sp3_shark','Price_Sp3_shark',
                                               'Gear_sea_horse','Number_sea_horse','Kilos_sea_horse','Purchase_buy_sea_horse','Price_sea_horse',
                                               'Kilos_dry_seaweed','Purchase_buy_dry_seaweed','Price_seaweed',
                                               'Gear_turtle','Number_species_turtle',
                                               'Sp1_turtle','Number_Sp1_turtle','Kilos_Sp1_turtle','Purchase_buy_Sp1_turtle','Price_Sp1_turtle',
                                               'Sp2_turtle','Number_Sp2_turtle','Kilos_Sp2_turtle','Purchase_buy_Sp2_turtle','Price_Sp2_turtle',
                                               'Gear_crab','Number_species_crab',
                                               'Sp1_crab','Number_Sp1_crab','Kilos_Sp1_crab','Purchase_buy_Sp1_crab','Price_Sp1_crab',
                                               'Sp2_crab','Number_Sp2_crab','Kilos_Sp2_crab','Purchase_buy_Sp2_crab','Price_Sp2_crab',
                                               'Gear_dolphin','Number_dolphin','Kilos_dolphin','Purchase_buy_dolphin','Price_dolphin',
                                               'Gear_urchin','Number_species_urchin',
                                               'Sp1_urchin','Number_Sp1_urchin','Kilos_Sp1_urchin','Purchase_buy_Sp1_urchin','Price_Sp1_urchin',
                                               'Sp2_urchin','Number_Sp2_urchin','Kilos_Sp2_urchin','Purchase_buy_Sp2_urchin','Price_Sp2_urchin',
                                               'Other_species1','Number_other_sp1','Kilos_other_sp1','Purchase_buy_other_sp1','Price_other_sp1',
                                               'Other_species2','Number_other_sp2','Kilos_other_sp2','Purchase_buy_other_sp2','Price_other_sp2',
                                               'Urchin1_other','Urchin2_other','Anarana_tanana', 'Fishing_spot_Salary_Tsandamba','Fishing_spot_list_other',
                                               'sc1_other','sc2_other','sc3_other','sc4_other','sc5_other',
                                               'fish1_other','fish2_other','fish3_other','fish4_other','fish5_other',
                                               'shell1_other','shell2_other','shell3_other','shell4_other','shell5_other',
                                               'ray1_other', 'ray2_other',
                                               'Unweighed_organisms','Number_of_unweighed',
                                               'id_','UUID','Submission_time','Validation_status','Notes',
                                               'Status', 'Submitted_by','Version','Tags','Index')
                                            


### MAKING KOBO-SOUTH-CLEAN THE CLEAN VERSION I WILL BE WORKING FROM ###

# 1. CREATE A RETURN_DATE COLUMN, BASED ON THE OVERNIGHT ANSWER
# Clean the Month column by extracting the numeric part, and merge the Month, Day and Year columns into one

kobo_NORTH_clean <- Kobo_fishing_log_NORTH_27092024 %>%
  mutate(
    Month_num = as.integer(sub("-.*", "", Month)),  # Extract numeric part of Month (e.g., "1-Jan" -> "1")
    
    # Create Start_date column by merging Year, Month, and Day
    Departure_date = make_date(Year, Month_num, Day)
  )


# I don't have actual NAs in Overnight, but empty strings "", so need to replace these empty spaces with NA
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(
    Overnight = if_else(Overnight == "", NA_character_, Overnight)  # Convert empty spaces to NA
  )

# Create the Return_date column based on the Overnight value AND Number of nights. 
  # If Overnight is NA, then Return_date = Departure_date.
  # If Overnight is Yes, then Return_date = departure_date + number of nights

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(
    Departure_date = as.Date(Departure_date, format = "%Y-%m-%d"),  # Ensure the date format is correct
    Return_date = case_when(
      Overnight == "No" ~ Departure_date,  # If Overnight is No, Return_date is the same as Departure_date
      Overnight == "Yes" & !is.na(Number_nights) ~ Departure_date + Number_nights,  # Add Number_nights to Departure_date
      Overnight == "Yes" & is.na(Number_nights) ~ Departure_date + 1  # If Number_nights is NA, add 1 day
    )
  )

# Reorder columns
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  dplyr::select(Start_entry_date, Start_entry_time, End_entry_date, End_entry_time, Town, Town_other, Collector_name, Month, Day, Year, Departure_date, Return_date, 
         Departure_time, Return_time, Overnight, Number_nights, Fishing_spot, Fishing_spot_Salary_Tsandamba, Fishing_spot_list_other, everything())




# 2. REMOVE LEADING AND TRAILING WHITE SPACE ACROSS ALL COLUMNS 

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(across(everything(), trimws))


# 3. CORRECT THE TOWN NAMES. Eg. We have Salare, Salary and Salary (Southwest), we only want Salary
# Need to use the dplyr package for this

unique(kobo_NORTH_clean$Town)
unique(kobo_NORTH_clean$Town_other)

  # Get rid of rows where Town = Tsandamba, this is the South data
kobo_NORTH_clean <- kobo_NORTH_clean %>% 
  filter(Town != "Tsandamba")


kobo_NORTH_clean <- kobo_NORTH_clean%>%
  mutate(Town = case_when(
    Town == "Ambodivahibe (Northeast)" ~ "Ambodivahibe",
    Town == "Ambavarano (Northeast)" ~ "Ambavarano",
    Town == "Ambatoloaka (Northwest)" ~ "Ambatoloaka",
    Town == "Tsandamba (Southwest)" ~ "Tsandamba",
    Town == "Nosy Sakatia (Northwest)" ~ "Nosy Sakatia",
    TRUE ~ Town
  ))

kobo_NORTH_clean <- kobo_NORTH_clean%>%
  mutate(Town_other = case_when(
    Town_other == "Ambatoloka" ~ "Ambatoloaka",
    Town_other == "Anbodivahibe" ~ "Ambodivahibe",
    Town_other == "Dzamandrar" ~ "Dzamandrary",
    Town_other == "Dzamandzar" ~ "Dzamandrary",
    Town_other == "Zamandrary" ~ "Dzamandrary",
    TRUE ~ Town_other
  ))
  
  ## Can check if the Town names have been changed correctly:

unique(kobo_NORTH_clean$Town)
unique(kobo_NORTH_clean$Town_other)


# 4. MERGE TOWN AND TOWN_OTHER COLUMNS so that whenever "Other" appears in the Town column it is replaced by the corresponding value in the Town_other column

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(Town = if_else(Town == "other", Town_other, Town))


# Remove rows where Town is "Not Listed Above"
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  filter(Town != "Not Listed Above")

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  filter(Town != "Tsandamba")

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  filter(Town != "Dzamandrary")

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  filter(Town != "")

# Get rid of the column Town_other not that we've merged both columns
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  dplyr::select(-Town_other)




# 5. MERGE FISHING_SPOT_SALARY_TSANDAMBA, FISHING_SPOT_LIST_OTHER AND FISHING_SPOT INTO 1 COLUMN
    # Replace the NA or empty string in Fishing_spot by the values in Fishing_spot_Salary_Tsandamba

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(
    Fishing_spot = if_else(is.na(Fishing_spot) | Fishing_spot == "", Fishing_spot_Salary_Tsandamba, Fishing_spot)
  )

# Get rid of the columns Fishing_spot_Salary_Tsandamba and Fishing_spot_list_other now that we've merged all 3 

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  dplyr::select(-Fishing_spot_Salary_Tsandamba, -Fishing_spot_list_other)

unique(kobo_NORTH_clean$Fishing_spot) 



# 6. NAME CORRECTIONS TO COLLECTOR NAMES 

unique(kobo_NORTH_clean$Collector_name)

name_corrections <- c("Carlot" = "Carloyss",
                      "Epsilon zafimora chartan" = "Epsilon",
                      "Ju" = "Juliana",
                      "Julianna" = "Juliana",
                      "Jiliana" = "Juliana",
                      "Liza." = "Liza",
                      "Sao" = "Soa",
                      "T" = "Tombofeno"
)

  # Apply corrections to the dataframe
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(Collector_name = recode(Collector_name, !!!name_corrections))  

unique(kobo_NORTH_clean$Collector_name) 





# 2. MERGE NUMBER_ COLUMNS.  Want to merge the columns for the number of sea cucumbers, fish, and other organisms into separate columns instead of having Nber_Sp1...
## Don't want to be looking specifically into species yet, but just organism level

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  # Merge the Number columns for Sea Cucumbers into one column
  mutate(Total_Number_Sea_Cuc = rowSums(dplyr::select(., starts_with("Number_Sp1_Sea_cuc"), 
                                               starts_with("Number_Sp2_Sea_cuc"), 
                                               starts_with("Number_Sp3_Sea_cuc"),
                                               starts_with("Number_Sp4_Sea_cuc"),
                                               starts_with("Number_Sp5_Sea_cuc")) %>%
                                          mutate_all(as.numeric),
                                        na.rm = TRUE)) %>%
  
  # Merge the Number columns for Fish into one column
  mutate(Total_Number_Fish = rowSums(dplyr::select(., starts_with("Number_Sp1_fish"), 
                                            starts_with("Number_Sp2_fish"),
                                            starts_with("Number_Sp3_fish"),
                                            starts_with("Number_Sp4_fish"),
                                            starts_with("Number_Sp5_fish")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  # Merge the Number columns for shells into one column
  mutate(Total_Number_Shell = rowSums(dplyr::select(., starts_with("Number_Sp1_shell"), 
                                             starts_with("Number_Sp2_shell"),
                                             starts_with("Number_Sp3_shell"),
                                             starts_with("Number_Sp4_shell"),
                                             starts_with("Number_Sp5_shell")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  # Merge the Number columns for Lobster into one column
  mutate(Total_Number_Lobster = rowSums(dplyr::select(., starts_with("Number_Sp1_lobster"), 
                                               starts_with("Number_Sp2_lobster")) %>%
                                          mutate_all(as.numeric),
                                        na.rm = TRUE)) %>%
  
  
  # Merge the Number columns for Ray into one column
  mutate(Total_Number_Ray = rowSums(dplyr::select(., starts_with("Number_Sp1_ray"), 
                                           starts_with("Number_Sp2_ray")) %>%
                                      mutate_all(as.numeric),
                                    na.rm = TRUE)) %>%
  
  # Merge the Number columns for Shrimp into one column
  mutate(Total_Number_Shrimp = rowSums(dplyr::select(., starts_with("Number_Sp1_shrimp"), 
                                              starts_with("Number_Sp2_shrimp")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  # Merge the Number columns for Squid into one column
  mutate(Total_Number_Squid = rowSums(dplyr::select(., starts_with("Number_Sp1_squid"), 
                                             starts_with("Number_Sp2_squid"),
                                             starts_with("Number_Sp3_squid")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  # Merge the Number columns for Eel into one column
  mutate(Total_Number_Eel = rowSums(dplyr::select(., starts_with("Number_Sp1_eel"), 
                                           starts_with("Number_Sp2_eel"),
                                           starts_with("Number_Sp3_eel")) %>%
                                      mutate_all(as.numeric),
                                    na.rm = TRUE)) %>%
  
  # Merge the Number columns for Shark into one column
  mutate(Total_Number_Shark = rowSums(dplyr::select(., starts_with("Number_Sp1_shark"), 
                                             starts_with("Number_Sp2_shark"),
                                             starts_with("Number_Sp3_shark")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  # Merge the Number columns for Turtle into one column
  mutate(Total_Number_Turtle = rowSums(dplyr::select(., starts_with("Number_Sp1_turtle"), 
                                              starts_with("Number_Sp2_turtle")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  # Merge the Number columns for Crab into one column
  mutate(Total_Number_Crab = rowSums(dplyr::select(., starts_with("Number_Sp1_crab"), 
                                            starts_with("Number_Sp2_crab")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  # Merge the Number columns for Sea Urchins into one column
  mutate(Total_Number_Urchin = rowSums(dplyr::select(., starts_with("Number_Sp1_urchin"), 
                                              starts_with("Number_Sp2_urchin")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE))


# 3. MERGE KILO COLUMNS 

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  # Merge the Kilos columns for Sea Cucumbers into one column
  mutate(Total_Kilos_Sea_Cuc = rowSums(dplyr::select(., starts_with("Kilos_Sp1_Sea_cuc"), 
                                              starts_with("Kilos_Sp2_Sea_cuc"), 
                                              starts_with("Kilos_Sp3_Sea_cuc"),
                                              starts_with("Kilos_Sp4_Sea_cuc"),
                                              starts_with("Kilos_Sp5_Sea_cuc")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for Fish into one column
  mutate(Total_Kilos_Fish = rowSums(dplyr::select(., starts_with("Kilos_Sp1_fish"), 
                                           starts_with("Kilos_Sp2_fish"),
                                           starts_with("Kilos_Sp3_fish"),
                                           starts_with("Kilos_Sp4_fish"),
                                           starts_with("Kilos_Sp5_fish")) %>%
                                      mutate_all(as.numeric),
                                    na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for shells into one column
  mutate(Total_Kilos_Shell = rowSums(dplyr::select(., starts_with("Kilos_Sp1_shell"), 
                                            starts_with("Kilos_Sp2_shell"),
                                            starts_with("Kilos_Sp3_shell"),
                                            starts_with("Kilos_Sp4_shell"),
                                            starts_with("Kilos_Sp5_shell")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for Lobster into one column
  mutate(Total_Kilos_Lobster = rowSums(dplyr::select(., starts_with("Kilos_Sp1_lobster"), 
                                              starts_with("Kilos_Sp2_lobster")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for Ray into one column
  mutate(Total_Kilos_Ray = rowSums(dplyr::select(., starts_with("Kilos_Sp1_ray"), 
                                          starts_with("Kilos_Sp2_ray")) %>%
                                     mutate_all(as.numeric),
                                   na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for shrimp into one column
  mutate(Total_Kilos_Shrimp = rowSums(dplyr::select(., starts_with("Kilos_Sp1_shrimp"), 
                                             starts_with("Kilos_Sp2_shrimp")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for squid into one column
  mutate(Total_Kilos_Squid = rowSums(dplyr::select(., starts_with("Kilos_Sp1_squid"), 
                                            starts_with("Kilos_Sp2_squid"),
                                            starts_with("Kilos_Sp3_squid")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for eel into one column
  mutate(Total_Kilos_Eel = rowSums(dplyr::select(., starts_with("Kilos_Sp1_eel"), 
                                          starts_with("Kilos_Sp2_eel"),
                                          starts_with("Kilos_Sp3_eel")) %>%
                                     mutate_all(as.numeric),
                                   na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for shark into one column
  mutate(Total_Kilos_Shark = rowSums(dplyr::select(., starts_with("Kilos_Sp1_shark"), 
                                            starts_with("Kilos_Sp2_shark"),
                                            starts_with("Kilos_Sp3_shark")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for turtle into one column
  mutate(Total_Kilos_Turtle = rowSums(dplyr::select(., starts_with("Kilos_Sp1_turtle"), 
                                             starts_with("Kilos_Sp2_turtle")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for crab into one column
  mutate(Total_Kilos_Crab = rowSums(dplyr::select(., starts_with("Kilos_Sp1_crab"), 
                                           starts_with("Kilos_Sp2_crab")) %>%
                                      mutate_all(as.numeric),
                                    na.rm = TRUE)) %>%
  
  # Merge the Kilos columns for sea urchins into one column
  mutate(Total_Kilos_Urchin = rowSums(dplyr::select(., starts_with("Kilos_Sp1_urchin"), 
                                             starts_with("Kilos_Sp2_urchin")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE))




# 4. RENAME NUMBER AND KILO COLUMNS OF ORGANISMS THAT DON'T HAVE SEVERAL SPECIES
# RENAME THE NUMBER_OCTOPUS COLUMN

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  rename(Total_Number_Octopus = Number_octopus)

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  rename(Total_Kilos_Octopus = Kilos_octopus)

# RENAME THE NUMBER_SEA_HORSE COLUMN
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  rename(Total_Number_Sea_Horse = Number_sea_horse)

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  rename(Total_Kilos_Sea_Horse = Kilos_sea_horse)

# RENAME THE NUMBER_DOLPHIN COLUMN
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  rename(Total_Number_Dolphin = Number_dolphin)







# ------- ADD PRICE COLUMNS ------- #

## Create total price columns per organism type



kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(
    Total_Avg_Price_Sea_Cuc = apply(select(., starts_with("Price_Sp1_Sea_cuc"), 
                                     starts_with("Price_Sp2_Sea_cuc"), 
                                     starts_with("Price_Sp3_Sea_cuc"),
                                     starts_with("Price_Sp4_Sea_cuc"),
                                     starts_with("Price_Sp5_Sea_cuc")) %>% 
                                mutate_all(as.numeric), 1,
                              function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Sea_Cuc = apply(select(., starts_with("Price_Sp1_Sea_cuc"), 
                                     starts_with("Price_Sp2_Sea_cuc"), 
                                     starts_with("Price_Sp3_Sea_cuc"),
                                     starts_with("Price_Sp4_Sea_cuc"),
                                     starts_with("Price_Sp5_Sea_cuc")) %>% 
                                mutate_all(as.numeric), 1,
                              function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Fish = apply(select(., starts_with("Price_Sp1_fish"),
                                  starts_with("Price_Sp2_fish"),
                                  starts_with("Price_Sp3_fish"),
                                  starts_with("Price_Sp4_fish"),
                                  starts_with("Price_Sp5_fish")) %>%
                             mutate_all(as.numeric), 1,
                           function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Fish = apply(select(., starts_with("Price_Sp1_fish"),
                                  starts_with("Price_Sp2_fish"),
                                  starts_with("Price_Sp3_fish"),
                                  starts_with("Price_Sp4_fish"),
                                  starts_with("Price_Sp5_fish")) %>%
                             mutate_all(as.numeric), 1,
                           function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Shell = apply(select(., starts_with("Price_Sp1_shell"),
                                   starts_with("Price_Sp2_shell"),
                                   starts_with("Price_Sp3_shell"),
                                   starts_with("Price_Sp4_shell"),
                                   starts_with("Price_Sp5_shell")) %>%
                              mutate_all(as.numeric), 1,
                            function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Shell = apply(select(., starts_with("Price_Sp1_shell"),
                                   starts_with("Price_Sp2_shell"),
                                   starts_with("Price_Sp3_shell"),
                                   starts_with("Price_Sp4_shell"),
                                   starts_with("Price_Sp5_shell")) %>%
                              mutate_all(as.numeric), 1,
                            function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Lobster = apply(select(., starts_with("Price_Sp1_lobster"),
                                     starts_with("Price_Sp2_lobster")) %>%
                                mutate_all(as.numeric), 1,
                              function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Lobster = apply(select(., starts_with("Price_Sp1_lobster"),
                                     starts_with("Price_Sp2_lobster")) %>%
                                mutate_all(as.numeric), 1,
                              function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Octopus = as.numeric(ifelse(is.na(Price_octopus), NA, Price_octopus)),
    Total_Med_Price_Octopus = Total_Avg_Price_Octopus,
    
    Total_Avg_Price_Ray = apply(select(., starts_with("Price_Sp1_ray"),
                                 starts_with("Price_Sp2_ray")) %>%
                            mutate_all(as.numeric), 1,
                          function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Ray = apply(select(., starts_with("Price_Sp1_ray"),
                                 starts_with("Price_Sp2_ray")) %>%
                            mutate_all(as.numeric), 1,
                          function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Shrimp = apply(select(., starts_with("Price_Sp1_shrimp"),
                                    starts_with("Price_Sp2_shrimp")) %>%
                               mutate_all(as.numeric), 1,
                             function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Shrimp = apply(select(., starts_with("Price_Sp1_shrimp"),
                                    starts_with("Price_Sp2_shrimp")) %>%
                               mutate_all(as.numeric), 1,
                             function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Squid = apply(select(., starts_with("Price_Sp1_squid"),
                                   starts_with("Price_Sp2_squid"),
                                   starts_with("Price_Sp3_squid")) %>%
                              mutate_all(as.numeric), 1,
                            function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Squid = apply(select(., starts_with("Price_Sp1_squid"),
                                   starts_with("Price_Sp2_squid"),
                                   starts_with("Price_Sp3_squid")) %>%
                              mutate_all(as.numeric), 1,
                            function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Eel = apply(select(., starts_with("Price_Sp1_eel"),
                                 starts_with("Price_Sp2_eel"),
                                 starts_with("Price_Sp3_eel")) %>%
                            mutate_all(as.numeric), 1,
                          function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Eel = apply(select(., starts_with("Price_Sp1_eel"),
                                 starts_with("Price_Sp2_eel"),
                                 starts_with("Price_Sp3_eel")) %>%
                            mutate_all(as.numeric), 1,
                          function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Shark = apply(select(., starts_with("Price_Sp1_shark"),
                                   starts_with("Price_Sp2_shark"),
                                   starts_with("Price_Sp3_shark")) %>%
                              mutate_all(as.numeric), 1,
                            function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Shark = apply(select(., starts_with("Price_Sp1_shark"),
                                   starts_with("Price_Sp2_shark"),
                                   starts_with("Price_Sp3_shark")) %>%
                              mutate_all(as.numeric), 1,
                            function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Turtle = apply(select(., starts_with("Price_Sp1_turtle"),
                                    starts_with("Price_Sp2_turtle")) %>%
                               mutate_all(as.numeric), 1,
                             function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Turtle = apply(select(., starts_with("Price_Sp1_turtle"),
                                    starts_with("Price_Sp2_turtle")) %>%
                               mutate_all(as.numeric), 1,
                             function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Crab = apply(select(., starts_with("Price_Sp1_crab"),
                                  starts_with("Price_Sp2_crab")) %>%
                             mutate_all(as.numeric), 1,
                           function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Crab = apply(select(., starts_with("Price_Sp1_crab"),
                                  starts_with("Price_Sp2_crab")) %>%
                             mutate_all(as.numeric), 1,
                           function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Urchin = apply(select(., starts_with("Price_Sp1_urchin"),
                                    starts_with("Price_Sp2_urchin")) %>%
                               mutate_all(as.numeric), 1,
                             function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)),
    
    Total_Med_Price_Urchin = apply(select(., starts_with("Price_Sp1_urchin"),
                                    starts_with("Price_Sp2_urchin")) %>%
                               mutate_all(as.numeric), 1,
                             function(x) if (all(is.na(x))) NA else median(x, na.rm = TRUE)),
    
    Total_Avg_Price_Sea_Horse = as.numeric(ifelse(is.na(Price_sea_horse), NA, Price_sea_horse)),
    Total_Med_Price_Sea_Horse = Total_Avg_Price_Sea_Horse
  )



kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(
    across(starts_with("Total_Kilos_"), ~ as.numeric(gsub(",", "", .))),
    across(starts_with("Total_Avg_Price_"), ~ as.numeric(gsub(",", "", .))),
    across(starts_with("Total_Med_Price_"), ~ as.numeric(gsub(",", "", .)))
  )


kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(
    Avg_Price_Fish_Per_kg = if_else(Total_Kilos_Fish > 0, Total_Avg_Price_Fish / Total_Kilos_Fish, NA_real_),
    Med_Price_Fish_Per_kg = if_else(Total_Kilos_Fish > 0, Total_Med_Price_Fish / Total_Kilos_Fish, NA_real_),
    
    Avg_Price_Octopus_Per_kg = if_else(Total_Kilos_Octopus > 0, Total_Avg_Price_Octopus / Total_Kilos_Octopus, NA_real_),
    Med_Price_Octopus_Per_kg = if_else(Total_Kilos_Octopus > 0, Total_Med_Price_Octopus / Total_Kilos_Octopus, NA_real_),
    
    Avg_Price_Squid_Per_kg = if_else(Total_Kilos_Squid > 0, Total_Avg_Price_Squid / Total_Kilos_Squid, NA_real_),
    Med_Price_Squid_Per_kg = if_else(Total_Kilos_Squid > 0, Total_Med_Price_Squid / Total_Kilos_Squid, NA_real_),
    
    Avg_Price_Sea_Cuc_Per_kg = if_else(Total_Kilos_Sea_Cuc > 0, Total_Avg_Price_Sea_Cuc / Total_Kilos_Sea_Cuc, NA_real_),
    Med_Price_Sea_Cuc_Per_kg = if_else(Total_Kilos_Sea_Cuc > 0, Total_Med_Price_Sea_Cuc / Total_Kilos_Sea_Cuc, NA_real_),
    
    Avg_Price_Lobster_Per_kg = if_else(Total_Kilos_Lobster > 0, Total_Avg_Price_Lobster / Total_Kilos_Lobster, NA_real_),
    Med_Price_Lobster_Per_kg = if_else(Total_Kilos_Lobster > 0, Total_Med_Price_Lobster / Total_Kilos_Lobster, NA_real_),
    
    Avg_Price_Shell_Per_kg = if_else(Total_Kilos_Shell > 0, Total_Avg_Price_Shell / Total_Kilos_Shell, NA_real_),
    Med_Price_Shell_Per_kg = if_else(Total_Kilos_Shell > 0, Total_Med_Price_Shell / Total_Kilos_Shell, NA_real_),
    
    Avg_Price_Eel_Per_kg = if_else(Total_Kilos_Eel > 0, Total_Avg_Price_Eel / Total_Kilos_Eel, NA_real_),
    Med_Price_Eel_Per_kg = if_else(Total_Kilos_Eel > 0, Total_Med_Price_Eel / Total_Kilos_Eel, NA_real_),
    
    Avg_Price_Shrimp_Per_kg = if_else(Total_Kilos_Shrimp > 0, Total_Avg_Price_Shrimp / Total_Kilos_Shrimp, NA_real_),
    Med_Price_Shrimp_Per_kg = if_else(Total_Kilos_Shrimp > 0, Total_Med_Price_Shrimp / Total_Kilos_Shrimp, NA_real_),
    
    Avg_Price_Crab_Per_kg = if_else(Total_Kilos_Crab > 0, Total_Avg_Price_Crab / Total_Kilos_Crab, NA_real_),
    Med_Price_Crab_Per_kg = if_else(Total_Kilos_Crab > 0, Total_Med_Price_Crab / Total_Kilos_Crab, NA_real_),
    
    Avg_Price_Urchin_Per_kg = if_else(Total_Kilos_Urchin > 0, Total_Avg_Price_Urchin / Total_Kilos_Urchin, NA_real_),
    Med_Price_Urchin_Per_kg = if_else(Total_Kilos_Urchin > 0, Total_Med_Price_Urchin / Total_Kilos_Urchin, NA_real_),
    
    Avg_Price_Shark_Per_kg = if_else(Total_Kilos_Shark > 0, Total_Avg_Price_Shark / Total_Kilos_Shark, NA_real_),
    Med_Price_Shark_Per_kg = if_else(Total_Kilos_Shark > 0, Total_Med_Price_Shark / Total_Kilos_Shark, NA_real_),
    
    Avg_Price_Ray_Per_kg = if_else(Total_Kilos_Ray > 0, Total_Avg_Price_Ray / Total_Kilos_Ray, NA_real_),
    Med_Price_Ray_Per_kg = if_else(Total_Kilos_Ray > 0, Total_Med_Price_Ray / Total_Kilos_Ray, NA_real_),
    
    Avg_Price_Turtle_Per_kg = if_else(Total_Kilos_Turtle > 0, Total_Avg_Price_Turtle / Total_Kilos_Turtle, NA_real_),
    Med_Price_Turtle_Per_kg = if_else(Total_Kilos_Turtle > 0, Total_Med_Price_Turtle / Total_Kilos_Turtle, NA_real_),
    
    Avg_Price_Sea_Horse_Per_kg = if_else(Total_Kilos_Sea_Horse > 0, Total_Avg_Price_Sea_Horse / Total_Kilos_Sea_Horse, NA_real_),
    Med_Price_Sea_Horse_Per_kg = if_else(Total_Kilos_Sea_Horse > 0, Total_Med_Price_Sea_Horse / Total_Kilos_Sea_Horse, NA_real_)
  )




## At first glance, Avg_ and Med_ columns seem to be similar, make sure they're not identical throughout the whole dataset.

organisms <- c("Sea_Cuc", "Fish", "Shell", "Lobster", "Octopus", "Ray", "Shrimp", 
               "Squid", "Eel", "Shark", "Turtle", "Crab", "Urchin", "Sea_Horse")

# Check if Avg and Med are always identical
identical_check <- sapply(organisms, function(org) {
  avg_col <- paste0("Avg_Price_", org)
  med_col <- paste0("Med_Price_", org)
  all_equal <- all(kobo_NORTH_clean[[avg_col]] == kobo_NORTH_clean[[med_col]] |
                     (is.na(kobo_NORTH_clean[[avg_col]]) & is.na(kobo_NORTH_clean[[med_col]])))
  return(all_equal)
})

identical_check


# Remove detailed species-level Price columns but keep aggregated ones
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  dplyr::select(-matches("^Price_Sp\\d+_"))




# 5. DELETE ALL NUMBER_SP AND KILO_SP COLUMNS (we have all combined them into one column for each organism).

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  dplyr::select(-starts_with("Number_Sp"))

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  dplyr::select(-starts_with("Kilos_Sp"))



# 6. GET RID OF UNECESSARY COLUMNS. 

colnames(kobo_NORTH_clean)

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  dplyr::select(
    -Kilos_dry_seaweed,
    -Kilos_other_sp1,
    -Kilos_other_sp2,
    -Number_other_sp1,
    -Number_other_sp2
  )




### Calculate the total number of men and women fishers per fishing trip, based on the gender of the person surveyed plus any additional crew members reported

kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(
    Additional_males = replace_na(as.numeric(Additional_males), 0),
    Additional_females = replace_na(as.numeric(Additional_females), 0),
    Total_Men = ifelse(Gender == "Male", 1, 0) + Additional_males,  
    Total_Women = ifelse(Gender == "Female", 1, 0) + Additional_females,  
    Total_Crew = Total_Men + Total_Women  
  )



range(kobo_NORTH_clean$Departure_date, na.rm = TRUE)

north_weird_dates <- kobo_NORTH_clean %>%
  filter(Departure_date < as.Date("2023-06-01"))


kobo_NORTH_clean <- kobo_NORTH_clean %>%
  filter(
    !(Departure_date < as.Date("2023-06-01"))
  )


## RENAME GENDER FROM MALE AND FEMALE TO MEN AND WOMEN 
kobo_NORTH_clean <- kobo_NORTH_clean %>%
  mutate(Gender = dplyr::recode(Gender,
                                "Male" = "Men",
                                "Female" = "Women"))

# 8. SAVE THE FINAL VERSION OF kobo_NORTH_clean. WILL BE USING THIS VERSION FOR CPUE.
write.csv(kobo_NORTH_clean, "Data/kobo_NORTH_clean.csv", row.names = FALSE)






