
## This script is the 1st step to the CPUE data from Kobo. This is for the initial cleaning.
# Cleaning column names, whitespaces, spelling mistakes in Town names, collector names, etc.
# The 2nd part of the script is cleaning the Unweighed_organisms column. This part consists in having the files named "Name_Unweighed_Corrections_SOUTH" and "Correct_names_Organism_Type"


#Raw data file directly downloaded from R into csv format, not touched in Excel. Catch data for the South. Downloaded 06/09/2024


Kobo_fishing_log_SOUTH_06092024 <- read.csv("Data/Kobo_fishing_log_SOUTH_06092024.csv", header = TRUE)
Name_Corrections_Unweighed_SOUTH2<-read.csv("Data/Name_Corrections_Unweighed_SOUTH2.csv", header = T)
Correct_names_Organism_type2<-read.csv("Data/Correct_names_Organism_Type2.csv", header = T)

library(tidyverse)
library(stringi)
library(stringdist)
library(lubridate)


# CHANGE COLUMN NAMES

colnames(Kobo_fishing_log_SOUTH_06092024) <- c('Start_entry_date','Start_entry_time','End_entry_date','End_entry_time','Town','Town_other','Collector_name',
                                               'Month','Day','Year','Did_you_collect','Why_no_collect','Why_no_collect_other',
                                               'Fisher_name','Gender','Gender_other','Age','Fishing_spot_SalTsan',
                                               'Fishing_spot_other','Fishing_spot','Departure_time','Return_time',
                                               'Overnight','Crew','Number_crew_split_catch','Name_crew','Additional_males',
                                               'Additional_females','Organisms_harvested','Gear_sea_cuc','Number_species_sea_cuc',
                                               'Sp1_Sea_cuc','Sp1_Sea_cuc_other','Number_Sp1_Sea_cuc','Kilos_Sp1_Sea_cuc','Price_Sp1_Sea_cuc',
                                               'Sp2_Sea_cuc','Sp2_Sea_cuc_other','Number_Sp2_Sea_cuc','Kilos_Sp2_Sea_cuc','Price_Sp2_Sea_cuc',
                                               'Sp3_Sea_cuc','Sp3_Sea_cuc_other','Number_Sp3_Sea_cuc','Kilos_Sp3_Sea_cuc','Price_Sp3_Sea_cuc',
                                               'Sp4_Sea_cuc','Sp4_Sea_cuc_other','Number_Sp4_Sea_cuc','Kilos_Sp4_Sea_cuc','Price_Sp4_Sea_cuc',
                                               'Sp5_Sea_cuc','Sp5_Sea_cuc_other','Number_Sp5_Sea_cuc','Kilos_Sp5_Sea_cuc','Price_Sp5_Sea_cuc',
                                               'Gear_fish','Number_species_fish',
                                               'Sp1_fish','Sp1_fish_other','Number_Sp1_fish','Kilos_Sp1_fish','Price_Sp1_fish',
                                               'Sp2_fish','Sp2_fish_other','Number_Sp2_fish','Kilos_Sp2_fish','Price_Sp2_fish',
                                               'Sp3_fish','Sp3_fish_other','Number_Sp3_fish','Kilos_Sp3_fish','Price_Sp3_fish',
                                               'Sp4_fish','Sp4_fish_other','Number_Sp4_fish','Kilos_Sp4_fish','Price_Sp4_fish',
                                               'Sp5_fish','Sp5_fish_other','Number_Sp5_fish','Kilos_Sp5_fish','Price_Sp5_fish',
                                               'Gear_shell','Number_species_shell',
                                               'Sp1_shell','Sp1_shell_other','Number_Sp1_shell','Kilos_Sp1_shell','Price_Sp1_shell',
                                               'Sp2_shell','Sp2_shell_other','Number_Sp2_shell','Kilos_Sp2_shell','Price_Sp2_shell',
                                               'Sp3_shell','Sp3_shell_other','Number_Sp3_shell','Kilos_Sp3_shell','Price_Sp3_shell',
                                               'Sp4_shell','Sp4_shell_other','Number_Sp4_shell','Kilos_Sp4_shell','Price_Sp4_shell',
                                               'Sp5_shell','Sp5_shell_other','Number_Sp5_shell','Kilos_Sp5_shell','Price_Sp5_shell',
                                               'Gear_lobster','Number_species_lobster',
                                               'Sp1_lobster','Number_Sp1_lobster','Kilos_Sp1_lobster','Price_Sp1_lobster',
                                               'Sp2_lobster','Number_Sp2_lobster','Kilos_Sp2_lobster','Price_Sp2_lobster',
                                               'Gear_octopus','Number_octopus','Kilos_octopus','Price_octopus',
                                               'Gear_ray','Number_species_ray',
                                               'Sp1_ray','Sp1_ray_other','Number_Sp1_ray','Kilos_Sp1_ray','Price_Sp1_ray',
                                               'Sp2_ray','Sp2_ray_other','Number_Sp2_ray','Kilos_Sp2_ray','Price_Sp2_ray',
                                               'Gear_shrimp','Number_species_shrimp',
                                               'Sp1_shrimp','Number_Sp1_shrimp','Kilos_Sp1_shrimp','Price_Sp1_shrimp',
                                               'Sp2_shrimp','Number_Sp2_shrimp','Kilos_Sp2_shrimp','Price_Sp2_shrimp',
                                               'Gear_squid','Number_species_squid',
                                               'Sp1_squid','Number_Sp1_squid','Kilos_Sp1_squid','Price_Sp1_squid',
                                               'Sp2_squid','Number_Sp2_squid','Kilos_Sp2_squid','Price_Sp2_squid',
                                               'Sp3_squid','Number_Sp3_squid','Kilos_Sp3_squid','Price_Sp3_squid',
                                               'Gear_eel','Number_species_eel',
                                               'Sp1_eel','Number_Sp1_eel','Kilos_Sp1_eel','Price_Sp1_eel',
                                               'Sp2_eel','Number_Sp2_eel','Kilos_Sp2_eel','Price_Sp2_eel',
                                               'Sp3_eel','Number_Sp3_eel','Kilos_Sp3_eel','Price_Sp3_eel',
                                               'Gear_shark','Number_species_shark',
                                               'Sp1_shark','Number_Sp1_shark','Kilos_Sp1_shark','Price_Sp1_shark',
                                               'Sp2_shark','Number_Sp2_shark','Kilos_Sp2_shark','Price_Sp2_shark',
                                               'Sp3_shark','Number_Sp3_shark','Kilos_Sp3_shark','Price_Sp3_shark',
                                               'Gear_sea_horse','Number_sea_horse','Kilos_sea_horse','Price_sea_horse',
                                               'Kilos_dry_seaweed','Price_seaweed',
                                               'Gear_turtle','Number_species_turtle',
                                               'Sp1_turtle','Number_Sp1_turtle','Kilos_Sp1_turtle','Price_Sp1_turtle',
                                               'Sp2_turtle','Number_Sp2_turtle','Kilos_Sp2_turtle','Price_Sp2_turtle',
                                               'Gear_crab','Number_species_crab',
                                               'Sp1_crab','Number_Sp1_crab','Kilos_Sp1_crab','Price_Sp1_crab',
                                               'Sp2_crab','Number_Sp2_crab','Kilos_Sp2_crab','Price_Sp2_crab',
                                               'Gear_dolphin','Number_dolphin','Kilos_dolphin','Price_dolphin',
                                               'Gear_urchin','Number_species_urchin',
                                               'Sp1_urchin','Sp1_urchin_other','Number_Sp1_urchin','Kilos_Sp1_urchin','Price_Sp1_urchin',
                                               'Sp2_urchin','Sp2_urchin_other','Number_Sp2_urchin','Kilos_Sp2_urchin','Price_Sp2_urchin',
                                               'Other_species','Number_other_sp','Kilos_other_sp','Price_other_sp',
                                               'Other_species_2','Number_other_sp2','Kilos_other_sp2','Price_other_sp2',
                                               'Unweighed_organisms','Number_of_unweighed','Gear_for_unweighed_organism',
                                               'Number_of_octopus_species','Species_octopus_1','Species_octopus_2','number_of_product',
                                               'marine_product','price','species_sea_horse','species_seaweed','number',
                                               'version0', 'version1','version2','version3','Kobo_ID','UUID','Submission_time','Validation_status','Notes',
                                               'Status', 'Submitted_by','Version_bis','Tags','Index')
                                               



### MAKING KOBO_SOUTH_CLEAN THE CLEAN VERSION I WILL BE WORKING FROM ###

# STEP 1. CREATE A RETURN_DATE COLUMN, BASED ON THE OVERNIGHT ANSWER.
  # We only have a Departure_date - we need the Return_date if we want to calculate CPUE.
  # Clean the Month column by extracting the numeric part, and merge the Month, Day and Year columns into one

kobo_SOUTH_clean <- Kobo_fishing_log_SOUTH_06092024 %>%
  mutate(
    Month_num = as.integer(sub("-.*", "", Month)), 
    Departure_date = make_date(Year, Month_num, Day)
  )

  # Create the Return_date column based on the Overnight value. This treats NA in Overnight as NA in Return_date

    # I don't have actual NAs in Overnight, but empty strings "", so need to replace these empty spaces with NA
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(
    Overnight = if_else(Overnight == "", NA_character_, Overnight) 
  )

    # Create the Return_date column based on the Overnight value. If Overnight is NA, then Return_date will match Departure_date.

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(
    Return_date = case_when(
      is.na(Overnight) ~ Departure_date,        # If Overnight is NA, set Return_date as Departure_date
      Overnight == "No" ~ Departure_date,       # If 'No', return Departure_date
      Overnight == "Yes" ~ Departure_date + days(1)  # If 'Yes', add 1 day to Departure_date
    )
  )

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  dplyr::select(Start_entry_date, Start_entry_time, End_entry_date, End_entry_time, Town, Town_other, Collector_name, Month, Day, Year, Departure_date, Return_date, 
         Departure_time, Return_time, Overnight, everything())




# STEP 2. REMOVE LEADING AND TRAILING WHITE SPACE ACROSS ALL COLUMNS 

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(across(everything(), trimws))


# STEP 3. CORRECT THE TOWN NAMES. 

unique(kobo_SOUTH_clean$Town)
unique(kobo_SOUTH_clean$Town_other)

kobo_SOUTH_clean %>%
  filter(Town_other == "0") %>%
  summarise(Count = n())

  # Enosy and Angisy (and Angisy soa) are not town names, and Andrahava, Imorona, Menatany, Ankiembe, Andoharano, and St. Augustin should not be a part of this dataset. 
  # Eg. We have Salare, Salary and Salary (Southwest), we only want Salary

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  filter(!Town %in% c("Imorona (Northeast)", "Menatany (Northwest)", "Andrahava (Southwest)", "St. Augustin (Southwest)",
                      "Enosy (Southwest)", "Ankiembe", "Andoharano (Southwest)", "Toliara"))

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  filter(!Town_other %in% c("Angisy soa", "Angisy", "0"))


unique(kobo_SOUTH_clean$Town)


kobo_SOUTH_clean <- kobo_SOUTH_clean%>%
  mutate(Town = case_when(
    Town == "Salare" ~ "Salary",
    Town == "Salary (Southwest)" ~ "Salary",
    Town == "Tsandamba (Southwest)" ~ "Tsandamba",
    Town == "Anakao (Southwest)" ~ "Anakao",
    Town == "Sarodrano (Southwest)" ~ "Sarodrano",
    Town == "Ambola (Southwest)" ~ "Ambola",
    Town == "Itampolo (Southwest)" ~ "Itampolo",
    TRUE ~ Town
  ))

kobo_SOUTH_clean <- kobo_SOUTH_clean%>%
  mutate(Town_other = case_when(
    Town_other == "Ankle" ~ "Ankilimiova",
    Town_other == "Ank" ~ "Ankilimiova",
    Town_other == "Ankt" ~ "Ankilimiova",
    Town_other == "T" ~ "Tsifota",
    Town_other == "Beholoke" ~ "Beheloke",
    TRUE ~ Town_other
  ))


unique(kobo_SOUTH_clean$Town)
unique(kobo_SOUTH_clean$Town_other)



# STEP 4. MERGE TOWN AND TOWN_OTHER COLUMNS so that whenever "Other" appears in the Town column it is replaced by the corresponding value in the Town_other column

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(Town = if_else(Town == "other", Town_other, Town))

    # Get rid of the column Town_other not that we've merged both columns
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  dplyr::select(-Town_other)

kobo_SOUTH_clean %>%
  filter(Town == "") %>%
  summarise(Count = n())

# Remove rows where Town is empty 
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  filter(!(Town == ""))



# STEP 5. MERGE FISHING_SPOT_SALTSAN, FISHING_SPOT_OTHER AND FISHING_SPOT INTO 1 COLUMN
    # Replace "other" in  Fishing_spot_SalTsan by the value found in Fishing_spot_other.

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(
    Fishing_spot_SalTsan = if_else(Fishing_spot_SalTsan == "other", Fishing_spot_other, Fishing_spot_SalTsan),
    Fishing_spot = if_else(is.na(Fishing_spot) | Fishing_spot == "", Fishing_spot_SalTsan, Fishing_spot)
  )

# Get rid of the columns Fishing_spot_SalTsan and Fishing_spot_other now that we've merged all 3 

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  dplyr::select(-Fishing_spot_SalTsan, -Fishing_spot_other)

unique(kobo_SOUTH_clean$Fishing_spot) 



# STEP 6. NAME CORRECTIONS TO COLLECTOR NAMES 

unique(kobo_SOUTH_clean$Collector_name)

name_corrections <- c("Gody\n" = "Gody",
                      "Gdy" = "Gody",
                      "Hofy" = "Gody",
                      "Gofy" = "Gody",
                      "Hdy" = "Gody",
                      "Ho" = "Gody",
                      "H,ofy" = "Gody",
                      "Gôdy" = "Gody",
                      "Hody" = "Gody",
                      "G" = "Gody",
                      "\nElie" = "Elie",
                      "Gpdy" = "Gody",
                      "Rollanb" = "Rolland",
                      "Sozay" = "Soazay",
                      "Elle" = "Elie",
                      "Olinah" = "Olina",
                      "Izzy" = "Zizy",
                      "Zizy\n" = "Zizy",
                      "\n\n\nZizy" = "Zizy",
                      "Soazay,zaho,niasa,fe,tsioke,mareloatse,katsimaro,olo,mandeha,anjiak\n\n\n\n" = "Soazay",
                      "Soazay,zaho, niasa, fe, tsioke, mareloatse, katsimaro, olo, mandeha, anjiak" = "Soazay",
                      "Gp" = "Gody",
                      "Aramand" = "Armand",
                      "Soazay \n" = "Soazay",
                      "Ârmand" = "Armand",
                      "Olianah" = "Olina",
                      "0lina" = "Olina",
                      "olina" = "Olina",
                      "Doa" = "Soazay",
                      "Soazay ,niasazaho,nanjoany,fetsimaro,olo,mandeha,anjiake,fa,mare,faratampoe,titsioke,iato" = "Soazay",
                      "Soazay,azafady,fa,manaopitra,ry,davo,dafaraiokoa, manaopitra" = "Soazay",
                      "Maiara  famoha reserve voalohany" = "Maiara",
                      "Soaza,zaho,tamie,le15tsiniasa,fanisy,fahasahirana,tampoke" = "Soazay",
                      "Soa" = "Soazay",
                      "Soazay, azafady fa,zahotriniasa,fa,anadahikozay,nimate,tampoke,any,morondava" = "Soazay",
                      "Soazay, azafady,fa, zahotriniasa, fa,tijala,ay,nandesiko,nataoko,amposo,ka,latsake,mivalike,tilohako,iato,henanezao,androany,le13-10-23" = "Soazay",
                      "Soazay ,manilo,hale, nantenaina,io" = "Soazay",
                      "Soazay, kiristhof,igne,mbonanilo, hâle avao" = "Soazay",
                      "Soazay , zahotriniasa, fa namonje,savatse,ankiliabo,tamie,le20,21,10,2023,zao" = "Soazay",
                      "Soazay, azafady, fa,febel,itoy, téléphone,toy,katsinahavita,asamaro" = "Soazay",
                      "FINY" = "Finy",
                      "Zi,zy" = "Zizy",
                      "Zlzy" = "Zizy",
                      "Soazay,lembo,tsimaro,olo, mandeha, anjiake,fa, toetranjo,mboraty" = "Soazay",
                      "Soazay ,zahay,lienisahira,faranaokoio,narare,talata,larobia,kamisy,zoma,maray,amie,5,ie,mate,farae,zaho,trihitako,tihataoko,fanjazaho,koa,lanitofotre,lefananjanoko,tiasako,zoma,sabotsy" = "Soazay",
                      "Fun " = "Finy",
                      "Fun" = "Finy",
                      "Soazay,zaho, leniasa,nanjoany,fe,tiolo,avy,anjiakene,avitsioke,marebibizay,farae,nimpoly,iaby,tiolo,katrimaro,tiolo,nahazaka,trioke" = "Soazay",
                      "Fin" = "Finy",
                      "Fine" = "Finy",
                      "So" = "Soazay",
                      "ARmand" = "Armand",
                      "Soazay, tsimaro, olo, mandeha, amin'ny,riake,maloto,riakiny,fa,be" = "Soazay",
                      "Finu" = "Finy",
                      "Soazay,misy,sikilony,amiay,atoy,nanomboka,tamin'ny,talata,kahatramin'izao,asabotsy,izao,ka,trinahazo,niasa" = "Soazay",
                      "Fi" = "Finy",
                      "ARMAND" = "Armand",
                      "First" = "Finy",
                      "Norina" = "Nirina",
                      "1" = "Rolland",
                      "Elie\n" = "Elie",
                      "Ely" = "Elie",
                      "Ankilimiova" = "Finy",
                      "Anakao" = "Armand",
                      "Angisy" = "Finy",
                      "Izzy'" = "Zizy",
                      "Zizy," = "Zizy",
                      "Live" = "Liva",
                      "Soazay,zaho,niasa,fe,tsioke,mareloatse,katsimaro,olo,mandeha,anjiak" = "Soazay",
                      "Sazay" = "Soazay"
)


kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(Collector_name = recode(Collector_name, !!!name_corrections))  

  # Get rid of whitespaces
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(Collector_name = gsub("\\s+", " ", trimws(Collector_name)))

unique(kobo_SOUTH_clean$Collector_name) 



# STEP 7. CORRECT GEAR TYPE. Eg. We have handline longline AND longline handline, and they should be the same under one row and not two

gear_columns_south <- kobo_SOUTH_clean %>% dplyr::select(starts_with("Gear_"))

unique_gear_types_south <- unique(unlist(gear_columns_south))
print(unique_gear_types_south)


kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(across(starts_with("Gear_"), ~ case_when(
    . %in% c("Handline Longline", "Longline Handline") ~ "Longline Handline",
    . %in% c("Handline Speargun", "Speargun Handline") ~ "Speargun Handline",
    . %in% c("Spear/hand Speargun", "Speargun Spear/hand") ~ "Speargun Spear/hand",
    . %in% c("Handline Simple net", "Simple net Handline") ~ "Simple net Handline",
    . %in% c("Handline Spear/hand", "Spear/hand Handline") ~ "Spear/hand Handline",
    . %in% c("Simple net Handline Speargun", "Speargun Simple net Handline") ~ "Speargun Simple net Handline",
    TRUE ~ .
  )))


# STEP 8. DELETING THE FIRST 26 ROWS (a lot of that was from Elie), departure or return time not present, and probably a test run.
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  slice(-c(1:26))





# STEP 9. MERGE NUMBER_ COLUMNS.  Want to merge the columns for the number of sea cucumbers, fish, and other organisms into separate columns instead of having Nber_Sp1...
## Don't want to be looking specifically into species yet, but just organism level

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(Total_Number_Sea_Cuc = rowSums(dplyr::select(., starts_with("Number_Sp1_Sea_cuc"), 
                                               starts_with("Number_Sp2_Sea_cuc"), 
                                               starts_with("Number_Sp3_Sea_cuc"),
                                               starts_with("Number_Sp4_Sea_cuc"),
                                               starts_with("Number_Sp5_Sea_cuc")) %>%
                                          mutate_all(as.numeric),
                                        na.rm = TRUE)) %>%
  
  mutate(Total_Number_Fish = rowSums(dplyr::select(., starts_with("Number_Sp1_fish"), 
                                            starts_with("Number_Sp2_fish"),
                                            starts_with("Number_Sp3_fish"),
                                            starts_with("Number_Sp4_fish"),
                                            starts_with("Number_Sp5_fish")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  mutate(Total_Number_Shell = rowSums(dplyr::select(., starts_with("Number_Sp1_shell"), 
                                              starts_with("Number_Sp2_shell"),
                                              starts_with("Number_Sp3_shell"),
                                              starts_with("Number_Sp4_shell"),
                                              starts_with("Number_Sp5_shell")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  mutate(Total_Number_Lobster = rowSums(dplyr::select(., starts_with("Number_Sp1_lobster"), 
                                               starts_with("Number_Sp2_lobster")) %>%
                                          mutate_all(as.numeric),
                                        na.rm = TRUE)) %>%
  
  
  mutate(Total_Number_Ray = rowSums(dplyr::select(., starts_with("Number_Sp1_ray"), 
                                           starts_with("Number_Sp2_ray")) %>%
                                      mutate_all(as.numeric),
                                    na.rm = TRUE)) %>%
  
  mutate(Total_Number_Shrimp = rowSums(dplyr::select(., starts_with("Number_Sp1_shrimp"), 
                                              starts_with("Number_Sp2_shrimp")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  mutate(Total_Number_Squid = rowSums(dplyr::select(., starts_with("Number_Sp1_squid"), 
                                             starts_with("Number_Sp2_squid"),
                                             starts_with("Number_Sp3_squid")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  mutate(Total_Number_Eel = rowSums(dplyr::select(., starts_with("Number_Sp1_eel"), 
                                           starts_with("Number_Sp2_eel"),
                                           starts_with("Number_Sp3_eel")) %>%
                                      mutate_all(as.numeric),
                                    na.rm = TRUE)) %>%
  
  mutate(Total_Number_Shark = rowSums(dplyr::select(., starts_with("Number_Sp1_shark"), 
                                             starts_with("Number_Sp2_shark"),
                                             starts_with("Number_Sp3_shark")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  mutate(Total_Number_Turtle = rowSums(dplyr::select(., starts_with("Number_Sp1_turtle"), 
                                              starts_with("Number_Sp2_turtle")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  mutate(Total_Number_Crab = rowSums(dplyr::select(., starts_with("Number_Sp1_crab"), 
                                            starts_with("Number_Sp2_crab")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  mutate(Total_Number_Urchin = rowSums(dplyr::select(., starts_with("Number_Sp1_urchin"), 
                                              starts_with("Number_Sp2_urchin")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE))




# STEP 10. MERGE KILO COLUMNS, just like we did for the Number_columns

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(Total_Kilos_Sea_Cuc = rowSums(dplyr::select(., starts_with("Kilos_Sp1_Sea_cuc"), 
                                              starts_with("Kilos_Sp2_Sea_cuc"), 
                                              starts_with("Kilos_Sp3_Sea_cuc"),
                                              starts_with("Kilos_Sp4_Sea_cuc"),
                                              starts_with("Kilos_Sp5_Sea_cuc")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Fish = rowSums(dplyr::select(., starts_with("Kilos_Sp1_fish"), 
                                           starts_with("Kilos_Sp2_fish"),
                                           starts_with("Kilos_Sp3_fish"),
                                           starts_with("Kilos_Sp4_fish"),
                                           starts_with("Kilos_Sp5_fish")) %>%
                                      mutate_all(as.numeric),
                                    na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Shell = rowSums(dplyr::select(., starts_with("Kilos_Sp1_shell"), 
                                             starts_with("Kilos_Sp2_shell"),
                                             starts_with("Kilos_Sp3_shell"),
                                             starts_with("Kilos_Sp4_shell"),
                                             starts_with("Kilos_Sp5_shell")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Lobster = rowSums(dplyr::select(., starts_with("Kilos_Sp1_lobster"), 
                                              starts_with("Kilos_Sp2_lobster")) %>%
                                         mutate_all(as.numeric),
                                       na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Ray = rowSums(dplyr::select(., starts_with("Kilos_Sp1_ray"), 
                                          starts_with("Kilos_Sp2_ray")) %>%
                                     mutate_all(as.numeric),
                                   na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Shrimp = rowSums(dplyr::select(., starts_with("Kilos_Sp1_shrimp"), 
                                             starts_with("Kilos_Sp2_shrimp")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Squid = rowSums(dplyr::select(., starts_with("Kilos_Sp1_squid"), 
                                            starts_with("Kilos_Sp2_squid"),
                                            starts_with("Kilos_Sp3_squid")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Eel = rowSums(dplyr::select(., starts_with("Kilos_Sp1_eel"), 
                                          starts_with("Kilos_Sp2_eel"),
                                          starts_with("Kilos_Sp3_eel")) %>%
                                     mutate_all(as.numeric),
                                   na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Shark = rowSums(dplyr::select(., starts_with("Kilos_Sp1_shark"), 
                                            starts_with("Kilos_Sp2_shark"),
                                            starts_with("Kilos_Sp3_shark")) %>%
                                       mutate_all(as.numeric),
                                     na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Turtle = rowSums(dplyr::select(., starts_with("Kilos_Sp1_turtle"), 
                                             starts_with("Kilos_Sp2_turtle")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Crab = rowSums(dplyr::select(., starts_with("Kilos_Sp1_crab"), 
                                           starts_with("Kilos_Sp2_crab")) %>%
                                      mutate_all(as.numeric),
                                    na.rm = TRUE)) %>%
  
  mutate(Total_Kilos_Urchin = rowSums(dplyr::select(., starts_with("Kilos_Sp1_urchin"), 
                                             starts_with("Kilos_Sp2_urchin")) %>%
                                        mutate_all(as.numeric),
                                      na.rm = TRUE))



# STEP 11. RENAME NUMBER AND KILO COLUMNS OF ORGANISMS THAT DON'T HAVE SEVERAL SPECIES
# RENAME THE NUMBER_OCTOPUS COLUMN

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  rename(Total_Number_Octopus = Number_octopus)

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  rename(Total_Kilos_Octopus = Kilos_octopus)

# RENAME THE NUMBER_SEA_HORSE COLUMN
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  rename(Total_Number_Sea_Horse = Number_sea_horse)

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  rename(Total_Kilos_Sea_Horse = Kilos_sea_horse)

# RENAME THE NUMBER_DOLPHIN COLUMN
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  rename(Total_Number_Dolphin = Number_dolphin)




# ------- ADD PRICE COLUMNS ------- #

## Create total price columns per organism type



kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
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

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(
    across(starts_with("Total_Kilos_"), ~ as.numeric(gsub(",", "", .))),
    across(starts_with("Total_Avg_Price_"), ~ as.numeric(gsub(",", "", .))),
    across(starts_with("Total_Med_Price_"), ~ as.numeric(gsub(",", "", .)))
  )


kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
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
  avg_col <- paste0("Total_Avg_Price_", org)
  med_col <- paste0("Total_Med_Price_", org)
  all_equal <- all(kobo_SOUTH_clean[[avg_col]] == kobo_SOUTH_clean[[med_col]] |
                     (is.na(kobo_SOUTH_clean[[avg_col]]) & is.na(kobo_SOUTH_clean[[med_col]])))
  return(all_equal)
})

identical_check


# Remove detailed species-level Price columns but keep aggregated ones
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  dplyr::select(-matches("^Price_Sp\\d+_"))


# STEP 12. DELETE ALL NUMBER_SP AND KILO_SP COLUMNS (we have all combined them into one column for each organism).

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  dplyr::select(-starts_with("Number_Sp"))

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  dplyr::select(-starts_with("Kilos_Sp"))




# STEP 13. GET RID OF UNECESSARY COLUMNS. 

colnames(kobo_SOUTH_clean)

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  dplyr::select(
    -Kilos_dry_seaweed,
    -Kilos_other_sp,
    -Kilos_other_sp2,
    -Number_of_octopus_species, # Getting rid of the 'extra' octopus species columns that we find at the end of the dataset - no data in these columns, all the data is in the 'Number_octopus' column
    -number_of_product
  )


# Count full duplicates across ALL columns
full_duplicates <- kobo_SOUTH_clean %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()



### CLEANING THE UNWEIGHED DATA ###

## MOSTLY USED CHAT GPT FOR THE SCRIPT (Species names corrections etc was done manually) ##
## TO CREATE A LIST OF SOMEWHAT UNIQUE NAMES OF UNWEIGHED SPECIES AT THE VERY BEGINNING, BEFORE ANY CLEANIGN IN R - USED CHAT GPT AND COMPLETED IT MANUALLY
## ONCE THIS WAS DONE, I ASKED ANIRA TO TRANSLATE THESE
## I THEN MANUALLY CLEANED THE REST IN THE NAME CORRECTIONS AND CORRECTED NAMES FILES



# STEP 1: NEED TO SPLIT SPECIES NAMES FROM ORGANISMS_UNWEIGHED INTO THEIR OWN INDIVIDUAL COLUMN UNWEIGHED1,2,3... 
# AFTER ANIRA'S TRANSLATION, THERE ARE STILL A LOT OF UNWEIGHED SPECIES NAMES THAT ARE GROUPED WITHIN ONE NAME BUT ARE SEVERAL DIFFERENT TYPES OF ORGANISM


kobo_SOUTH_clean$Unweighed_organism <- trimws(kobo_SOUTH_clean$Unweighed_organism)


# Standardize commas and periods to ensure proper separation

kobo_SOUTH_clean$Unweighed_organism <- gsub(",([^ ])", ", \\1", kobo_SOUTH_clean$Unweighed_organism)  # Add space after comma if no space
kobo_SOUTH_clean$Unweighed_organism <- gsub("\\.([^ ])", ". \\1", kobo_SOUTH_clean$Unweighed_organism)  # Add space after period if no space

# Replace problematic entries with NA

kobo_SOUTH_clean$Unweighed_organism <- ifelse(kobo_SOUTH_clean$Unweighed_organism %in% c("0", "00", "01", "27000"), NA, kobo_SOUTH_clean$Unweighed_organism)

# Apply splitting rules to clean specific long names

apply_custom_splitting <- function(name) {
  if (is.na(name)) return(NA)
  name <- str_replace_all(name, "Angisindolo tsontso", "Angisindolo;Tsontso")
  name <- str_replace_all(name, "Antserake atsiva bozike", "Antserak'atsiva;Bozike")
  name <- str_replace_all(name, "Antseran'katsiva nohol leme", "Antserak'atsiva;Leme")
  name <- str_replace_all(name, "Atserak ' atsiva noh bozike mena", "Antserak'atsiva;Bozike mena")
  name <- str_replace_all(name, "Badiga bozike", "Badiga;Bozike")
  name <- str_replace_all(name, "Badiga horitan'akora", "Badiga;horitan'akora")
  name <- str_replace_all(name, "Zaga horita nakora", "Zaga;Horita;Nakora")
  name <- str_replace_all(name, "Bozike joka noh lafo", "Bozike jonka;Lafo")
  name <- str_replace_all(name, "Bozike joka noh lafon", "Bozike jonka;Lafo")
  name <- str_replace_all(name, "Bozike joka noh Lafon", "Bozike jonka;Lafo")
  name <- str_replace_all(name, "Bozike jonka noho lafo", "Bozike jonka;Lafo")
  name <- str_replace_all(name, "Bozike mena noho lafo", "Bozike mena;Lafo")
  name <- str_replace_all(name, "Bozike noho lafo", "Bozike;Lafo")
  name <- str_replace_all(name, "Bozike joka noho jak'tits", "Bozike jonka;Jak'tits")
  name <- str_replace_all(name, "Bozike jonka bozikeena nohodrakake", "Bozike jonka;Bozikeena;Drakake")
  name <- str_replace_all(name, "Bozike jonka lejaleja derago", "Bozike jonka;Lejaleja;Derago")
  name <- str_replace_all(name, "Bozike mena noh bozike joka", "Bozike mena;Bozike jonka")
  name <- str_replace_all(name, "Bozike mena noh falalijatsy", "Bozike mena, Falalijatsy")
  name <- str_replace_all(name, "Bozike mena noh hovohovo", "Bozike mena;Hovohovo")
  name <- str_replace_all(name, "Bozike mena noh jakak", "Bozike mena;Jakake")
  name <- str_replace_all(name, "Bozike mena noh kabokabok", "Bozike mena;Kabokabok,")
  name <- str_replace_all(name, "Bozike mena noh lejaleja", "Bozike mena;Lejaleja")
  name <- str_replace_all(name, "Bozike mena noho jonka", "Bozike mena;Jonka")
  name <- str_replace_all(name, "Bozikemena noho jonka", "Bozike mena;Jonka")
  name <- str_replace_all(name, "Bozike mena, Falalijatsy", "Bozike mena;Falalijatsy")
  name <- str_replace_all(name, "Bozike mena noh lafon jakake noh takalo", "Bozike mena;Lafo;Jakake;Takalo")
  name <- str_replace_all(name, "Bozike mena noh Lafon jakake noh takalo", "Bozike mena;Lafo;Jakake;Takalo")
  name <- str_replace_all(name, "Bozike mena noho tsotso", "Bozike mena;Tsotso")
  name <- str_replace_all(name, "Bozike noh kabokina", "Bozike;Kabokina")
  name <- str_replace_all(name, "Bozike kabokina", "Bozike;Kabokina")
  name <- str_replace_all(name, "Bozike noho horitan'akora", "Bozike;Horitan'akora")
  name <- str_replace_all(name, "Bozike noho lejaleja", "Bozike;Lejaleja")
  name <- str_replace_all(name, "Bozike noho moroy", "Bozike;Moroy")
  name <- str_replace_all(name, "Bozike'mena noho lendralendra nohoagisy bato", "Bozike mena;Lendralendra;Agisy bato")
  name <- str_replace_all(name, "Drakake noho bozike mena", "Drakake;Bozike mena")
  name <- str_replace_all(name, "Drakake'noho'bozikemena", "Drakake;Bozike mena")
  name <- str_replace_all(name, "Jakake mena noho bozike mena", "Jakake mena;Bozike mena")
  name <- str_replace_all(name, "Jakake noho bozike mena", "Jakake;Bozike mena")
  name <- str_replace_all(name, "Jakake noh bozike mena", "Jakake;Bozike mena")
  name <- str_replace_all(name, "Drakake nozike", "Drakake;Bozike")
  name <- str_replace_all(name, "Jakake bozike", "Jakake;Bozike")
  name <- str_replace_all(name, "Faifafa noh falalijatsy", "Faifafa;Falalijatsy")
  name <- str_replace_all(name, "Faifoty noh takalo", "Fay foty;Takalo")
  name <- str_replace_all(name, "Faigitara lagnilagny", "Faigitara;Lagnilagny")
  name <- str_replace_all(name, "Falalijatsy noh sitilo", "Falalijatsy;Sitilo")
  name <- str_replace_all(name, "Zaga falalijatsy noh sitilo", "Zanga falalijatsy;Sitilo")
  name <- str_replace_all(name, "Falalindrake noho sitilo", "Falalindrake;Sitilo")
  name <- str_replace_all(name, "Falalindrake noho tona", "Falalindrake;Tona")
  name <- str_replace_all(name, "Fay rara horitanakora", "Fay rara;Horitan'akora")
  name <- str_replace_all(name, "Fay rara noho horitan'akora", "Fay rara;Horitan'akora")
  name <- str_replace_all(name, "Fia tsotso noho kabokina", "Fia tsotso;Kabokina")
  name <- str_replace_all(name, "Horita nakora noh tsotso", "Horitan'akora;Tsotso")
  name <- str_replace_all(name, "Horita nakora noho bozike joka", "Horitan'akora;Bozike jonka")
  name <- str_replace_all(name, "Horitanakora bozike jonka", "Horitan'akora;Bozike jonka")
  name <- str_replace_all(name, "Horitanakora nohokabokaboke bozike jonka", "Horitan'akor;Kabokaboke;Bozike jonka")
  name <- str_replace_all(name, "Horita nakota bozike kabokina", "Horita nakota;Bozike kabokina")
  name <- str_replace_all(name, "Horitanakora lendralendra", "Horitan'akora;Lendralendra")
  name <- str_replace_all(name, "Hovohovo noh fela", "Hovohovo;Fela")
  name <- str_replace_all(name, "Jakake noho tsotso", "Jakake;Tsotso")
  name <- str_replace_all(name, "Leme noho tsotso", "Leme;Tsotso")
  name <- str_replace_all(name, "Jakatitse bozike derago", "Jakatitse;Bozike;Derago")
  name <- str_replace_all(name, "Kabokabok noh moroy", "Kabokaboke;Moroy")
  name <- str_replace_all(name, "Kabokaboke nohomoroy", "Kabokaboke;Moroy")
  name <- str_replace_all(name, "Kabokaboke'noho bozike mena", "Kabokaboke;Bozike mena")
  name <- str_replace_all(name, "Kambokina nohobozike jonka", "Kabokina;Bozike jonka")
  name <- str_replace_all(name, "Kabokina lafo bozike", "Kabokina;Lafo;Bozike")
  name <- str_replace_all(name, "Lafo bendra kabokaboke", "Lafo;Bendra;Kabokaboke")
  name <- str_replace_all(name, "Lafo noho kabokina", "Lafo;Kabokina")
  name <- str_replace_all(name, "Tona noh kabokina", "Tona;Kabokina")
  name <- str_replace_all(name, "Lamera noh bozike mena", "Lamera;Bozike mena")
  name <- str_replace_all(name, "Lamera noho bozike", "Lamera;Bozike")
  name <- str_replace_all(name, "Lanilany noh lejaleja noh moroy", "Lanilany;Lejaleja;Moroy")
  name <- str_replace_all(name, "Lejaleja lamera", "Lejaleja;Lamera")
  name <- str_replace_all(name, "Lejaleja noh bozike joka", "Lejaleja;Bozike jonka")
  name <- str_replace_all(name, "Lejaleja noh horitadolo falalijatsy", "Lejaleja;Horitandolo;Falalijatsy")
  name <- str_replace_all(name, "Moroy noh bozike", "Moroy;Bozike")
  name <- str_replace_all(name, "Moroy noho bozike mena", "Moroy;Bozike mena")
  name <- str_replace_all(name, "Toho noho agisy ndolo", "Toho;Angisindolo")
  name <- str_replace_all(name, "Tsotso noho leme", "Tsontso;Leme")
  name <- str_replace_all(name, "Tsotso noh leme", "Tsontso;Leme")
  name <- str_replace_all(name, "Tsontso leme", "Tsontso;Leme")
  name <- str_replace_all(name, "Zaga falalijatsy noh bozike joka", "Zanga falalijatsy;Bozike jonka")
  name <- str_replace_all(name, "Zaga sitilo noh béja", "Zanga sitilo;Beja")
  name <- str_replace_all(name, "Zaga sitilo noh Béja", "Zanga sitilo;Beja")
  name <- str_replace_all(name, "Zagatirikitera bozike", "Zanga tirikitera;Bozike")
  name <- str_replace_all(name, "Zanga hebotse nohodrango nohobozike", "Zanga hebotse;Drango;Bozike")
  name <- str_replace_all(name, "Zanga liva", "Zanga;Liva")
  name <- str_replace_all(name, "Zanga sy jakake", "Zanga;Jakake")
  name <- str_replace_all(name, "Zanga sy liva ary bozike", "Zanga;Liva;Bozike")
  name <- str_replace_all(name, "Zanga sy liva Ary bozike", "Zanga;Liva;Bozike")
  name <- str_replace_all(name, "Zanga'hovohovo", "Zanga;Hovohovo")
  name <- str_replace_all(name, "Lamera Tona", "Lamera;Tona")
  name <- str_replace_all(name, "Jakake mena", "Jakake;Mena")
  name <- str_replace_all(name, "Lameratsotso", "Lamera;Tsontso")
  name <- str_replace_all(name, "Botana njonka", "Botana;Jonka")
  name <- str_replace_all(name, "Leme moloto", "Leme;Moloto")
  name <- str_replace_all(name, "Kabokanoke noho drango", "Kabokanoke;Drango")
  name <- str_replace_all(name, "Liva karioke", "Liva;Karioke")
  name <- str_replace_all(name, "Takalo noh lejaleja", "Takalo;Lejaleja")
  name <- str_replace_all(name, "takalo noh lejaleja", "Takalo;Lejaleja")
  name <- str_replace_all(name, "Jakake lafo noho", "Jakake;Lafo")
  name <- str_replace_all(name, "Falalindrake noho drakake noho lagnelagne", "Falalidrake;Drakake;Lagnilagny")
  name <- str_replace_all(name, "Tsotso bozike mena", "Tsontso;Bozike;Mena")
  name <- str_replace_all(name, "Lejaleja noh jakake", "Lejaleja;Jakake")
  name <- str_replace_all(name, "Lejaleja noh horitadolo", "Lejaleja;Horitandolo")
  name <- str_replace_all(name, "Jakake noh takalo", "Jakake;Takalo")
  name <- str_replace_all(name, "Tsotso noh kabokina", "Tsontso;Kabokina")
  name <- str_replace_all(name, "Bozike  sy jakake", "Bozike;Jakake")
  name <- str_replace_all(name, "Zanga  noho soky", "Zanga;Soky")
  name <- str_replace_all(name, "Bozike mena noh tsots", "Bozike mena;Tsontso")
  name <- str_replace_all(name, "Nakora nohokabokaboke", "Ankora;Kabokaboke")
  name <- str_replace_all(name, "nakora nohokabokaboke", "Ankora;Kabokaboke")
  
  return(name)
}



kobo_SOUTH_clean$Unweighed_organism <- sapply(kobo_SOUTH_clean$Unweighed_organism, apply_custom_splitting)

# Remove commas before numbers to ensure proper formatting

kobo_SOUTH_clean$Unweighed_organism <- gsub(",\\s*(\\d+)", " \\1", kobo_SOUTH_clean$Unweighed_organism)


# Function to clean and split species names while preserving numbers

clean_and_split_species <- function(x) {
  if (is.na(x) || x == "") return(NA)
  
  # Split by comma, period, or semicolon
  
  species_list <- unlist(strsplit(as.character(x), ",\\s*|\\.\\s*|;\\s*"))
  
  cleaned_species <- c() 
  
  for (species in species_list) {
    # Use regex to match species names followed by numbers
    matches <- gregexpr("([A-Za-zÀ-ÿ'\\-]+(?:\\s+[A-Za-zÀ-ÿ'\\-]+)*)(\\s*\\d+)?", species)
    species_parts <- regmatches(species, matches)
    
    # Append cleaned species names with their associated numbers
    for (part in species_parts[[1]]) {
      cleaned_species <- c(cleaned_species, trimws(part))  # Trim and add each part
    }
  }
  
  return(cleaned_species)
}

# Apply the function to split and clean species

split_species_list <- lapply(kobo_SOUTH_clean$Unweighed_organism, clean_and_split_species)

# Find the maximum number of species in any row

max_species_count <- max(sapply(split_species_list, length), na.rm = TRUE)

# Convert the list into a data frame with separate columns

species_df <- data.frame(do.call(rbind, lapply(split_species_list, `length<-`, max_species_count)), stringsAsFactors = FALSE)

# Rename the new columns as Unweighed1, Unweighed2, ..., based on the number of columns

colnames(species_df) <- paste0("Unweighed", 1:ncol(species_df))


# Capitalize the first word in each Unweighed column

capitalize_first_word <- function(x) {
  if (is.na(x)) return(NA)
  return(gsub("^(\\w)(.*)", "\\U\\1\\L\\2", x, perl = TRUE))  # Capitalize first letter
}

species_df <- as.data.frame(lapply(species_df, function(col) {
  sapply(col, capitalize_first_word)
}), stringsAsFactors = FALSE)

# Ensure that species_df has the same number of rows as the original dataset

species_df <- species_df[1:nrow(kobo_SOUTH_clean), ]

# Add the new species columns to the original dataset

kobo_SOUTH_clean <- cbind(kobo_SOUTH_clean, species_df)

# Separate the species names from their numbers

for (i in 1:max_species_count) {
  unweighed_col <- paste0("Unweighed", i)
  nb_unweighed_col <- paste0("Nb_unweighed", i)
  
  # Extract numbers into Nb_unweighed columns
  kobo_SOUTH_clean[[nb_unweighed_col]] <- as.numeric(sub(".*?(\\d+).*", "\\1", kobo_SOUTH_clean[[unweighed_col]]))
  
  # Remove numbers from Unweighed columns
  kobo_SOUTH_clean[[unweighed_col]] <- trimws(sub("(\\s*\\d+).*", "", kobo_SOUTH_clean[[unweighed_col]]))
}

# Rename the original column for clarity
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  rename(Organisms_Unweighed = Unweighed_organism)




## STEP 2: I HAVE VALUES IN UNWEIGHED DATA THAT AREN'T ACTUALLY SPECIES NAMES, AND WE NEED TO DELETE THEM 

# Define the list of unwanted species
unwanted_species <- c("Fe", "L", "La", "'", "M", "Ty", "Nj", "O", "00", "Za", "Nji", "Sihoa", "Sio", "Voloso",
                      "Tsimilanja'drakakemenareo", "Tsimisy", "Basimpia", "Gogo tapagony", "Goniraike", "Gony",
                      "Lakaraike", "Manday torsa", "Nanilo", "Tonotono", "Fanapie", "Fela", "Kantsane", "Manday", "Nkena",
                      "Soke tampangony", "Atserakantsiva sihoa raike", "Soke tonotono raike", "Soke lakaraike",
                      "Soke goniraike notapae", "Soke goniraike", "Soke tonontono", "Ampozo", "Noho", "Soke lakaraike", "Tsimisy", "soke lakaraike", "tsimisy",
                      "Antwa", "Fau", "Bosol", "Antwiva", "Lamai","Fianako", "Voronkaro", "Hita","Lomotse", "Ambohone", "Roro", "Ajiva", "Fitse",
                      "Drakat", "Doly", "Lafi", "Jiro", "Ra", "Benj", "Tanda", "Intse", "Lato")

# Normalize unwanted species list
normalize_name_vectorized <- function(names) {
  sapply(names, function(name) {
    if (is.na(name)) return(NA)
    trimws(tolower(name))
  })
}
unwanted_species <- normalize_name_vectorized(unwanted_species)

# Identify relevant columns (Organisms_Harvested + Unweighed1 to Unweighed19)
columns_to_modify <- c("Organisms_Harvested", grep("^Unweighed\\d+$", names(kobo_SOUTH_clean), value = TRUE))

# Replace unwanted species with NA
for (col in columns_to_modify) {
  if (sum(!is.na(kobo_SOUTH_clean[[col]])) > 0) {
    kobo_SOUTH_clean[[col]] <- ifelse(
      normalize_name_vectorized(kobo_SOUTH_clean[[col]]) %in% unwanted_species,
      NA,
      kobo_SOUTH_clean[[col]]
    )
    print(paste("Processed column:", col))
  } else {
    print(paste("Skipped empty column:", col))
  }
}

# Verification - Check if any unwanted species remain
for (col in columns_to_modify) {
  remaining <- unique(kobo_SOUTH_clean[[col]][normalize_name_vectorized(kobo_SOUTH_clean[[col]]) %in% unwanted_species & !is.na(kobo_SOUTH_clean[[col]])])
  if (length(remaining) > 0) {
    print(paste("Unwanted names still in", col, ":", paste(remaining, collapse = ", ")))
  } else {
    print(paste("No unwanted names in", col))
  }
}


# CHECK THAT THE SPECIES NAMES HAVE ACTUALLY BEEN CHANGED (SEPARATED) OR DELETED AS WANTED

# Filter rows where Unweighed1 equals "Bozike mena noh lafon jakake noh takalo", excluding NAs
filtered_rows <- kobo_SOUTH_clean[!is.na(kobo_SOUTH_clean$Unweighed1) & 
                                    kobo_SOUTH_clean$Unweighed1 == "Bozike mena noh lafon jakake noh takalo", ]

filtered_rows <- kobo_SOUTH_clean[!is.na(kobo_SOUTH_clean$Unweighed1) & 
                                    kobo_SOUTH_clean$Unweighed1 == "Gogo tapagony", ]

filtered_rows <- kobo_SOUTH_clean[!is.na(kobo_SOUTH_clean$Unweighed1) & 
                                    kobo_SOUTH_clean$Unweighed1 == "Tonotono", ]

filtered_rows <- kobo_SOUTH_clean[!is.na(kobo_SOUTH_clean$Unweighed2) & 
                                    kobo_SOUTH_clean$Unweighed2 == "béja bozike", ]



filtered_rows <- kobo_SOUTH_clean[!is.na(kobo_SOUTH_clean$Unweighed1) & 
                                    kobo_SOUTH_clean$Unweighed1 == "Bozike mena noh lafon jakake noh takalo", ]

filtered_rows <- kobo_SOUTH_clean[!is.na(kobo_SOUTH_clean$Unweighed1) & 
                                    kobo_SOUTH_clean$Unweighed1 == "Zaga sitilo noh béja", ]






## STEP 3: CORRECT THE MISSPELLINGS USING THE NAME CORRECTION EXCEL FILE CREATED
# A LOT OF SPECIES ARE EITHER MISSPELLED OR WRITTEN IN A FEW DIFFERENT WAYS. 
# THE FILE NAME_CORRECTIONS_UNWEIGHED_SOUTH GROUPS ALL THE DIFFERENT WAYS ONE SPECIES IS WRITTEN AND HAS A SEPARATE COLUMN FOR THE CORRECT SPELLING
# WE ARE GOING TO USE THIS FILE TO CLEAN OUR UNWEIGHED SPECIES NAMES

# Clean and split the 'Original_Names' column by commas
Name_Corrections_Unweighed_SOUTH2 <- Name_Corrections_Unweighed_SOUTH2 %>%
  mutate(Original_Names = strsplit(Original_Names, ",")) %>%  # Split by comma
  unnest(Original_Names) %>%  # Expand into rows
  mutate(Original_Names = str_trim(Original_Names))  # Trim whitespace around each name


# Identify all Unweighed columns in kobo_SOUTH_clean
unweighed_cols <- grep("^Unweighed", names(kobo_SOUTH_clean), value = TRUE)

# Clean leading/trailing whitespace in Unweighed columns (no lowercasing!)
for (col in unweighed_cols) {
  kobo_SOUTH_clean[[col]] <- str_trim(kobo_SOUTH_clean[[col]])
}


# Function to replace species names based on corrections
correct_species_names <- function(column, corrections) {
  corrected_column <- column  # Copy the input column
  for (i in seq_len(nrow(corrections))) {
    # Replace exact matches of original names with correct names, preserving case
    corrected_column[corrected_column == corrections$Original_Names[i]] <- corrections$Correct_Names[i]
  }
  return(corrected_column)
}

# Apply corrections to each Unweighed column
for (col in unweighed_cols) {
  kobo_SOUTH_clean[[col]] <- correct_species_names(kobo_SOUTH_clean[[col]], Name_Corrections_Unweighed_SOUTH2)
}




## STEP 4: WE WANT TO KNOW IF WE HAVE ANY ROWS WHERE THERE IS NO DATA IN NUMBER_OF_UNWEIGHED OR IN COLUMNS IN NB_UNWEIGHED1,2,3...
# IF THIS IS THE CASE THIS MEANTS THAT WE HAVE NO USEFUL DATA FOR UNWEIGHED SPECIES AND WE CAN'T CORRECTLY USE THIS DATA

# Identify the Nb_unweighed columns
nb_unweighed_cols <- grep("^Nb_unweighed", names(kobo_SOUTH_clean), value = TRUE)

# Find rows where Organisms_Unweighed has data (not NA or empty string)
# and Number_of_Unweighed + all Nb_unweighed columns are NA
problematic_rows <- kobo_SOUTH_clean %>%
  filter(
    Organisms_Unweighed != "" & !is.na(Organisms_Unweighed) &   # Data exists in Organisms_Unweighed
      is.na(Number_of_unweighed) &                               # Number_of_Unweighed is NA
      rowSums(!is.na(dplyr::select(., all_of(nb_unweighed_cols)))) == 0 # All Nb_unweighed columns are NA
  )




# I HAVE 62 ROWS WHERE I HAVE NO DATA ON NUMBER OF PRODUCT for unweighed organisms, SO THIS IS USELESS FOR MY CPUE CALCULATION, so I'm going to transform that as NAs, so it's as if I don't have unweighed organisms for that fishing trip

# Define the list of species names to replace with NA
species_to_na <- c("Bozike", "Amalo", "Tsy misy", "Fia gogo", "Gogo tapagony", 
                   "Zanga", "Lomots", "Tonotono", "Tona", 
                   "Jakake", "Akio", "Tsabeaky", "Tsimisy", "Gogo tampasihoa", "Katsa tapa sihoa", "Zanga sihoaraiki",
                   "Soke", "Soke tampadaka", "Soky", "Soke lakaraike")  # Removed "Soke" from the list to keep those rows

# Identify all Unweighed columns
unweighed_cols <- grep("^Unweighed", names(kobo_SOUTH_clean), value = TRUE)

# Combine Organisms_Unweighed and Unweighed columns into a single vector
cols_to_modify <- c("Organisms_Unweighed", unweighed_cols)

# Loop through rows to handle the replacement logic
for (i in 1:nrow(kobo_SOUTH_clean)) {
  # Check if Unweighed1 contains a species to replace AND no data in Number_of_Unweighed or Nb_Unweighed columns
  if (
    kobo_SOUTH_clean$Unweighed1[i] %in% species_to_na && 
    is.na(kobo_SOUTH_clean$Number_of_unweighed[i]) && 
    all(is.na(kobo_SOUTH_clean[i, grep("^Nb_Unweighed", names(kobo_SOUTH_clean))]))
  ) {
    # Replace values in Organisms_Unweighed and all Unweighed columns with NA
    kobo_SOUTH_clean[i, cols_to_modify] <- NA
  }
}


  ## Maybe actually better to just drop these 62 rows with no unweighed data (just species names) rather than transforming that as NA and pretending there is no unweighed but keeping the fishing trip and underestimating the catch

# Identify Nb_unweighed columns
nb_unweighed_cols <- grep("^Nb_unweighed", names(kobo_SOUTH_clean), value = TRUE)

# Identify problematic rows
problematic_rows <- kobo_SOUTH_clean %>%
  filter(
    Organisms_Unweighed != "" & !is.na(Organisms_Unweighed) &
      is.na(Number_of_unweighed) &
      rowSums(!is.na(dplyr::select(., all_of(nb_unweighed_cols)))) == 0
  )

nrow(problematic_rows)  # Should return 31

kobo_SOUTH_clean <- anti_join(kobo_SOUTH_clean, problematic_rows)



## STEP 5: WE NOW WANT TO ASSIGN AN ORGANISM TYPE TO EACH SPECIES NAME IN OUR UNWEIGHED COLUMNS
# Correct_names_Organism_type HAS A COLUMN CORRECT_NAMES AND A COLUMN ORGANISM_TYPE

# We need to clean the Correct_names_Organism_type file, getting rid of accents, whitespace, and changing all names ot lower case

# Remove unnecessary Unweighed_organisms column
kobo_SOUTH_clean <- kobo_SOUTH_clean %>% dplyr::select(-Unweighed_organisms)


### Define Normalization Function
# Function to remove accents, convert to lowercase, and trim whitespace
normalize_name <- function(name) {
  if (is.na(name)) return(NA)
  stri_trans_general(str = trimws(tolower(name)), id = "Latin-ASCII")
}

### Normalize Correct_Names
# Apply normalization to Correct_Names for comparison
Correct_names_Organism_type2$Normalized_Names <- sapply(Correct_names_Organism_type2$Correct_Names, normalize_name)

# Check normalization
normalized_correct_names <- Correct_names_Organism_type2$Normalized_Names

# Debug mismatched species (optional step)
for (mismatch in c("zanga benono", "fia angy", "béja bozike", "hâle")) {
  print(paste("Mismatch:", mismatch))
  print(paste("In Correct_Names:", mismatch %in% normalized_correct_names))
}

### Normalize kobo_SOUTH_clean
# Identify all Unweighed columns
columns_to_normalize <- grep("^Unweighed\\d+$", names(kobo_SOUTH_clean), value = TRUE)

# Normalize species names in kobo_SOUTH_clean columns
for (col in columns_to_normalize) {
  kobo_SOUTH_clean[[col]] <- sapply(kobo_SOUTH_clean[[col]], normalize_name)
}

### Remove Invalid Species
# Define invalid species names and normalize them
species_to_na <- c("Amalo", "Tsy misy", "Fia gogo", "Gogo tapagony", "Lomots", "Tonotono", "Tona", 
                   "Jakake", "Akio", "Tsabeaky", "Tsimisy", "Gogo tampasihoa", "Katsa tapa sihoa", 
                   "Zanga sihoaraiki", "Soke", "Soke tampadaka", "Soky", "Soke lakaraike")
species_to_na <- sapply(species_to_na, normalize_name)

# Replace invalid species with NA
for (col in columns_to_normalize) {
  kobo_SOUTH_clean[[col]] <- ifelse(
    kobo_SOUTH_clean[[col]] %in% species_to_na, NA, kobo_SOUTH_clean[[col]]
  )
}

### Check Remaining Mismatches
# Verify if all species in kobo_SOUTH_clean match with Correct_Names
for (col in columns_to_normalize) {
  unmatched <- unique(kobo_SOUTH_clean[[col]][
    !kobo_SOUTH_clean[[col]] %in% Correct_names_Organism_type2$Normalized_Names & !is.na(kobo_SOUTH_clean[[col]])
  ])
  if (length(unmatched) > 0) {
    print(paste("Unmatched species in", col, ":", paste(unmatched, collapse = ", ")))
  } else {
    print(paste("All species in", col, "match with Correct_Names."))
  }
}


all_matched <- all(sapply(columns_to_normalize, function(col) {
  all(kobo_SOUTH_clean[[col]] %in% Correct_names_Organism_type2$Normalized_Names | is.na(kobo_SOUTH_clean[[col]]))
}))

if (all_matched) {
  cat("All unweighed species now match with reference list. Ready for merge.\n")
} else {
  cat("Some species still unmatched. Check warnings above.\n")
}




## STEP 6: WE ALREADY HAVE COLUMNS CALLED NUMERIC_TOTAL_NUMBER IN THE FILE, AND THE SCRIPT BELOW IS GOING TO BE CREATING COLUMNS WITH THE SAME NAME
# WE WANT TO DELETE THE ALREADY EXISTING NUMERIC_TOTAL_NUMBER COLUMNS, AS OUR TOTAL_NUMBER COLUMNS HAVE THE EXACT SAME DATA

# Identify columns starting with "numeric_Total_Number"
numeric_total_cols <- grep("^numeric_Total_Number_", names(kobo_SOUTH_clean), value = TRUE)

# Remove these columns from the data frame
kobo_SOUTH_clean <- kobo_SOUTH_clean[, !names(kobo_SOUTH_clean) %in% numeric_total_cols]

# Confirm removal
print("Removed numeric_Total_Number_* columns:")
print(numeric_total_cols)




# Identify columns starting with "numeric_Total_Number"
numeric_total_cols <- grep("^numeric_Total_Kilos_", names(kobo_SOUTH_clean), value = TRUE)

# Remove these columns from the data frame
kobo_SOUTH_clean <- kobo_SOUTH_clean[, !names(kobo_SOUTH_clean) %in% numeric_total_cols]

# Confirm removal
print("Removed numeric_Total_Kilos_* columns:")
print(numeric_total_cols)



## STEP 7: ASSIGN UNWEIGHED COUNTS TO ORGANISM TYPES
  # This step integrates unweighed organism data into numeric_Total_Number_* columns.
   # - If species-specific counts (Nb_unweighed*) are available, they are added directly based on species-organism mapping.
   # - If no Nb_unweighed data exist, Number_of_unweighed is distributed proportionally based on the frequency of organism types listed in Unweighed columns.
    #   For example, if Unweighed1 = Fish and Unweighed2 = Fish and Unweighed3 = Squid, and Number_of_unweighed = 9, then Fish receives 6 and Squid receives 3.



  # Define all organism types
  organism_types <- c("Fish", "Eel", "Shark", "Shell", "Sea_Cuc", "Urchin", "Lobster", "Ray", "Shrimp", "Squid", "Turtle", "Crab", "Octopus")
  
  # Step 1: Initialize numeric_Total_Number_* columns if not already present
  for (organism in organism_types) {
    col_name <- paste0("numeric_Total_Number_", organism)
    if (!col_name %in% names(kobo_SOUTH_clean)) {
      kobo_SOUTH_clean[[col_name]] <- 0
    }
  }
  
  # Step 2: Setup for processing
  unweighed_cols <- grep("^Unweighed\\d+$", names(kobo_SOUTH_clean), value = TRUE)
  nb_unweighed_cols <- grep("^Nb_unweighed\\d+$", names(kobo_SOUTH_clean), value = TRUE)
  
  # Step 3: Create lookup table for species → organism type
  species_lookup <- setNames(Correct_names_Organism_type2$Organism_Type, Correct_names_Organism_type2$Normalized_Names)
  
  # Step 4: Loop through each row
  for (i in seq_len(nrow(kobo_SOUTH_clean))) {
    row <- kobo_SOUTH_clean[i, ]
    additions <- setNames(rep(0, length(organism_types)), organism_types)
    
    ## 4a. Use Nb_unweighed values if provided
    for (j in seq_along(nb_unweighed_cols)) {
      nb_col <- nb_unweighed_cols[j]
      unweighed_col <- unweighed_cols[j]
      
      nb_val <- row[[nb_col]]
      species_name <- row[[unweighed_col]]
      
      if (!is.na(nb_val) && !is.na(species_name)) {
        if (species_name %in% names(species_lookup)) {
          organism_type <- species_lookup[[species_name]]
          additions[organism_type] <- additions[organism_type] + as.numeric(nb_val)
        }
      }
    }
    
    ## 4b. If Nb_unweighed is missing but Number_of_unweighed is present, distribute proportionally
    has_nb_data <- any(!is.na(row[nb_unweighed_cols]))
    if (!has_nb_data && !is.na(row$Number_of_unweighed)) {
      organism_list <- unlist(lapply(unweighed_cols, function(col) {
        species_name <- row[[col]]
        if (!is.na(species_name) && species_name %in% names(species_lookup)) {
          species_lookup[[species_name]]
        } else {
          NA
        }
      }))
      
      organism_list <- na.omit(organism_list)
      
      if (length(organism_list) > 0) {
        freq_table <- table(organism_list)
        total_mentions <- sum(freq_table)
        
        for (org_type in names(freq_table)) {
          proportion <- freq_table[[org_type]] / total_mentions
          additions[org_type] <- additions[org_type] + (as.numeric(row$Number_of_unweighed) * proportion)
        }
      }
    }
    
    ## 4c. Apply additions to numeric_Total_Number_* columns
    for (organism_type in names(additions)) {
      target_col <- paste0("numeric_Total_Number_", organism_type)
      
      if (target_col %in% colnames(kobo_SOUTH_clean) && !is.na(additions[[organism_type]])) {
        kobo_SOUTH_clean[i, target_col] <- kobo_SOUTH_clean[i, target_col] + additions[[organism_type]]
      }
    }
  }
  
  
  kobo_SOUTH_clean %>%
    slice(165) %>%
    dplyr::select(Number_of_unweighed, Nb_unweighed1, Nb_unweighed2, Unweighed1, Unweighed2, Total_Number_Squid, Total_Number_Sea_Cuc, numeric_Total_Number_Squid, numeric_Total_Number_Sea_Cuc)
  
  kobo_SOUTH_clean %>%
    slice(439) %>%
    dplyr::select(Number_of_unweighed, Nb_unweighed1, Nb_unweighed2, Unweighed1, Unweighed2, Total_Number_Fish,numeric_Total_Number_Fish)
  
  
### WHILE WE STILL ONLY HAVE UWNEIGHED DATA IN OUR NEW NUMERIC_TOTAL_NUMBER COLUMNS, LET'S CALCULATE A WEIGHT AVERAGE FOR EACH ORGANISM IN ORDER TO APPLY THIS WEIGHT TO UNWEIGHED DATA

# Step 1: Calculate the average weight per individual for each organism type
#organism_types <- c("Fish", "Eel", "Shark", "Shell", "Sea_Cuc", "Urchin", "Lobster", "Ray", "Shrimp", "Squid", "Turtle", "Crab", "Octopus")


# Initialize a named list to store average weights
#average_weights <- list()
  # Convert Columns to Numeric
#for (organism in organism_types) {
#  total_kilos_col <- paste0("Total_Kilos_", organism)
#  total_number_col <- paste0("Total_Number_", organism)
  
  # Convert columns to numeric (suppress warnings for coercion)
#  kobo_SOUTH_clean[[total_kilos_col]] <- as.numeric(gsub(",", "", kobo_SOUTH_clean[[total_kilos_col]]))
#  kobo_SOUTH_clean[[total_number_col]] <- as.numeric(gsub(",", "", kobo_SOUTH_clean[[total_number_col]]))
#}


#for (organism in organism_types) {
  # Columns for total numbers and total kilos
#  total_number_col <- paste0("Total_Number_", organism)
#  total_kilos_col <- paste0("Total_Kilos_", organism)
  
  # Calculate the total weight and total number across all rows
#  total_weight <- sum(kobo_SOUTH_clean[[total_kilos_col]], na.rm = TRUE)
#  total_number <- sum(kobo_SOUTH_clean[[total_number_col]], na.rm = TRUE)
  
  # Calculate the average weight per individual (grams per individual)
#  if (total_number > 0) {
#    average_weights[[organism]] <- (total_weight / total_number)
#  } else {
#    average_weights[[organism]] <- NA  # No data for this organism type
#  }
#}



## IMPORTANT: Above averages are calculated without filtering out data entry errors (example. Total number of fish = 15,000 in 1 fishing trip.)
  # Below, I am filtering out from the average weights calculations trips that have Total Catch in Kilos > 300, or Total Catch in Number of Individuals > 1000 just to be safe



# Initialize list
average_weights <- list()

for (organism in organism_types) {
  total_kilos_col <- paste0("Total_Kilos_", organism)
  total_number_col <- paste0("Total_Number_", organism)
  
  # Convert columns to numeric
  kobo_SOUTH_clean[[total_kilos_col]] <- as.numeric(gsub(",", "", kobo_SOUTH_clean[[total_kilos_col]]))
  kobo_SOUTH_clean[[total_number_col]] <- as.numeric(gsub(",", "", kobo_SOUTH_clean[[total_number_col]]))
  
  # Step 1: Filter out implausible records (e.g., >1000 individuals or >99 kg in one trip)
  filtered <- kobo_SOUTH_clean %>%
    filter(
      !is.na(.data[[total_kilos_col]]) & !is.na(.data[[total_number_col]]),
      .data[[total_kilos_col]] <= 300,
      .data[[total_number_col]] <= 1000
    )
  
  # Step 2: Compute totals using only filtered data
  total_weight <- sum(filtered[[total_kilos_col]], na.rm = TRUE)
  total_number <- sum(filtered[[total_number_col]], na.rm = TRUE)
  
  # Step 3: Average weight
  if (total_number > 0) {
    average_weights[[organism]] <- total_weight / total_number
  } else {
    average_weights[[organism]] <- NA
  }
}





# Step 2: Apply the average weight to estimate weights for unweighed individuals
for (organism in organism_types) {
  # Columns for total numbers and total kilos
  total_number_col <- paste0("Total_Number_", organism)
  total_kilos_col <- paste0("Total_Kilos_", organism)
  
  # Estimate weight for rows where Total_Kilos is zero but Total_Number is > 0
  kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
    mutate(
      !!sym(total_kilos_col) := ifelse(
        !!sym(total_kilos_col) == 0 & !!sym(total_number_col) > 0,
        !!sym(total_number_col) * average_weights[[organism]],  # Apply average weight
        !!sym(total_kilos_col)  # Keep existing values otherwise
      )
    )
}

  # Convert Columns to Numeric
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(across(starts_with("Total_Kilos_"), ~ as.numeric(gsub(",", "", .x))))

# Step 3: Round estimated weights for consistency
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(across(starts_with("Total_Kilos_"), ~ round(.x, 1)))

# Check the calculated averages
print(average_weights)




### APPLYING THE AVERAGE WEIGHT OF SEA CUCUMBERS COLLECTED BY CHARLOTTE AND MIZA TO TOTAL_NUMBER_SEA_CUC TO HAVE WEIGHT VALUES IN TOTAL_KILOS_SEA_CUC

# Step 1: Ensure the column Total_Kilos_Sea_Cuc exists
#if (!"Total_Kilos_Sea_Cuc" %in% colnames(kobo_SOUTH_clean)) {
#  kobo_SOUTH_clean$Total_Kilos_Sea_Cuc <- 0
#}

# Step 2: Update Total_Kilos_Sea_Cuc based on Total_Number_Sea_Cuc and average weight
#average_weight_grams <- 216.8  # Average weight in grams
#average_weight_kilos <- average_weight_grams / 1000  # Convert to kilograms

#kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
#  mutate(
#    Total_Kilos_Sea_Cuc = ifelse(
#      Total_Number_Sea_Cuc > 0,  # Only update rows with non-zero sea cucumber numbers
#      Total_Number_Sea_Cuc * average_weight_kilos,  # Calculate weight
#      Total_Kilos_Sea_Cuc  # Leave as is if Total_Number_Sea_Cuc is 0
#    )
# )



# Step 1: Ensure the column exists
if (!"numeric_Total_Kilos_Sea_Cuc" %in% names(kobo_SOUTH_clean)) {
  kobo_SOUTH_clean$numeric_Total_Kilos_Sea_Cuc <- 0
}

# Step 2: Define average weight
average_weight_kilos <- 216.8 / 1000

# Step 3: Apply average only to Charlotte/Miza rows with unweighed sea cucumbers
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(
    numeric_Total_Kilos_Sea_Cuc = ifelse(
      Collector_name %in% c("Charlotte", "Miza") &
        numeric_Total_Number_Sea_Cuc > 0 &
        numeric_Total_Kilos_Sea_Cuc == 0,
      numeric_Total_Number_Sea_Cuc * average_weight_kilos,
      numeric_Total_Kilos_Sea_Cuc
    )
  ) %>%
  mutate(
    numeric_Total_Kilos_Sea_Cuc = round(as.numeric(numeric_Total_Kilos_Sea_Cuc), 1)
  )



## CREATE NUMERIC_TOTAL_KILOS_* COLUMNS BY APPLYING THE WEIGHT AVERAGES - MULTIPLY VALUES IN NUMERIC_TTOAL_NUMBER BY THE AVERAGE FOR EACH ORGANISM TYPE

# Define a mapping of organism names to match column names
organism_column_mapping <- list(
  "Fish" = "Fish",
  "Eel" = "Eel",
  "Shark" = "Shark",
  "Sea_Cuc" = "Sea_Cuc",  
  "Urchin" = "Urchin",
  "Lobster" = "Lobster",
  "Ray" = "Ray",
  "Shrimp" = "Shrimp",
  "Squid" = "Squid",
  "Turtle" = "Turtle",
  "Crab" = "Crab",
  "Octopus" = "Octopus",
  "Shell" = "Shell"
)



# Step 2: Round the values in numeric_Total_Kilos_* columns for consistency
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(across(starts_with("numeric_Total_Kilos_"), ~ round(.x, 1)))

for (organism in names(organism_column_mapping)) {
  col_suffix <- organism_column_mapping[[organism]]
  
  numeric_total_number_col <- paste0("numeric_Total_Number_", col_suffix)
  numeric_total_kilos_col <- paste0("numeric_Total_Kilos_", col_suffix)
  
  if (!numeric_total_kilos_col %in% names(kobo_SOUTH_clean)) {
    kobo_SOUTH_clean[[numeric_total_kilos_col]] <- 0
  }
  
  # Skip recalculating Sea Cucumbers — keep the 216.8g estimates
  if (organism == "Sea_Cuc") next
  
  # Apply average weights to all other organisms
  kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
    mutate(
      !!sym(numeric_total_kilos_col) := !!sym(numeric_total_number_col) * average_weights[[organism]]
    )
}





## STEP 8: THIS PREVIOUS STEP ONLY ADDS THE TOTAL NUMBER FOR THE ORGANISMS THAT ARE UNWEIGHED
# NEED TO ADD A STEP WHERE WE DIRECTLY COPY VALUES FROM Total_Number_* columns INTO THEIR CORRESPONDING numeric_Total_Number_* COLUMNS. 


### BUT FIRST BEFORE DOING THAT:
### PROBLEM IS THAT TOTAL_NUMBER COLUMNS HAVE A CAPITAL LETTER FOR ORGANISM TYPES BUT NEW NUMERIC_TOTAL_NUMBER COLUMNS DO NOT HAVE A CAPITAL LETTER FOR ORGANISM TYPES
### ADD CAPITAL LETTER TO FIRST LETTER OF ORGANISM TYPE IN NUMERIC_TOTAL_NUMBER COLUMNS

# Step 1: Identify all Total_Number_* columns
total_number_cols <- grep("^Total_Number_", names(kobo_SOUTH_clean), value = TRUE)

# Step 2: Rename numeric_Total_Number_* columns to match the capitalization
for (col in total_number_cols) {
  # Extract the organism type with correct capitalization
  organism_type <- sub("^Total_Number_", "", col)  # e.g., Fish, Eel
  
  # Define the current and new column names
  old_col <- paste0("numeric_Total_Number_", tolower(organism_type))
  new_col <- paste0("numeric_Total_Number_", organism_type)
  
  # Rename the column if it exists
  if (old_col %in% colnames(kobo_SOUTH_clean)) {
    names(kobo_SOUTH_clean)[names(kobo_SOUTH_clean) == old_col] <- new_col
  }
}

# Confirm the changes
print("Renamed numeric_Total_Number_* columns to match Total_Number_* capitalization.")



## Directly copy values from Total_Number_* columns into their corresponding numeric_Total_Number_* columns. 

# Identify all Total_Number_* columns
total_number_cols <- grep("^Total_Number_", names(kobo_SOUTH_clean), value = TRUE)

# Add values from Total_Number_* columns to numeric_Total_Number_* columns
for (col in total_number_cols) {
  # Construct the target numeric_Total_Number_* column
  target_col <- gsub("^Total_Number_", "numeric_Total_Number_", col)
  
  # Ensure the target column exists; if not, create it with default zeros
  if (!target_col %in% names(kobo_SOUTH_clean)) {
    kobo_SOUTH_clean[[target_col]] <- 0
  }
  
  # Add the values from Total_Number_* to numeric_Total_Number_*
  kobo_SOUTH_clean[[target_col]] <- kobo_SOUTH_clean[[target_col]] + as.numeric(kobo_SOUTH_clean[[col]])
}




### I NOW WANT TO DO WHAT I DID FOR NUMBER OF WEIGHED PRODUCT, I WANT TO COPY DIRECTLY VALUES IN TOTAL_KILOS TO NUMERIC_TOTAL_KILOS THAT WAY I HAVE ALL ORGANISMS, WEIGHED AND UNWEIGHED, IN NUMERIC COLUMNS



## Directly copy values from Total_Kilos_* columns into their corresponding numeric_Total_Kilos_* columns. 

# Identify all Total_Number_* columns
total_kilos_cols <- grep("^Total_Kilos_", names(kobo_SOUTH_clean), value = TRUE)

# Add values from Total_Number_* columns to numeric_Total_Number_* columns
for (col in total_kilos_cols) {
  # Construct the target numeric_Total_Number_* column
  target_col <- gsub("^Total_Kilos_", "numeric_Total_Kilos_", col)
  
  # Ensure the target column exists; if not, create it with default zeros
  if (!target_col %in% names(kobo_SOUTH_clean)) {
    kobo_SOUTH_clean[[target_col]] <- 0
  }
  
  # Add the values from Total_Number_* to numeric_Total_Number_*
  kobo_SOUTH_clean[[target_col]] <- kobo_SOUTH_clean[[target_col]] + as.numeric(kobo_SOUTH_clean[[col]])
}





# 1. Remove the original weighed-only columns
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  dplyr::select(-starts_with("Total_Kilos_"),
         -starts_with("Total_Number_"))

# 2. Rename numeric_Total_Kilos_* → Total_Kilos_* and numeric_Total_Number_* → Total_Number_*
names(kobo_SOUTH_clean) <- gsub("^numeric_Total_Kilos_", "Total_Kilos_", names(kobo_SOUTH_clean))
names(kobo_SOUTH_clean) <- gsub("^numeric_Total_Number_", "Total_Number_", names(kobo_SOUTH_clean))


### Calculate the total number of men and women fishers per fishing trip, based on the gender of the person surveyed plus any additional crew members reported

kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(
    Additional_males = replace_na(as.numeric(Additional_males), 0),
    Additional_females = replace_na(as.numeric(Additional_females), 0),
    Total_Men = ifelse(Gender == "Male", 1, 0) + Additional_males,  
    Total_Women = ifelse(Gender == "Female", 1, 0) + Additional_females,  
    Total_Crew = Total_Men + Total_Women  
  )



## CLEANING START AND END DATES - SOUTH SHOWS DATES IN DECEMBER 2024 - NOT POSSIBLE - Data downloaded in September 2024.

range(kobo_SOUTH_clean$Departure_date, na.rm = TRUE)

SW_weird_dates <- kobo_SOUTH_clean %>%
  filter(Departure_date > as.Date("2024-09-30"))


kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  filter(
    !(Departure_date > as.Date("2024-09-30"))
  )


## RENAME GENDER FROM MALE AND FEMALE TO MEN AND WOMEN 
kobo_SOUTH_clean <- kobo_SOUTH_clean %>%
  mutate(Gender = dplyr::recode(Gender,
                                "Male" = "Men",
                                "Female" = "Women"))


write.csv(kobo_SOUTH_clean, "Data/kobo_SOUTH_clean.csv", row.names = FALSE)




