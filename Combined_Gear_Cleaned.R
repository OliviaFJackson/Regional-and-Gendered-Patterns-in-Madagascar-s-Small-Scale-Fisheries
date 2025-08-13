library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(gt)

CPUE_South_All <- read.csv("Data/CPUE_South_All.csv", header = TRUE)
CPUE_North_All <- read.csv("Data/CPUE_North_All.csv", header = TRUE)



# Combine both CPUE_South_All and CPUE_North_All, 2 files where we calculated CPUE, and keep only rows that interest us for the analysis.
# Keep only rows where Did_you_collect = Yes


combined_CPUE <- bind_rows(
  CPUE_North_All %>%
    dplyr::select(
      Trip_ID, Region, Did_you_collect, Collector_name, Departure_date, Return_date, Departure_time, Return_time, Overnight, Town, Fishing_spot,
      Total_Crew, Total_Men, Total_Women, Government_Gazetted_Reserve, Season, Organisms_harvested, Fishing_effort_hours, Total_Catch_Kilos, CPUE_Kilos, 
      starts_with("CPUE_"),  
      starts_with("Gear_"),
      starts_with("Total_Avg_"),
      starts_with("Total_Med_"),
      starts_with("Avg_"),
      starts_with("Med_")
    ),
  
  CPUE_South_All %>%
    dplyr::select(
      Trip_ID, Region, Did_you_collect, Collector_name, Departure_date, Return_date, Departure_time, Return_time, Overnight, Town, Fishing_spot,
      Total_Crew, Total_Men, Total_Women, Government_Gazetted_Reserve, Season, Organisms_harvested, Fishing_effort_hours, Total_Catch_Kilos, CPUE_Kilos, 
      starts_with("CPUE_"),  
      starts_with("Gear_"),
      starts_with("Total_Avg_"),
      starts_with("Total_Med_"),
      starts_with("Avg_"),
      starts_with("Med_")      
    )
) %>%
  filter(Did_you_collect == "Yes")

write.csv(combined_CPUE, "Data/combined_CPUE.csv", row.names = FALSE)


combined_CPUE_full <- combined_CPUE


## CLEAN GEAR NAMES 
# Define the gear dictionary for corrections
gear_dict <- list(
  "Beach seine" = c("Beach Seine"),
  "Beach seine large" = c("Beach Seine"),
  "Beach seine small" = c("Beach Seine"),
  "Shark Net" = c("Shark Net"),
  "Spear/hand" = c("Spear/hand"),
  "Speargun" = c("Speargun"),
  "Speargun Spear/hand" = c("Speargun", "Spear/hand"),
  "Handline" = c("Handline"),
  "Longline" = c("Longline"),
  "Longline Handline" = c("Longline", "Handline"),
  "Longline Simple net" = c("Longline", "Simple net"),
  "Longline Spear/hand" = c("Longline", "Spear/hand"),
  "Simple net" = c("Simple net"),
  "Simple net Handline" = c("Simple net", "Handline"),
  "Simple net Shark net" = c("Simple net", "Shark net"),
  "Simple net Spear/hand" = c("Simple net", "Spear/hand"),
  "Simple net Spear/hand Speargun" = c("Simple net", "Spear/hand", "Speargun"),
  "Speargun Simple Net Handline" = c("Speargun", "Simple net", "Handline"),
  "Simple Net Handline" = c("Simple Net", "Handline"),
  "Spear/hand Handline" = c("Spear/hand", "Handline"),
  "Spear/hand Shark net" = c("Spear/hand", "Shark net"),
  "Spear/hand Simple net" = c("Spear/hand", "Simple net"),
  "Spear/hand Simple net Handline" = c("Spear/hand", "Simple net", "Handline"),
  "Speargun Handline" = c("Speargun", "Handline"),
  "Speargun Simple net" = c("Speargun", "Simple net"),
  "Speargun Simple net Handline" = c("Speargun", "Simple net", "Handline"),
  "ZZ net" = c("ZZ net"),
  "Gill net" = c("Gill net"),
  "ZZ net Shark net" = c("ZZ net", "Shark net"),
  "Handline Longline" = c("Handline", "Longline"),
  "Handline Simple net" = c("Handline", "Simple net"),
  "Handline Speargun" = c("Handline", "Speargun"),
  "handnet with handle Simple net" = c("Handnet with handle", "Simple net"),
  "Longline Speargun Simple net" = c("Longline", "Speargun", "Simple net"),
  "Simple net Speargun Handline" = c("Simple net", "Speargun", "Handline"),
  "Spear/hand handnet with handle" = c("Spear/hand", "Handnet with handle"),
  "handnet with handle" = c("Handnet with handle"),
  "dam-block when low tide goes out" = c("Dam block"),
  "dam-block when low tide goes out caught" = c("Dam block")
)


# Create a Gear Replacements Table 
gear_replacements <- unlist(gear_dict) %>% 
  tibble::enframe(name = "misspelled", value = "corrected") %>%
  distinct()  


# Select gear columns from dataset
gear_columns <- grep("^Gear_", names(combined_CPUE), value = TRUE)

# Convert wide-format gear data to long format
gear_long <- combined_CPUE %>%
  dplyr::select(Trip_ID, Region, Government_Gazetted_Reserve, Town, all_of(gear_columns)) %>%
  pivot_longer(cols = all_of(gear_columns), names_to = "Organism", values_to = "Gear_Type") %>%
  filter(Gear_Type != "" & !is.na(Gear_Type)) %>%  # Remove blank gear values
  mutate(Organism = str_replace(Organism, "Gear_", ""))  

gear_long %>%
  count(Trip_ID, Organism) %>%
  filter(n > 1)

# Function to clean gear names using the dictionary
clean_gear_names <- function(gear_data, gear_dict) {
  gear_data %>%
    rowwise() %>%
    mutate(Gear_Split = case_when(
      Gear_Type %in% names(gear_dict) ~ list(gear_dict[[Gear_Type]]),  # Standardize names
      TRUE ~ list(Gear_Type)  
    )) %>%
    unnest(Gear_Split)  # Split multi-gear configurations into separate rows
}

# Apply the function to clean gear names
gear_long_cleaned <- clean_gear_names(gear_long, gear_dict)

gear_long_cleaned <- gear_long_cleaned %>%
  mutate(
    Organism = case_when(
      Organism == "shell" ~ "Shells",
      Organism == "sea_cuc" ~ "Sea cucumber",
      Organism == "sea_horse" ~ "Sea horse",
      TRUE ~ str_to_title(Organism)
    )
  )

gear_long_cleaned <- gear_long_cleaned %>%
  mutate(
    Gear_Split = case_when(
      Gear_Split == "Spear/hand" ~ "Hand-held spear",
      TRUE ~ Gear_Split
    )
  )

# Convert CPUE data to long format

cpue_long <- combined_CPUE %>%
  pivot_longer(
    cols = matches("^CPUE_.*_kg$"),
    names_to = "Organism",
    values_to = "CPUE_Organism"
  ) %>%
  mutate(
    Organism = str_replace(Organism, "CPUE_", ""),
    Organism = str_replace(Organism, "_kg$", ""),
    Organism = case_when(
      Organism == "Sea_Cuc" ~ "Sea cucumber",
      Organism == "Sea_Horse" ~ "Sea horse",
      Organism == "Shell" ~ "Shells",
      TRUE ~ str_to_title(Organism)
    )
  )



# Run this to see the column names that match the pattern
grep("^CPUE_.*_kg$", names(combined_CPUE), value = TRUE)

unique(cpue_long$Organism)
unique(gear_long_cleaned$Organism)
summary(combined_CPUE$CPUE_Sea_Horse_kg)

setdiff(unique(cpue_long$Organism), unique(gear_long$Organism))
setdiff(unique(gear_long_cleaned$Organism), unique(cpue_long$Organism))



# Merge the CPUE data with the cleaned gear data
combined_CPUE_gear <- cpue_long %>%
  left_join(gear_long_cleaned, by = c("Trip_ID", "Region", "Government_Gazetted_Reserve", "Town", "Organism"))

# Ensuring we don't have any duplicated trips
cpue_long %>%
  count(Trip_ID, Region, Government_Gazetted_Reserve, Town, Organism) %>%
  filter(n > 1)

gear_long_cleaned %>%
  count(Trip_ID, Region, Government_Gazetted_Reserve, Town, Organism) %>%
  filter(n > 1)


# Check if gear and organisms match up properly
combined_CPUE_gear %>%
  filter(!is.na(Gear_Split)) %>%
  dplyr::select(Trip_ID, Organism, CPUE_Organism, Gear_Split)

sort(unique(cpue_long$Organism))
sort(unique(gear_long_cleaned$Organism))




## Adjust CPUE (Catch Per Unit Effort) values based on the number of different gear types used for each organism within each fishing trip


combined_CPUE_gear <- combined_CPUE_gear %>%
  filter(
    !is.na(Organism),
    !is.na(Gear_Split),
    !is.na(CPUE_Kilos)
  )


# Step 1: Count number of times each Gear_Split is used per Trip_ID and Region
gear_row_counts <- combined_CPUE_gear %>%
  group_by(Trip_ID, Region, Gear_Split) %>%
  summarise(rows_for_gear = n(), .groups = "drop")

# Step 2: Count number of unique gears per Trip_ID and Region
gear_counts <- combined_CPUE_gear %>%
  distinct(Trip_ID, Region, Gear_Split) %>%
  group_by(Trip_ID, Region) %>%
  summarise(num_gears = n(), .groups = "drop")

# Step 3: Get CPUE_Kilos per Trip_ID and Region
cpue_kilos_per_trip <- combined_CPUE_gear %>%
  group_by(Trip_ID, Region) %>%
  summarise(CPUE_Kilos_Trip = first(CPUE_Kilos), .groups = "drop")

# Step 4: Join all together
combined_CPUE_gear <- combined_CPUE_gear %>%
  left_join(gear_row_counts, by = c("Trip_ID", "Region", "Gear_Split")) %>%
  left_join(gear_counts, by = c("Trip_ID", "Region")) %>%
  left_join(cpue_kilos_per_trip, by = c("Trip_ID", "Region")) %>%
  mutate(CPUE_Gear_Adjusted = (CPUE_Kilos_Trip / num_gears) / rows_for_gear)

check_cpue <- combined_CPUE_gear %>%
  group_by(Trip_ID, Region) %>%
  summarise(
    sum_adjusted = sum(CPUE_Gear_Adjusted, na.rm = TRUE),
    original_cpue = first(CPUE_Kilos),
    match = abs(sum_adjusted - original_cpue) < 1e-6,
    .groups = "drop"
  )

summary(check_cpue$match)


unique(combined_CPUE_gear$Gear_Split)

# Grouping Gear Types into 4 main categories
combined_CPUE_gear <- combined_CPUE_gear %>%
  filter(!is.na(Gear_Split)) %>%  
  mutate(Gear_Split = case_when(
    Gear_Split %in% c("Beach Seine", "ZZ net", "Gill net", "Handnet with handle", "Shark net", "Simple net", "Dam block") ~ "Nets",
    Gear_Split %in% c("Longline", "Handline", "handline-thrown") ~ "Lines",
    TRUE ~ Gear_Split
  )) %>%
  group_by(Gear_Split) %>%
  ungroup()


# Check for weird Organism-Gear combinations 
bad_trips <- combined_CPUE_gear %>%
  filter(
    (Organism == "Crab" & Gear_Split %in% c("Lines")) |
      (Organism == "Shark" & Gear_Split == "Hand-held spear") |
      (Organism == "Sea cucumber" & Gear_Split == "Nets") 
  ) %>%
  distinct(Trip_ID, Gear_Split, Organism)

# Remove these weird combinations 
combined_CPUE_gear <- combined_CPUE_gear %>%
  filter(!Trip_ID %in% bad_trips$Trip_ID)


write.csv(combined_CPUE_gear, "Data/combined_CPUE_gear.csv", row.names = FALSE)


names(combined_CPUE_gear)
names(combined_CPUE_full)



# STEP 1: Pivot price columns BEFORE gear split
price_data_long <- combined_CPUE_full %>%
  select(Trip_ID, Region, Town,
         starts_with("Total_Avg_Price_"), starts_with("Total_Med_Price_"),
         starts_with("Avg_Price_"), starts_with("Med_Price_")) %>%
  pivot_longer(
    cols = -c(Trip_ID, Region, Town),
    names_to = "Price_Var",
    values_to = "Price_Value"
  ) %>%
  mutate(
    Price_Type = case_when(
      str_detect(Price_Var, "^Total_Avg_Price_") ~ "Total_Avg_Price",
      str_detect(Price_Var, "^Total_Med_Price_") ~ "Total_Med_Price",
      str_detect(Price_Var, "^Avg_Price_.*_Per_kg") ~ "Avg_Price_Per_kg",
      str_detect(Price_Var, "^Med_Price_.*_Per_kg") ~ "Med_Price_Per_kg",
      TRUE ~ NA_character_
    ),
    Organism = Price_Var %>%
      str_remove_all("^Total_Avg_Price_|^Total_Med_Price_|^Avg_Price_|^Med_Price_") %>%
      str_remove("_Per_kg$") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  ) %>%
  mutate(
    Organism = case_when(
      Organism == "Sea Cuc"    ~ "Sea cucumber",
      Organism == "Sea Horse"  ~ "Sea horse",
      Organism == "Shell"      ~ "Shells",
      Organism == "Shrimo"     ~ "Shrimp",  # Just in case
      TRUE                     ~ Organism
    )
  ) %>%
  select(-Price_Var)

# STEP 2: Average duplicated entries and pivot wide
price_cleaned <- price_data_long %>%
  group_by(Trip_ID, Region, Town, Organism, Price_Type) %>%
  summarise(Price_Value = mean(Price_Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Price_Type, values_from = Price_Value)

# STEP 3: Count how many rows per Trip × Organism (due to gear splitting)
organism_row_counts <- combined_CPUE_gear %>%
  group_by(Trip_ID, Organism) %>%
  summarise(n_rows = n(), .groups = "drop")


# STEP 1: Identify wide-format columns to remove
price_cols_to_remove <- names(combined_CPUE_gear) %>%
  stringr::str_subset("^(Avg|Med|Total)_(Price|Avg_Price|Med_Price).*") %>%
  setdiff(c("Avg_Price_Per_kg", "Med_Price_Per_kg", "Total_Avg_Price", "Total_Med_Price"))

gear_cols_to_remove <- names(combined_CPUE_gear) %>%
  stringr::str_subset("^Gear_") %>%
  setdiff(c("Gear_Type", "Gear_Split"))

# STEP 2: Create cleaned master dataset
Masterdata_CPUE_North_South_2023_2024 <- combined_CPUE_gear %>%
  select(-any_of(c(price_cols_to_remove, gear_cols_to_remove))) %>%
  left_join(price_cleaned, by = c("Trip_ID", "Region", "Town", "Organism")) %>%
  left_join(organism_row_counts, by = c("Trip_ID", "Organism")) %>%
  mutate(across(
    c(Total_Avg_Price, Total_Med_Price, Avg_Price_Per_kg, Med_Price_Per_kg),
    ~ . / n_rows
  )) %>%
  select(-n_rows)




# Check for unmatched joins
anti_join(combined_CPUE_gear, price_cleaned, by = c("Trip_ID", "Region", "Town", "Organism")) %>%
  count(Region)

anti_join(combined_CPUE_gear, price_cleaned, by = c("Trip_ID", "Region", "Town", "Organism")) %>%
  distinct(Organism) %>%
  arrange(Organism)

distinct(price_cleaned, Organism) %>%
  arrange(Organism)

nrow(combined_CPUE_gear) == nrow(Masterdata_CPUE_North_South_2023_2024)

combined_CPUE_gear %>% distinct(Trip_ID) %>% count()
Masterdata_CPUE_North_South_2023_2024 %>% distinct(Trip_ID) %>% count()


# Recompute n_rows per Trip_ID (not just per organism)
trip_row_counts <- Masterdata_CPUE_North_South_2023_2024 %>%
  count(Trip_ID, name = "n_rows_trip")

# Join and adjust CPUE_Kilos_Trip
Masterdata_CPUE_North_South_2023_2024 <- Masterdata_CPUE_North_South_2023_2024 %>%
  left_join(trip_row_counts, by = "Trip_ID") %>%
  mutate(CPUE_Kilos_Trip = CPUE_Kilos / n_rows_trip) %>%
  select(-n_rows_trip)

# CHeck that CPUE_Kilos_Trip sum matches CPUE_Kilos
Masterdata_CPUE_North_South_2023_2024 %>%
  group_by(Trip_ID) %>%
  summarise(
    sum_adjusted_CPUE = sum(CPUE_Kilos_Trip, na.rm = TRUE),
    original_CPUE = first(CPUE_Kilos),
    match = abs(sum_adjusted_CPUE - original_CPUE) < 1e-6
  ) %>%
  summarise(all_match = all(match))





# Reorder columns for clean Masterdata
Masterdata_CPUE_North_South_2023_2024 <- Masterdata_CPUE_North_South_2023_2024 %>%
  select(
    Trip_ID, Region, Town, Did_you_collect, Collector_name, 
    Departure_date, Return_date, Departure_time, Return_time, Overnight, Fishing_effort_hours, Fishing_spot, 
    Total_Crew, Total_Men, Total_Women, Organisms_harvested, Total_Catch_Kilos, CPUE_Kilos, CPUE_Kilos_Trip,
    Organism, CPUE_Organism, Gear_Type, Gear_Split, 
    CPUE_Gear_Adjusted, 
    Avg_Price_Per_kg, Med_Price_Per_kg, 
    Total_Avg_Price, Total_Med_Price,
    everything()
  )

Masterdata_CPUE_North_South_2023_2024 <- Masterdata_CPUE_North_South_2023_2024 %>%
  select(-CPUE_Kilos)


write.csv(Masterdata_CPUE_North_South_2023_2024, "Data/Masterdata_CPUE_North_South_2023_2024.csv", row.names = FALSE)
write_xlsx(Masterdata_CPUE_North_South_2023_2024, "Data/Masterdata_CPUE_North_South_2023_2024.xlsx")




Masterdata_CPUE_North_South_2023_2024 %>%
  filter(!is.na(Avg_Price_Per_kg)) %>%
  group_by(Organism) %>%
  summarise(
    n = n(),
    Min = min(Avg_Price_Per_kg, na.rm = TRUE),
    Q1 = quantile(Avg_Price_Per_kg, 0.25, na.rm = TRUE),
    Median = median(Avg_Price_Per_kg, na.rm = TRUE),
    Q3 = quantile(Avg_Price_Per_kg, 0.75, na.rm = TRUE),
    Max = max(Avg_Price_Per_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Organism) %>%
  gt() %>%
  tab_header(
    title = "Summary of Avg_Price_Per_kg by Organism"
  )




Masterdata_CPUE_North_South_2023_2024 %>%
  filter(between(Avg_Price_Per_kg, 100, 30000)) %>%
  ggplot(aes(x = Organism, y = Avg_Price_Per_kg)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Filtered Price per Kilogram by Organism (100–30,000 Ar)", y = "Avg Price (Ar/kg)")




Masterdata_CPUE_North_South_2023_2024 <- Masterdata_CPUE_North_South_2023_2024 %>%
  mutate(
    Single_Gender_Trips = case_when(
      Total_Men > 0 & Total_Women == 0 ~ "Men",
      Total_Women > 0 & Total_Men == 0 ~ "Women",
      TRUE ~ NA_character_  # mixed-gender or unclear trips
    )
  )


Masterdata_CPUE_North_South_2023_2024 %>%
  filter(
    !is.na(Med_Price_Per_kg),
    Avg_Price_Per_kg > 200,
    !is.na(Single_Gender_Trips),
    !Organism %in% c("Urchin", "Shark", "Eel")
  ) %>%
  ggplot(aes(x = Organism, y = Avg_Price_Per_kg, fill = Single_Gender_Trips)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  scale_y_log10() +
  facet_wrap(~ Region) +
  theme_minimal() +
  labs(
    title = "Price per Kilogram by Organism, Gender, and Region (Filtered)",
    y = "Avg Price (Ar/kg)",
    x = "Organism"
  ) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.spacing = unit(1, "lines"),
    plot.caption = element_text(size = 12, hjust = 0, margin = margin(t = 12)),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("Men" = "#1f77b4", "Women" = "#ff7f0e"))



sample_sizes <- Masterdata_CPUE_North_South_2023_2024 %>%
  # Filter consistent with your plot
  filter(
    !is.na(Avg_Price_Per_kg),
    Avg_Price_Per_kg > 100,
    !is.na(Total_Men), !is.na(Total_Women),
    !Organism %in% c("Urchin", "Shark", "Eel")
  ) %>%
  mutate(
    Gender = case_when(
      Total_Men > 0 & Total_Women == 0 ~ "Men",
      Total_Women > 0 & Total_Men == 0 ~ "Women",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Gender)) %>%
  group_by(Region, Gender, Organism) %>%
  summarise(N = n(), .groups = "drop") %>%
  arrange(Region, Organism, Gender)

print(sample_sizes)


# Define ordered labels
price_labels <- c("<50", "50–100", "100–200", "200–500", "500–1000", "1K–5K", "5K–10K", "10K–50K", ">50K")

# Create summary with correct bin order
price_bin_summary <- Masterdata_CPUE_North_South_2023_2024 %>%
  filter(!is.na(Avg_Price_Per_kg)) %>%
  mutate(
    Price_Bin = cut(
      Avg_Price_Per_kg,
      breaks = c(-Inf, 50, 100, 200, 500, 1000, 5000, 10000, 50000, Inf),
      labels = price_labels,
      right = FALSE
    )
  ) %>%
  group_by(Region, Single_Gender_Trips, Organism, Price_Bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Price_Bin = factor(Price_Bin, levels = price_labels)) %>%
  pivot_wider(
    names_from = Price_Bin,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(Region, Single_Gender_Trips, Organism) %>%
  select(Region, Single_Gender_Trips, Organism, all_of(price_labels))  # force column order

# View result
price_bin_summary












# Total unique trips across both datasets
total_unique_trips_all <- combined_CPUE %>%
  distinct(Trip_ID) %>%
  nrow()

# Unique trips with < 1 hour effort across both datasets
short_effort_trips_all <- combined_CPUE %>%
  filter(Fishing_effort_hours < 1) %>%
  distinct(Trip_ID) %>%
  nrow()

# Combined percentage
pct_short_trips_all <- round(
  (short_effort_trips_all / total_unique_trips_all) * 100, 2
)

pct_short_trips_all
