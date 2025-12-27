# ============
# NOAA Storm Database Analysis - Project Setup
# ============
# Purpose: Analyse storm events for population health and economic impact
# Author: DrNWM with help of Claude AI
# Date: 2025-12-26
# =============

# 1. INITIAL SETUP
getwd()

# Install required package if not already installed
required_packages <-  c("here", "usethis", "renv", "tidyverse", 
                        "data.table", "lubridate", "scales",
                        "R.utils","knitr","ggplot2")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages) > 0){
        install.packages(new_packages)
}   

# Now we can load libraries
library(here)
library(tidyverse)
library(data.table)
library(lubridate)
library(scales)
library(R.utils)

# 2. CREATE PROJECT STRUCTURE ---
#create directory structure
dir.create(here("data"), showWarnings = FALSE)
dir.create(here("data","raw"), showWarnings = FALSE)
dir.create(here("data","processed"), showWarnings = FALSE)
dir.create(here("figures"), showWarnings = FALSE)
dir.create(here("figures","exploratory"), showWarnings = FALSE)
dir.create(here("figures","final"), showWarnings = FALSE)
dir.create(here("scripts"), showWarnings = FALSE)
dir.create(here("reports"), showWarnings = FALSE)

cat("Project structure created successfully!\n")
list.dirs(here(),recursive = FALSE)

# 3. DOWNLOAD RAW DATA ----
data_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
raw_data_file <- here("data","raw","StormData.csv.bz2")

if(!file.exists(raw_data_file)) {
        cat ("Downloading storm data...\n")
        download.file(data_url,raw_data_file, method = "auto")
        cat("Download complete!\n")
}else {
        cat ("Raw data file already exists.\n")
}

# Record download date
download.info <- data.frame(
        file = "StormData.csv.bz2",
        url = data_url, 
        download_date = Sys.time()
)
write.csv(download.info,here("data","raw","download_info.csv"),row.names=FALSE)

# 4. LOAD AND INITIAL EXPLORATION
cat("Reading storm data (this may take a moment)...\n")
storm_data <- fread(raw_data_file)

# Basic exploration
cat("\nDataset dimensions:", dim(storm_data),"\n")
cat("\nColumn names:\n")
print(names(storm_data))
cat("\nFirst few rows\n")
print(head(storm_data,3))
cat("\nData structure:\n")
str(storm_data)

# 5. DATA PROCESSING
cat("\nProcessing data...\n")

# Select relevant columsn for analysis
storm_clean <- storm_data |>
        select(BGN_DATE,STATE,EVTYPE,FATALITIES,INJURIES,
               PROPDMG, PROPDMGEXP,CROPDMG,CROPDMGEXP) |>
        mutate(
                BGN_DATE = mdy_hms(BGN_DATE),
                YEAR = year(BGN_DATE)
        )

# function to convert damage exponents to numeric multipliers
convert_exponent <- function(exp) {
        case_when(
                toupper(exp) == "K" ~ 1e3,
                toupper(exp) == "M" ~ 1e6,
                toupper(exp) == "B" ~ 1e9,
                toupper(exp) %in% c("H", "2") ~ 1e2,
                toupper(exp) %in% as.character(0:9) ~ 10 ^ as.numeric(toupper(exp)),  # include 9 just in case
                # Explicitly catch common bad values and treat as 1 (no multiplier)
                exp %in% c("", " ", "+", "-", "?", "0") ~ 1,
                TRUE ~ 1  # anything else = 1 (safe default)
        )
}
# Calculate actual damage values
storm_clean <- storm_clean |>
        mutate(
                PROP_MULTIPLIER = convert_exponent(PROPDMGEXP),
                CROP_MULTIPLIER = convert_exponent(CROPDMGEXP),
                PROPERTY_DAMAGE = PROPDMG * PROP_MULTIPLIER,
                CROP_DAMAGE = CROPDMG * CROP_MULTIPLIER,
                TOTAL_DAMAGE = PROPERTY_DAMAGE + CROP_DAMAGE,
                TOTAL_CASUALTIES = FATALITIES + INJURIES
        )
# Sort event types first 
storm_clean |> count(EVTYPE, sort = TRUE) 

# Clean event types (consolidate similar events)
storm_clean <- storm_clean |>
        mutate(
                EVTYPE_CLEAN = str_to_upper (str_trim(EVTYPE)),
                EVTYPE_CLEAN = case_when(
                        str_detect(EVTYPE_CLEAN,"TORNADO") ~"TORNADO",
                        str_detect(EVTYPE_CLEAN,"FLOOD|FLD") ~"FLOOD",
                        str_detect(EVTYPE_CLEAN,"HURRICANE|TYPHOON") ~"HURRICANE",
                        str_detect(EVTYPE_CLEAN,"HEAT|WARM") ~"EXCESSIVE HEAT",
                        str_detect(EVTYPE_CLEAN,"LIGHTNING") ~"LIGHTNING",
                        str_detect(EVTYPE_CLEAN,"WIND")  ~"HIGH WIND",
                        str_detect(EVTYPE_CLEAN,"WINTER|SNOW|ICE|BLIZZARD") ~"WINTER STORM",
                        str_detect(EVTYPE_CLEAN,"HAIL") ~"HAIL",
                        str_detect(EVTYPE_CLEAN,"THUNDERSTORM|TSTM") ~"THUNDERSTORM WIND",
                        str_detect(EVTYPE_CLEAN,"FIRE") ~"WILDFIRE",
                        str_detect(EVTYPE_CLEAN,"DROUGHT") ~"DROUGHT",
                        TRUE ~ EVTYPE_CLEAN
                )
        )

# Save processed data
processed_file <-  here("data","processed","storm_data_processed.rds")
saveRDS(storm_clean,processed_file)
cat("\nProcessed data saved to:", processed_file,"\n")

# 6. ANALYSIS: POPULATION HEALTH IMPACT---
cat("\nAnalyzing population health impact\n")

health_impact <-  storm_clean |>
        group_by(EVTYPE_CLEAN) |>
        summarise(
                Total_Fatalities = sum (FATALITIES,na.rm = TRUE), 
                Total_Injuries = sum(INJURIES,na.rm = TRUE), 
                Total_Casualties = sum(TOTAL_CASUALTIES, na.rm = TRUE), 
                Events = n()
        ) |>
        filter(Total_Casualties >0 ) |>
        arrange(desc(Total_Casualties)) |>
        slice_head(n=15)

print(health_impact)

# 7. ANALYSIS: ECONOMIC IMPACT ---
cat("\nAnalyzing economic impact\n")

economic_impact <- storm_clean |>
        group_by(EVTYPE_CLEAN) |>
        summarize(
                Total_Property_Damage = sum(PROPERTY_DAMAGE, na.rm =TRUE),
                Total_Crop_Damage = sum(CROP_DAMAGE, na.rm = TRUE),
                Total_Economic_Damage = sum(TOTAL_DAMAGE, na.rm=TRUE), 
                Events = n()
        ) |>
        filter(Total_Economic_Damage > 0) |>
        arrange(desc(Total_Economic_Damage)) |>
        slice_head(n=15)

print(economic_impact)

# 8. CREATE VISUALIZATION --
cat("\nCreating Visualizations...\n")

# Plot 1: Health Impact
p1 <- health_impact |>
        pivot_longer(
                cols = c(Total_Fatalities, Total_Injuries),
                names_to = "Type",
                values_to = "Count"
        ) |>
        mutate(Type = str_remove(Type, "Total_")) |>
        # Add this to get top 15 (by total Count across both types)
        slice_max(order_by = Count, n = 15) |>
        ggplot(aes(x = reorder(EVTYPE_CLEAN, Count), y = Count, fill = Type)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = comma) +
        scale_fill_manual(values = c("Fatalities" = "#d73027", "Injuries" = "#fee090")) +
        labs(
                title = "Top 15 Weather Events by Population Health Impact",
                subtitle = "Total fatalities and injuries across the United States",
                x = "Event Type",
                y = "Number of People Affected",
                fill = "Impact type"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", size = 14),
                legend.position = "bottom"
        )

ggsave(here("figures", "final", "health_impact.png"), p1,
       width = 10, height = 8, dpi = 300)

# Plot 2: Economic Impact

p2 <- economic_impact |>
        pivot_longer(
                cols = c(Total_Property_Damage, Total_Crop_Damage),
                names_to = "Type",
                values_to = "Damage"
        ) |>
        mutate(
                Type = str_remove(Type,"Total_"),
                Type = str_remove(Type,"_Damage"),
                Damage_Billions = Damage/1e9
        ) |>
        slice_max(order_by = Damage_Billions, n=15) |>
        ggplot(aes(
                x = reorder(EVTYPE_CLEAN, Damage_Billions),
                y = Damage_Billions, fill = Type
        )) +
        geom_col(position = "stack") +
        coord_flip() +
        scale_y_continuous(labels = dollar) +
        scale_fill_manual(
                values = c("Property" = "#4575b4", "Crop" = "#91cf60"),
                drop = FALSE            
        ) +
        labs(
                title = "Top 15 Weather Events by Economic Impact",
                subtitle = "total property and crop damage across United State",
                x = 'Event Type',
                y = "Economic Damage (Billions USD)",
                fill = "Damage Type"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", size = 14),
                legend.position = "bottom"
        )

ggsave(here("figures","final", "economic_impact.png"), p2,
       width = 10, height = 8, dpi = 300)


cat("\nVisualizations saved to figures/final\n")

# 9. SUMMARY STATISTICS ---
summary_stats <- list(
        health = health_impact,
        economic = economic_impact,
        overall = storm_clean|>
                summarize(
                        Total_Events = n(), 
                        Date_Range = paste(min (BGN_DATE, na.rm = TRUE), "to", 
                                           max (BGN_DATE, na.rm = TRUE)), 
                        Total_Fatalities = sum(FATALITIES,na.rm = TRUE), 
                        Total_Injuries = sum(INJURIES,na.rm = TRUE), 
                        Total_Economic_Damage_Billions = sum(TOTAL_DAMAGE, na.rm = TRUE)/1e9
                )
        )

saveRDS(summary_stats, here("data", "processed", "summary_statistic.rds"))

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("\nSummary Statistics\n")
print(summary_stats$overall)

cat("\nTop 5 Most Harmful Events\n")
print(head(health_impact,5))

cat("\nTop 5 Most Costly Events\n")
print(head(economic_impact,5))


