# OSM Deer Modelling
# Nov 27, 2019

getwd()

install.packages("faraway")
install.packages("MuMIn")
install.packages("devtools")
devtools::install_github("rvalavi/blockCV")
install.packages("shinyjs")
install.packages("tmaptools")
install.packages("precrec")
install.packages("tidyverse")
install.packages("MASS")

library(tidyverse)
library(lme4)
library(lattice)
library(MuMIn)
library(AICcmodavg)
library(faraway)
library(knitr)
library(AICcmodavg)
library(DT)
library(boot)
library(dotwhisker)
library(stargazer)
library(cowplot)
library(broom)
library(sf)
library(tmap)
library(raster)
library(shinyjs)
library(tmaptools)
library(precrec)
library(mapview)
library(visreg)
library(MASS)
library(stars)

test <- st_read("realdeer_locations.shp")
test$WMU_Code %>% unique()

test %>% filter(WMU_Code == 0)


# Work with script: "OSM Deer HF Modelling" for distance-based HF modelling
# Add: area-based HF modelling and area-based natural-features modelling


##### SECTION 1: NRCAN Data and Distance Classifications ----------

# Load Data
deer_NRCAN <- as_tibble(read_csv("OSM Deer Docs/realdeer_NRCAN_lc_summary.csv"))
random_NRCAN <- as_tibble(read_csv("OSM Deer Docs/random_deer_NRCAN_lc_summary.csv"))

unique(deer_NRCAN$gridcode)

class(deer_NRCAN$gridcode)
deer_NRCAN$gridcode <- as.character(deer_NRCAN$gridcode)

### Real Deer -----
## Convert numeric classes to words
deer_NRCAN[deer_NRCAN$gridcode == 1, 3] <- "Barren/Snow-Ice"
deer_NRCAN[deer_NRCAN$gridcode == 2, 3] <- "Urban"
deer_NRCAN[deer_NRCAN$gridcode == 3, 3] <- "Lichen, Herb, Shrub"
deer_NRCAN[deer_NRCAN$gridcode == 4, 3] <- "Grassland"
deer_NRCAN[deer_NRCAN$gridcode == 5, 3] <- "Cultivated Areas - Low Biomass"
deer_NRCAN[deer_NRCAN$gridcode == 6, 3] <- "Cultivated Areas - Medium Biomass"
deer_NRCAN[deer_NRCAN$gridcode == 7, 3] <- "Cultivated Areas - High Biomass"
deer_NRCAN[deer_NRCAN$gridcode == 8, 3] <- "Herb-Shrub, Low Vegetetaion Cover"
deer_NRCAN[deer_NRCAN$gridcode == 9, 3] <- "Shrubland"
deer_NRCAN[deer_NRCAN$gridcode == 10, 3] <- "Wetland - Herb-Shrub, Treed"
deer_NRCAN[deer_NRCAN$gridcode == 11, 3] <- "Broadleaf - low to medium density, young regeneration"
deer_NRCAN[deer_NRCAN$gridcode == 12, 3] <- "Broadleaf - medium to high density"
deer_NRCAN[deer_NRCAN$gridcode == 13, 3] <-  "Mixedwood - low to medium density, young regeneration"
deer_NRCAN[deer_NRCAN$gridcode == 14, 3] <-  "Mixedwood - medium to high density"
deer_NRCAN[deer_NRCAN$gridcode == 15, 3] <- "Conifer - low density"
deer_NRCAN[deer_NRCAN$gridcode == 16, 3] <-  "Conifer - medium to high density"
deer_NRCAN[deer_NRCAN$gridcode == 17, 3] <- "Water"
  
unique(deer_NRCAN$gridcode)  
     
## Rename columns:
# Rename OBJECTID to "Point"
deer_NRCAN <- deer_NRCAN %>% rename(Point = OBJECTID_1)

# Rename gridcode to lc_class
deer_NRCAN <- deer_NRCAN %>% rename(lc_class = gridcode)

## Pivot table --
pivot_deer_NRCAN_table <- pivot_wider(deer_NRCAN, id_cols = Point, names_from = lc_class, values_from = AREA)

# Switch NA to 0
pivot_deer_NRCAN_table <- pivot_deer_NRCAN_table %>% mutate_if(is.numeric , replace_na, replace = 0)

# Add "Use"
pivot_deer_NRCAN_table$Use = 1

# Check table:
names(pivot_deer_NRCAN_table)


### Random Deer -----
class(random_NRCAN$gridcode)
random_NRCAN$gridcode <- as.character(random_NRCAN$gridcode)

## Convert numeric classes to words
random_NRCAN[random_NRCAN$gridcode == 1, 3] <- "Barren/Snow-Ice"
random_NRCAN[random_NRCAN$gridcode == 2, 3] <- "Urban"
random_NRCAN[random_NRCAN$gridcode == 3, 3] <- "Lichen, Herb, Shrub"
random_NRCAN[random_NRCAN$gridcode == 4, 3] <- "Grassland"
random_NRCAN[random_NRCAN$gridcode == 5, 3] <- "Cultivated Areas - Low Biomass"
random_NRCAN[random_NRCAN$gridcode == 6, 3] <- "Cultivated Areas - Medium Biomass"
random_NRCAN[random_NRCAN$gridcode == 7, 3] <- "Cultivated Areas - High Biomass"
random_NRCAN[random_NRCAN$gridcode == 8, 3] <- "Herb-Shrub, Low Vegetetaion Cover"
random_NRCAN[random_NRCAN$gridcode == 9, 3] <- "Shrubland"
random_NRCAN[random_NRCAN$gridcode == 10, 3] <- "Wetland - Herb-Shrub, Treed"
random_NRCAN[random_NRCAN$gridcode == 11, 3] <- "Broadleaf - low to medium density, young regeneration"
random_NRCAN[random_NRCAN$gridcode == 12, 3] <- "Broadleaf - medium to high density"
random_NRCAN[random_NRCAN$gridcode == 13, 3] <-  "Mixedwood - low to medium density, young regeneration"
random_NRCAN[random_NRCAN$gridcode == 14, 3] <-  "Mixedwood - medium to high density"
random_NRCAN[random_NRCAN$gridcode == 15, 3] <- "Conifer - low density"
random_NRCAN[random_NRCAN$gridcode == 16, 3] <-  "Conifer - medium to high density"
random_NRCAN[random_NRCAN$gridcode == 17, 3] <- "Water"

unique(random_NRCAN$gridcode)  

## Rename columns:
# Rename OBJECTID to "Point"
random_NRCAN <- random_NRCAN %>% rename(Point = OBJECTID_1)

# Rename gridcode to lc_class
random_NRCAN <- random_NRCAN %>% rename(lc_class = gridcode)

## Pivot table --
pivot_random_NRCAN_table <- pivot_wider(random_NRCAN, id_cols = Point, names_from = lc_class, values_from = AREA)

# Switch NA to 0
pivot_random_NRCAN_table <- pivot_random_NRCAN_table %>% mutate_if(is.numeric , replace_na, replace = 0)

# Add "Use"
pivot_random_NRCAN_table$Use = 0

# Check table:
names(pivot_random_NRCAN_table)


##### Merge NRCAN and Distance tables  ----
### Real Deer:
# Read deer distance_table 
glimpse(deer_distancetable)

# Merge Datasets  
deer_NRCAN_merged <- merge(pivot_deer_NRCAN_table, deer_distancetable, by = c("Point", "Use"))
View(deer_NRCAN_merged)
names(deer_NRCAN_merged)

### Random Deer:
# random_distancetable <- read_csv("RandomDeerOutside500.csv")
glimpse(random_distancetable)

# Merge Random Datasets 
random_NRCAN_merged <- merge(pivot_random_NRCAN_table, random_distancetable, by = c("Point", "Use"))
View(random_NRCAN_merged)
names(random_NRCAN_merged)

# Remove excess columns - "WMU" - from random table
random_NRCAN_merged <- random_NRCAN_merged[, -19]


##### Merge Real and Random Tables -----
names(deer_NRCAN_merged)
names(random_NRCAN_merged)

# Rename Variables
random_NRCAN_merged <- random_NRCAN_merged %>% rename("Cutblocks Distance (m)" = "ClearCut Distance (m)")
random_NRCAN_merged <- random_NRCAN_merged %>% rename("Transmission Line Distance (m)" = "Transmission_Line Distance (m)")
random_NRCAN_merged <- random_NRCAN_merged %>% rename("Pipeline Distance (m)" = "Pipelines Distance (m)")
deer_NRCAN_merged <- deer_NRCAN_merged %>% rename("Transmission Line Distance (m)" = "Transmission Distance (m)")

# Remove "Barren/Snow-Ice" from Random dataset - no real deer locations have barren/snow area
random_NRCAN_merged <- random_NRCAN_merged[, -17]

identical(ls(deer_NRCAN_merged), ls(random_NRCAN_merged))

### Merge Real and Random Locations
alldeer_NRCAN <- rbind(deer_NRCAN_merged, random_NRCAN_merged)
View(alldeer_NRCAN)

# Save backup files:
# Has correct number of rows for Random Locations - 12256!
#   write_csv(random_NRCAN_merged, "random_NRCAN_plus_Distance.csv")
#   write_csv(deer_NRCAN_merged, "deer_NRCAN_plus_Distance.csv")
#   write_csv(alldeer_NRCAN, "alldeer_NRCAN_plus_Distance.csv")
nrow(alldeer_NRCAN)



#### SECTION 2:  Obtain land-cover areas within buffer polygons -------------
# For features: water, wellsites, cultivation, cutblocks

# Arc "Tabulate Intersection" creates a table with values only where the feature has an area. Missing rows for buffers that have 0 area of the feature specified. 
# Test will be merging these to generate table with 0s where no area was observed. 

# Generate data like the data I will be merging 
df_points = tibble(point = c(1:100))   # All points 
df2_values = tibble(point = seq(1, 100, by = 5), value = c(runif(20, 1, 100)))  # values for area for certain points

# Merge the points and zeros data 
merge_test <- merge(df_points, df2_values, all = TRUE)
merge_test[is.na(merge_test)] <- 0
          

# Remove excess rows from pivot_random_table --
# {  remove_test <- c(12256:12350)  }
# {  pivot_random_table <- pivot_random_table[- c(12256:12350), ]  }
# {  range(pivot_random_table$Point)  }

### Load my data 
getwd()

# Generate Table 1's with real and random objectids
range(pivot_deer_table$Point)  #1-2470
range(pivot_random_table$Point) #1-12256
# Need 1:12256

## Generate Real and Random Points 
df_real_points <- tibble(point = pivot_deer_table$Point)
range(df_real_points$point)
df_random_points <- tibble(point = pivot_random_table$Point)
range(df_random_points)

##### Feature layers - loading "tabulte intersection" table from real and random points, with feature layers 
## Waterbodies 
# Load Data
real_deer_waterbodies_sheet <- read_csv("OSM Deer Docs/realdeer_waterbodies_area.csv")
random_deer_waterbodies_sheet <- read_csv("OSM Deer Docs/randomdeer_waterbodies_area.csv")

# Load "Waterbodies" dataframes  
real_deer_waterbodies <- tibble(point = real_deer_waterbodies_sheet$OBJECTID_1, area = real_deer_waterbodies_sheet$AREA)
random_deer_waterbodies <- tibble(point = random_deer_waterbodies_sheet$OBJECTID_1, area = random_deer_waterbodies_sheet$AREA)

# Merge points and areas 
realdeer_waterbodies_summary <- merge(df_real_points, real_deer_waterbodies, all = TRUE)
realdeer_waterbodies_summary[is.na(realdeer_waterbodies_summary)] <- 0
range(realdeer_waterbodies_summary$point)

randomdeer_waterbodies_summary <- merge(df_random_points, random_deer_waterbodies, all = TRUE)
randomdeer_waterbodies_summary[is.na(randomdeer_waterbodies_summary)] <- 0
range(randomdeer_waterbodies_summary$point)

###### Note: Random Deer Distance Table only has Points up to 12256. Need to *delete* points above 12256 in Arc. Delete points above. **LATER**. 
randomdeer_waterbodies_summary <- randomdeer_waterbodies_summary[- c(12256:12350), ]
range(randomdeer_waterbodies_summary$point)


## Wellsites Active
# Load Data and Create Dataframes 
csv_realdeer_wellsites_active <- read_csv("OSM Deer Docs/realdeer_wellsites_active_area.csv")
csv_randomdeer_wellsites_active <- read_csv("OSM Deer Docs/randomdeer_wellsites_active_area.csv")

df_realdeer_wellsites_active <- tibble(point = csv_realdeer_wellsites_active$OBJECTID_1, area = csv_realdeer_wellsites_active$AREA)
df_randomdeer_wellsites_active <- tibble(point = csv_randomdeer_wellsites_active$OBJECTID_1, area = csv_randomdeer_wellsites_active$AREA)

# Merge points and areas
realdeer_wellsites_active_summary <- merge(df_real_points, df_realdeer_wellsites_active, all = TRUE)
realdeer_wellsites_active_summary[is.na(realdeer_wellsites_active_summary)] <- 0
range(realdeer_wellsites_active_summary$point)

randomdeer_wellsites_active_summary <- merge(df_random_points, df_randomdeer_wellsites_active, all = TRUE)
randomdeer_wellsites_active_summary[is.na(randomdeer_wellsites_active_summary)] <- 0


### Wellsites Abandoned 
csv_realdeer_wellsites_abandoned <- read_csv("OSM Deer Docs/realdeer_wellsites_abandoned_area.csv")
csv_randomdeer_wellsites_abandoned <- read_csv("OSM Deer Docs/randomdeer_wellsites_abandoned_area.csv")

df_realdeer_wellsites_abandoned <- tibble(point = csv_realdeer_wellsites_abandoned$OBJECTID_1, area = csv_realdeer_wellsites_abandoned$AREA)
df_randomdeer_wellsites_abandoned <- tibble(point = csv_randomdeer_wellsites_abandoned$OBJECTID_1, area = csv_randomdeer_wellsites_abandoned$AREA)

realdeer_wellsites_abandoned_summary <- merge(df_real_points, df_realdeer_wellsites_abandoned, all = TRUE)
realdeer_wellsites_abandoned_summary[is.na(realdeer_wellsites_abandoned_summary)] <- 0
range(realdeer_wellsites_abandoned_summary$point)

randomdeer_wellsites_abandoned_summary <- merge(df_random_points, df_randomdeer_wellsites_abandoned, all = TRUE)
randomdeer_wellsites_abandoned_summary[is.na(randomdeer_wellsites_abandoned_summary)] <- 0


### Cultivation
csv_realdeer_cultivation <- read_csv("OSM Deer Docs/realdeer_cultivation_area.csv")
csv_randomdeer_cultivation <- read_csv("OSM Deer Docs/randomdeer_cultivation_area.csv")

df_realdeer_cultivation <- tibble(point = csv_realdeer_cultivation$OBJECTID_1, area = csv_realdeer_cultivation$AREA)
df_randomdeer_cultivation <- tibble(point = csv_randomdeer_cultivation$OBJECTID_1, area = csv_randomdeer_cultivation$AREA)

realdeer_cultivation_summary <- merge(df_real_points, df_realdeer_cultivation, all = TRUE)
realdeer_cultivation_summary[is.na(realdeer_cultivation_summary)] <- 0
range(realdeer_cultivation_summary$point)

randomdeer_cultivation_summary <- merge(df_random_points, df_randomdeer_cultivation, all = TRUE)
randomdeer_cultivation_summary[is.na(randomdeer_cultivation_summary)] <- 0


### Cutblocks before 1997
csv_realdeer_cutblocksbefore1997 <- read_csv("OSM Deer Docs/realdeer_cutblocksbefore1997_area.csv")
csv_randomdeer_cutblocksbefore1997 <- read_csv("OSM Deer Docs/randomdeer_cutblocksbefore1997_area.csv")

df_realdeer_cutblocksbefore1997 <- tibble(point = csv_realdeer_cutblocksbefore1997$OBJECTID_1, area = csv_realdeer_cutblocksbefore1997$AREA)
df_randomdeer_cutblocksbefore1997 <- tibble(point = csv_randomdeer_cutblocksbefore1997$OBJECTID_1, area = csv_randomdeer_cutblocksbefore1997$AREA)

realdeer_cutblocksbefore1997_summary <- merge(df_real_points, df_realdeer_cutblocksbefore1997, all = TRUE)
realdeer_cutblocksbefore1997_summary[is.na(realdeer_cutblocksbefore1997_summary)] <- 0
range(realdeer_cutblocksbefore1997_summary$point)

randomdeer_cutblocksbefore1997_summary <- merge(df_random_points, df_randomdeer_cutblocksbefore1997, all = TRUE)
randomdeer_cutblocksbefore1997_summary[is.na(randomdeer_cutblocksbefore1997_summary)] <- 0


### Cutblocks 1997 to 2011
csv_realdeer_cutblocks1997to2011 <- read_csv("OSM Deer Docs/realdeer_cutblocks1997to2011_area.csv")
csv_randomdeer_cutblocks1997to2011 <- read_csv("OSM Deer Docs/randomdeer_cutblocks1997to2011_area.csv")

df_realdeer_cutblocks1997to2011 <- tibble(point = csv_realdeer_cutblocks1997to2011$OBJECTID_1, area = csv_realdeer_cutblocks1997to2011$AREA)
df_randomdeer_cutblocks1997to2011 <- tibble(point = csv_randomdeer_cutblocks1997to2011$OBJECTID_1, area = csv_randomdeer_cutblocks1997to2011$AREA)

realdeer_cutblocks1997to2011_summary <- merge(df_real_points, df_realdeer_cutblocks1997to2011, all = TRUE)
realdeer_cutblocks1997to2011_summary[is.na(realdeer_cutblocks1997to2011_summary)] <- 0
range(realdeer_cutblocks1997to2011_summary$point)

randomdeer_cutblocks1997to2011_summary <- merge(df_random_points, df_randomdeer_cutblocks1997to2011, all = TRUE)
randomdeer_cutblocks1997to2011_summary[is.na(randomdeer_cutblocks1997to2011_summary)] <- 0


### Cutblocks after 2011
csv_realdeer_cutblocksafter2011 <- read_csv("OSM Deer Docs/realdeer_cutblocksafter2011_area.csv")
csv_randomdeer_cutblocksafter2011 <- read_csv("OSM Deer Docs/randomdeer_cutblocksafter2011_area.csv")

df_realdeer_cutblocksafter2011 <- tibble(point = csv_realdeer_cutblocksafter2011$OBJECTID_1, area = csv_realdeer_cutblocksafter2011$AREA)
df_randomdeer_cutblocksafter2011 <- tibble(point = csv_randomdeer_cutblocksafter2011$OBJECTID_1, area = csv_randomdeer_cutblocksafter2011$AREA)

realdeer_cutblocksafter2011_summary <- merge(df_real_points, df_realdeer_cutblocksafter2011, all = TRUE)
realdeer_cutblocksafter2011_summary[is.na(realdeer_cutblocksafter2011_summary)] <- 0
range(realdeer_cutblocksafter2011_summary$point)

randomdeer_cutblocksafter2011_summary <- merge(df_random_points, df_randomdeer_cutblocksafter2011, all = TRUE)
randomdeer_cutblocksafter2011_summary[is.na(randomdeer_cutblocksafter2011_summary)] <- 0


## Organize datasets:
area_datasets <- list("realdeer_waterbodies_summary, randomdeer_waterbodies_summary", "realdeer_wellsites_Active_summary, randomdeer_wellsites_Active_summary", 
                      "realdeer_wellsites_abandoned_summary, randomdeer_wellsites_abandoned_summary", "realdeer_cultivation_summary, randomdeer_cultivation_summary", 
                      "realdeer_cutblocksbefore1997_summary, randomdeer_cutblocksbefore1997_summary", "realdeer_cutblocks1997to2011_summary, randomdeer_cutblocks1997to2011_summary", 
                      "realdeer_cutblocksafter2011_summary, randomdeer_cutblocksafter2011_summary",
                      "realdeer_area_summary, randomdeer_area_summary")



### Merge datasets -----------------------
## Random locations --
# Test datasets:
nrow(randomdeer_waterbodies_summary)
nrow(randomdeer_wellsites_active_summary)
nrow(randomdeer_wellsites_abandoned_summary)
nrow(randomdeer_cultivation_summary)
nrow(randomdeer_cutblocksbefore1997_summary)
nrow(randomdeer_cutblocks1997to2011_summary)
nrow(randomdeer_cutblocksafter2011_summary)
# 12255

# Merge datasets:
randomdeer_area_summary <- list(randomdeer_waterbodies_summary, randomdeer_wellsites_active_summary, randomdeer_wellsites_abandoned_summary, randomdeer_cultivation_summary, 
                                randomdeer_cutblocksbefore1997_summary, randomdeer_cutblocks1997to2011_summary, randomdeer_cutblocksafter2011_summary) %>% reduce(left_join, by = "point")

# Change column names:
names(randomdeer_area_summary) = c("point", "waterbodies area", "wellsites active area", "wellsites abandoned area", "cultivation area", "cutblocks_pre97", "cutblocks_97to11", 
                                   "cutblocks_post11")

# write_csv(randomdeer_area_summary, "randomdeer_area_summary.csv")

## Real locations --
nrow(realdeer_waterbodies_summary)
nrow(realdeer_wellsites_active_summary)
nrow(realdeer_wellsites_abandoned_summary)
nrow(realdeer_cultivation_summary)
nrow(realdeer_cutblocksbefore1997_summary)
nrow(realdeer_cutblocks1997to2011_summary)
nrow(realdeer_cutblocksafter2011_summary)
# 2470

# Merge datasets:
realdeer_area_summary <- list(realdeer_waterbodies_summary, realdeer_wellsites_active_summary, realdeer_wellsites_abandoned_summary, realdeer_cultivation_summary, 
                                realdeer_cutblocksbefore1997_summary, realdeer_cutblocks1997to2011_summary, realdeer_cutblocksafter2011_summary) %>% reduce(left_join, by = "point")

names(realdeer_area_summary) = c("point", "waterbodies area", "wellsites active area", "wellsites abandoned area", "cultivation area", "cutblocks_pre97", "cutblocks_97to11", 
                                   "cutblocks_post11")

# write_csv(realdeer_area_summary, "realdeer_area_summary.csv")

# Check columns 
head(realdeer_area_summary$`waterbodies area`)
head(realdeer_waterbodies_summary)

head(realdeer_area_summary$`wellsites active area`)
head(realdeer_wellsites_active_summary)

head(realdeer_area_summary$`wellsites abandoned area`)
head(realdeer_wellsites_abandoned_summary)

head(realdeer_area_summary$`cultivation area`)
head(realdeer_cultivation_summary)


### Add "Use" column to both datasets --
realdeer_area_summary$Use = 1
names(realdeer_area_summary)

randomdeer_area_summary$Use = 0 
names(randomdeer_area_summary)

### Merge real and random datasets ---
alldeer_feature_areas <- rbind(realdeer_area_summary, randomdeer_area_summary)
names(alldeer_feature_areas)

# write_csv(alldeer_feature_areas, "alldeer_feature_areas.csv")
# Has correct ranges: Used Loc's 1-2470,Random Loc's 1-12256
# Has "Use" column



##### SECTION 3: Merge alldeer_feature_areas with alldeer_NRCAN   ------------

# Investigate Datasets:
# 1) Both datasets have 12256 rows for Random Locations
# 2) Columns?

# Column Names
ls(alldeer_NRCAN)
ls(alldeer_feature_areas)

alldeer_feature_areas <- alldeer_feature_areas %>% rename(Point = point)

deer_merge <- merge(alldeer_NRCAN, alldeer_feature_areas, by = c("Point", "Use"))
glimpse(deer_merge)

deer_merge_main_master_file_big_Dec02 <- deer_merge
# write_csv(deer_merge, "deer_merge_main_file_Dec02.csv")

deer_distance_data <- deer_data
identical(deer_data, deer_distance_data)
glimpse(deer_distance_data)
# write_csv(deer_distance_data, "deer_distance_data_main_backup.csv")



##### SECTION 4: Create Main Dataframe   ---------------

deer_data <- tibble(
  Point = deer_merge$Point,
  Use = deer_merge$Use,
  grassland = deer_merge$Grassland,
  cultivated_low = deer_merge$`Cultivated Areas - Low Biomass`,
  cultivated_med = deer_merge$`Cultivated Areas - Medium Biomass`,
  cultivated_high = deer_merge$`Cultivated Areas - High Biomass`,
  herb_shrub_low = deer_merge$`Herb-Shrub, Low Vegetetaion Cover`,
  shrubland = deer_merge$Shrubland,
  broadleaf_regen = deer_merge$`Broadleaf - low to medium density, young regeneration`,
  broadleaf_medhigh = deer_merge$`Broadleaf - medium to high density`,
  mixedwood_regen = deer_merge$`Mixedwood - low to medium density, young regeneration`,
  mixedwood_medhigh = deer_merge$`Mixedwood - medium to high density`,
  conifer_low = deer_merge$`Conifer - low density`,
  conifer_medhigh = deer_merge$`Conifer - medium to high density`,
  wetland = deer_merge$`Wetland - Herb-Shrub, Treed`,
  water_area = deer_merge$`waterbodies area`,
  rivers_dist = deer_merge$`Rivers Distance (m)`,
  lakes_dist = deer_merge$`Lakes Distance (m)`,
  rail_dist = deer_merge$`Rail Distance (m)`,
  wellsites_dist = deer_merge$`Wellsites Distance (m)`,
  seismic_grid_dist = deer_merge$`Low_Impact_Seismic Distance (m)`,
  seismic_linetrail_dist = deer_merge$`Seismic Line Trail Distance`,
  pipeline_dist = deer_merge$`Pipeline Distance (m)`,
  transmission_dist = deer_merge$`Transmission Line Distance (m)`,
  roads_dist = deer_merge$`Roads Distance (m)`,
  cultivation_dist = deer_merge$Distance_Cultivation,
  cutblocks_dist = deer_merge$`Cutblocks Distance (m)`,
  wellsites_active_area = deer_merge$`wellsites active area`,
  wellsites_abandoned_area = deer_merge$`wellsites abandoned area`,
  cultivation_area = deer_merge$`cultivation area`,
  cutblocks_pre97 = deer_merge$cutblocks_pre97,
  cutblocks_97to11 = deer_merge$cutblocks_97to11,
  cutblockspost11 = deer_merge$cutblocks_post11,
)

glimpse(deer_data)
#   write_csv(deer_data, "deer_data_Dec02_main_backup.csv")


### SECTION 5 - Re-Run analysis with updated Roads Distances -------
backup_deer_merge <- deer_merge 

# Investigate NAs in roads
which(is.na(deer_merge$`Roads Distance (m)`))
plot(deer_merge$`Roads Distance (m)`)

deer_merge_used <- filter(deer_merge, Use == 1) 
deer_merge_available <- filter(deer_merge, Use == 0) 

which(is.na(deer_merge_available$`Roads Distance (m)`))
which(is.na(deer_merge_used$`Roads Distance (m)`))

# Which deer locations are greater than 10 km?
deer_data_used <- filter(deer_data, Use == 1) 
deer_data_available <- filter(deer_data, Use == 0) 

filter(deer_data_used, roads_dist_new > 10000)
filter(deer_data_available, roads_dist_new > 10000)

head(deer_data$roads_dist_new)
head(deer_data$roads_dist)
tail(deer_data$roads_dist_new)
tail(deer_data$roads_dist)


deer_merge <- deer_merge %>% rename(roads_distance = `Roads Distance (m)`)
test <- deer_merge %>% mutate(roads_distance = ifelse(Use == 1 & is.na(roads_distance), 10000, roads_distance))
test %>% filter(Use ==1) %>% is.na() %>% which()
test %>% filter(Use ==0) %>% is.na() %>% which()  # This is saying roads has NAs
deer_merge %>% filter(Use ==0, roads_distance > 0) %>% range(.$roads_distance)

ggplot(deer_merge) + 
  geom_point(aes(x = Use, y = roads_distance)) + ylim(0, 11000) + theme_bw()
ggplot(deer_data) + 
  geom_point(aes(x = Use, y = roads_dist)) + ylim(0, 16000) + theme_bw()


### Load updated distances table 
# Ran distance table in ArcMap, from all ABMI roads to deer locations
new_table <- read_csv("deer_roads_dist_newMay12.csv") 
new_table <- new_table %>% rename(roads_dist_new = NEAR_DIST)
roads_table <- new_table %>% dplyr::select(Pont_ID, roads_dist_new)
roads_table <- roads_table %>% rename(Point_ID = Pont_ID)
deer_data <- deer_data %>% rename(Point_ID = Point)

deer_data <- left_join(deer_data, roads_table, by = "Point_ID")


### Investigate new roads data
which(is.na(deer_data$roads_dist_new))
range(deer_data$roads_dist_new)  # 0 to 18976

p.roads_dist_new <- ggplot(deer_data) + 
  geom_point(aes(x = Use, y = roads_dist_new)) + ylim(0, 16000) + theme_bw()
# MUCH better plot 
p.roads_dist_old <- ggplot(deer_data) + 
  geom_point(aes(x = Use, y = roads_dist)) + ylim(0, 16000) + theme_bw()

plot_grid(p.roads_dist_old, p.roads_dist_new)
# Notice old plot only goes to 1000 for random locations, and new plot goes past 1500 for random locations. Much better.

library(cowplot)
plot_grid(p.roads_dist_old, p.roads_dist_new) %>% save_plot("roads_distances_old_new_comparison.jpeg", ., base_width = 8, base_height = 8)




##### SECTION 8: Correlation Coefficients and Variance Inflation ------------

# Variable Lists:
deer_data_variables
deer_data_distance_variables
deer_data_NRCAN_lc_variables
deer_data_all_feature_area_variables

# Seismic Lines and Regenerating Forest
cor_forest_regen_seismic_lines <- print(cor(deer_data[, c("mixedwood_regen", "broadleaf_regen", "seismic_linetrail_dist")]))

# Seismic Lines and Broadleaf-Mixedwood Forest
cor_broadleaf_mixedwood_seismic_lines <- print(cor(deer_data[, c("mixedwood_medhigh", "broadleaf_medhigh", "seismic_linetrail_dist")]))

# COR all Distance Variables
cor_distance_variables <- print(cor(deer_data[, c(deer_data_distance_variables)]))
stargazer(cor_distance_variables, summary = FALSE, type = "text", out = "cor_deer_data_distance_variables.html")
## Note: Roads-Rail correlation = 0.661. Very correlated.

# VIF all distance variables
vif_distance_variables <- print(vif(deer_data[, c(deer_data_distance_variables)]))
## Railways has highest VIF (2.21), but not over 3.

### Seismic Lines Correlation --  # outcome: cor_seismic_linetrail_all_variables
## Function: flattenCorrMatrix
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
library(Hmisc)
cor_test <-rcorr(as.matrix(deer_data))   # Create COR Matrix
cor_test_tbl <- flattenCorrMatrix(cor_test$r, cor_test$P)   # Flatten matrix to table 
view(cor_test_tbl) # Extract seismic_linetrail values --- Rows 213:231 + extra rows below
cor_seismic_linetrail_all_variables <- cor_test_tbl[c(213:231, 253, 275, 298, 322, 347, 373, 400, 428, 457, 487, 518), 1:3]

test1 <- cor_seismic_linetrail_all_variables[1:19, ]
test2 <- cor_seismic_linetrail_all_variables[20:30, ]
test1 <- select(test1, column, everything())
test1 <- rename(test1, row1 = column)
test1 <- rename(test1, column = row)
test2 <- rename(test2, row1 = row)
test3 <- rbind(test1, test2)

cor_table_seismic_lines_allvariables <- test3
cor_table_seismic_lines_allvariables <- cor_table_seismic_lines_allvariables %>% rename(Seismic_Lines = row1)
cor_table_seismic_lines_allvariables <- cor_table_seismic_lines_allvariables %>% rename(Paired_Variable = column)
cor_table_seismic_lines_allvariables <- cor_table_seismic_lines_allvariables %>% rename(Correlation_Coefficient = cor)

cor_table_seismic_lines_allvariables <- cor_table_seismic_lines_allvariables[
  order(cor_table_seismic_lines_allvariables$Correlation_Coefficient), ]

stargazer(cor_table_seismic_lines_allvariables, summary = FALSE, type = "text", out = "Results/cor_table_seismic_lines_allvariables.html")


### Part 1 - Extract Broadleaf values from "cor_test_tbl" ----
# All correlation coefficients for every variable in "deer_data"
View(cor_test_tbl)

# Extract just broadleaf_regen
cor_broadleaf_regen <- filter(cor_test_tbl, row == "broadleaf_regen" | column == "broadleaf_regen")

# Split table based on where broadleaf_regen in in a row vs. in a column
cor_broadleaf_regen_top_half <- filter(cor_broadleaf_regen, column == "broadleaf_regen")
cor_broadleaf_regen_bottom_half <- filter(cor_broadleaf_regen, row == "broadleaf_regen")

# Rename columsn to faciliate merge
cor_broadleaf_regen_top_half <- rename(cor_broadleaf_regen_top_half, Broadleaf_Regen = column)
cor_broadleaf_regen_top_half <- rename(cor_broadleaf_regen_top_half, Paired_Variable = row)
cor_broadleaf_regen_top_half <- cor_broadleaf_regen_top_half[c(3:8), ]

cor_broadleaf_regen_bottom_half <- rename(cor_broadleaf_regen_bottom_half, Broadleaf_Regen = row)
cor_broadleaf_regen_bottom_half <- rename(cor_broadleaf_regen_bottom_half, Paired_Variable = column)

# Merge tables into one correlation table
cor_broadleaf_regen_table <- rbind(cor_broadleaf_regen_top_half, cor_broadleaf_regen_bottom_half)
# cor_broadleaf_regen_table <- select(cor_broadleaf_regen_table, Broadleaf_Regen, everything()) -- not working

# Order columns
cor_broadleaf_regen_table <- cor_broadleaf_regen_table[, c(2, 1, 3, 4)]

# Order rows by value of cor column
cor_broadleaf_regen_table <- cor_broadleaf_regen_table[
  order(cor_broadleaf_regen_table$cor), ]

stargazer(cor_broadleaf_regen_table, summary = FALSE, type = "text", out = "Results/cor_broadleaf_regen_table.html")

# <FLAG> insert the better way of doing this from Oak Bay Deer script


### Part 2 - Correlation Plots ----
# Note: these are a handy way of visualizing more detail in relationships showed in correlation tables 

# Broadleaf Regeneration
p_brlf_regen_seis_line <- ggplot(deer_data, aes(x = seismic_linetrail_dist, y = broadleaf_regen)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
p_brlf_regen_seis_grid <- ggplot(deer_data, aes(x = seismic_grid_dist, y = broadleaf_regen)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
p_brlf_regen_roads <- ggplot(deer_data, aes(x = roads_dist, y = broadleaf_regen)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
p_brlf_regen_rails <- ggplot(deer_data, aes(x = rail_dist, y = broadleaf_regen)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
plot_cor_broadleaf_regen <- plot_grid(p_brlf_regen_seis_line, p_brlf_regen_seis_grid, 
                                      p_brlf_regen_roads, p_brlf_regen_rails, nrow = 1)
save_plot("Results/plot_cor_broadleaf_regen.jpeg", plot_cor_broadleaf_regen, base_width = 20, base_height = 6.66)


p_wetland_seis_line <- ggplot(deer_data, aes(x = seismic_linetrail_dist, y = wetland)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
p_wetland_seis_grid <- ggplot(deer_data, aes(x = seismic_grid_dist, y = wetland)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
p_wetland_roads <- ggplot(deer_data, aes(x = roads_dist, y = wetland)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
p_wetland_conifer_medhigh <- ggplot(deer_data, aes(x = conifer_medhigh, y = wetland)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
p_wetland_broadleaf_regen <- ggplot(deer_data, aes(x = broadleaf_regen, y = wetland)) + 
  geom_point(color = "grey22", alpha = 0.6) +
  theme_bw() + 
  geom_smooth(method = "lm", se = FALSE)
print(cor(deer_data[, c("conifer_medhigh", "wetland", "seismic_linetrail_dist")]))

# Correlation of 0.21 between wetland and conifer
plot_cor_wetland_seisline_conifer <- plot_grid(p_wetland_seis_line, p_wetland_conifer_medhigh, nrow = 1)
save_plot("Results/plot_cor_wetland_seisline_conifer.jpeg", plot_cor_wetland_seisline_conifer, base_width = 8, base_height = 3)


### Part 3 - Correlation table for variables in the Global StepAIC Model -----
stepAIC_variable_list <- c("cultivated_low", "cultivated_med", "cultivated_high", "herb_shrub_low", "shrubland", "broadleaf_regen", 
                              "broadleaf_medhigh", "mixedwood_regen", "mixedwood_medhigh", "conifer_low", "conifer_medhigh", "wetland", 
                              "rivers_dist", "lakes_dist", "roads_dist", "seismic_grid_dist", "seismic_linetrail_dist", "pipeline_dist", 
                              "transmission_dist", "cutblocks_pre97")

# Note: must use rcorr to produce class "rcorr" correlation matrix for this to work. Don't use cor() function. 
cor_stepAIC_variable_matrix <- rcorr(as.matrix((deer_data[, c(stepAIC_variable_list)])))
cor_stepAIC_variable_tbl <- flattenCorrMatrix(cor_stepAIC_variable_matrix$r, cor_stepAIC_variable_matrix$P) %>% as_tibble()

# Select all values greater than abs 0.2, and round to 3 digits
cor_stepAIC_variables_greater_point2 <- filter(cor_stepAIC_variable_tbl, cor >= 0.2 | cor <= -0.2)
cor_stepAIC_variables_greater_point2 <- cor_stepAIC_variables_greater_point2[order(cor_stepAIC_variables_greater_point2$cor), ]
cor_stepAIC_variables_greater_point2 <- cor_stepAIC_variables_greater_point2 %>% mutate_if(is.numeric, round, digits = 3)

# stargazer(cor_stepAIC_variables_greater_point2, summary = FALSE, type = "text", out = "Results/cor_stepAIC_variables_greater_point2.html")
#  This removes variable names from the columns and rows.

step_AIC_lakes_correlation <- filter(cor_stepAIC_variable_tbl, row == "lakes_dist" | column == "lakes_dist")



### Part 4 - Correlation table for variables in the global model --------
coefficients(m_global)
library(Hmisc)
m_global_variable_list <- c("grassland","cultivated_med", "cultivated_high", "herb_shrub_low", "shrubland", "broadleaf_regen", 
                           "broadleaf_medhigh", "mixedwood_regen", "mixedwood_medhigh", "conifer_low", "conifer_medhigh", "wetland", 
                           "rivers_dist", "lakes_dist", "roads_dist", "seismic_grid_dist", "seismic_linetrail_dist", "pipeline_dist", 
                           "transmission_dist", "wellsites_dist", "cutblocks_pre97", "cutblocks_97to11", "cutblockspost11")

# Note: must use rcorr to produce class "rcorr" correlation matrix for this to work. Don't use cor() function. 
cor_m_global_variable_list <- rcorr(as.matrix((deer_data[, c(m_global_variable_list)])))
cor_m_global_variable_list_tbl <- flattenCorrMatrix(cor_m_global_variable_list$r, cor_m_global_variable_list$P) %>% as_tibble()

# Select all values greater than abs 0.2, and round to 3 digits
cor_m_global_variables_greater_point2 <- filter(cor_m_global_variable_list_tbl, cor >= 0.2 | cor <= -0.2)
cor_m_global_variables_greater_point2 <- cor_m_global_variables_greater_point2[order(cor_m_global_variables_greater_point2$cor), ]
cor_m_global_variables_greater_point2 <- cor_m_global_variables_greater_point2 %>% mutate_if(is.numeric, round, digits = 3)

# stargazer(cor_stepAIC_variables_greater_point2, summary = FALSE, type = "text", out = "Results/cor_stepAIC_variables_greater_point2.html")
#  This removes variable names from the columns and rows.

cor_m_global_variable_list_tbl %>% arrange(desc(row)) %>% print(n = nrow(.))
cor_m_global_variable_list_tbl %>% arrange(desc(column)) %>% print(n = nrow(.))

# Vif
print(vif(deer_data[, c(m_global_variable_list)]))
# Removing cultivated_low resolves this...


##### SECTION 9: Plot Histograms  ----------

### Binomial Distribution Plots ---
with(deer_data, plot(cultivation_area, Use)) # Wide range of area
with(deer_data, plot(wetland, Use))   # Used sites have low area of wetlands
with(deer_data, plot(mixedwood_regen, Use))  # Used sites have low area of mixedwood regen 
with(deer_data, plot(broadleaf_regen, Use))  # More positive skew than mixedwoodregen
with(deer_data, plot(cutblocks_pre97, Use))  
with(deer_data, plot(seismic_linetrail_dist, Use))


### Histograms ---
# Convert to data frame and plot histogram
test <- as.data.frame(deer_data)
with(test[test$Use == 1, ], hist(wetland, breaks = 100))

# Seismic Lines
hist_seis_line_use <- ggplot(deer_data[deer_data$Use == 1, ], aes(x = seismic_linetrail_dist)) + 
  geom_histogram(binwidth = 15, fill = "grey21") + 
  theme_bw() + xlim(0, 3000) + 
  xlab("Seismic Line Distance Used")
ggsave("Results/hist_seis_line_use.jpeg", hist_seis_line_use, scale = 1)

ggplot(deer_data[deer_data$Use == 1, ], aes(x = seismic_linetrail_dist)) + 
  geom_histogram(binwidth = 15, fill = "grey21") + 
  xlab("Seismic Line Distance Used")
# Note range is 0-3000, compared to range of distances to seismic grids which is 0-35,000

hist_seis_line_avail <- ggplot(deer_data[deer_data$Use == 0, ], aes(x = seismic_linetrail_dist)) + 
  geom_histogram(binwidth = 15, fill = "grey21") + 
  theme_bw() + xlim(0, 3000) + 
  xlab("Seismic Line Distance Available")
ggsave("Results/hist_seis_line_avail.jpeg", hist_seis_line_avail, scale = 1)

ggplot(deer_data[deer_data$Use == 0, ], aes(x = seismic_linetrail_dist)) + 
  geom_histogram(binwidth = 15, fill = "grey21") + 
  xlab("Seismic Line Distance Available")
# Note range is 0-3000/5000, compared to range of distances to seismic grids which is 0-40,000

hist_seis_line_use_avail <- plot_grid(hist_seis_line_use, hist_seis_line_avail)
ggsave("Results/hist_seis_line_use_avail.jpeg", hist_seis_line_use_avail, width = 5, height = 3)

nrow(filter(deer_data, Use == 0, seismic_linetrail_dist <= 1000))  # 11226
nrow(filter(deer_data, Use == 0, seismic_linetrail_dist > 1000))   # 936



# Seismic Grids
hist_seis_grid_use <- ggplot(deer_data[deer_data$Use == 1, ], aes(x = seismic_grid_dist)) + 
  geom_histogram(binwidth = 12, fill = "grey21") + 
  theme_bw() + xlim(0, 3000) + 
  xlab("Seismic Grids Distance Used")
ggsave("hist_seis_grid_use.jpeg", hist_seis_grid_use, scale = 1)

ggplot(deer_data[deer_data$Use == 1, ], aes(x = seismic_grid_dist)) + 
  geom_histogram(binwidth = 12, fill = "grey21")
  xlab("Seismic Grids Distance Used")
# Without xlimit of 3000 - shows the full range of distance values
### Note - range is 0 to 35,000 m - 35 km!

hist_seis_grid_avail <- ggplot(deer_data[deer_data$Use == 0, ], aes(x = seismic_grid_dist)) + 
  geom_histogram(binwidth = 12, fill = "grey21") + 
  theme_bw() + xlim(0, 3000) + 
  xlab("Seismic Grids Distance Available")
ggsave("hist_seis_grid_avail.jpeg", hist_seis_grid_avail, scale = 1)

ggplot(deer_data[deer_data$Use == 0, ], aes(x = seismic_grid_dist)) + 
  geom_histogram(binwidth = 12, fill = "grey21") + 
  xlab("Seismic Grids Distance Available")
# Without xlimit of 3000 - shows the full range of distance values
### Note - range is 0 to 40,000 m!!!

hist_seis_grid_use_avail <- plot_grid(hist_seis_grid_use, hist_seis_grid_avail)
ggsave("hist_seis_grid_use_avail.jpeg", hist_seis_grid_use_avail, width = 6, height = 3)

ggplot(deer_data[deer_data$Use == 1, ], aes(x = seismic_grid_dist)) + 
  geom_histogram(binwidth = 15, fill = "grey21") + 
  theme_bw()

### Note the HUGE difference in distances to seismic lines vs. distances to seismic grids. Selection of seismic grids may be 
# not adequately captured here because of the uneven distribution compared to seismic lines. Lots of deer have distances exceeding 2 km, 
# likely beyond the distance they actually select for habitat features in their home range.




# Area Variables --
# Wetland - needs massive bin-width to visualize
ggplot(data = deer_data, aes(x = wetland)) + 
  geom_histogram(binwidth = 1000, fill = "grey21") + 
  theme_bw()

ggplot(deer_data[deer_data$Use == 1, ], aes(x = wetland)) + 
  geom_histogram(binwidth = 1000, fill = "grey21") + 
  theme_bw()

# Wellsites abandoned
ggplot(data = deer_data, aes(x = wellsites_abandoned_area)) + 
  geom_histogram(binwidth = 1000, fill = "grey21") + 
  theme_bw()

# Cultivation
ggplot(deer_data[deer_data$Use == 1, ], aes(x = cultivation_area)) + 
  geom_histogram(binwidth = 1000, fill = "grey21") + 
  theme_bw()


##### SECTION 10: Coefficient Tables ------

distance_variables <- distance_variables %>% mutate_if(is.numeric, round, digits = 4)
coef_global_distance_vars <- stargazer(distance_variables, summary = FALSE, type = "text", 
          out = "Results/coef_distance_variables.html")

area_variables_by2sd <- tidy(natural_landcover_plus_linear_features_plus_cultivation_cutblocks)[c(2:14, 24:26), ] %>% by_2sd(deer_data)
area_variables_by2sd <- area_variables_by2sd %>% mutate_if(is.numeric, round, digits = 4)
coef_global_area_vars <- stargazer(area_variables_by2sd, summary = FALSE, type = "text", 
                                   out = "Results/area_variables_by2sd.html")



##### Section 12 A -- Visreg *global model* plots -------
visreg_global <- visreg(natural_landcover_plus_linear_features_plus_cultivation_cutblocks, 
                        scale = "response", ylim = c(0,1))
# This produces an unholy mess of plots for every single variable 

visreg_seis_lines <- visreg(natural_landcover_plus_linear_features_plus_cultivation_cutblocks, 
                            "seismic_linetrail_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Seismic Line Distance (m)") + theme_bw()

visreg_seis_grids <- visreg(natural_landcover_plus_linear_features_plus_cultivation_cutblocks, 
                            "seismic_grid_dist", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Seismic Grid Distance (m)") + theme_bw()

visreg_broadleaf_regen <- visreg(natural_landcover_plus_linear_features_plus_cultivation_cutblocks, 
                                 "broadleaf_regen", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Area Broadleaf Regeneration (m^2)") + theme_bw()

visreg_conifer_medhigh <- visreg(natural_landcover_plus_linear_features_plus_cultivation_cutblocks, 
                                 "conifer_medhigh", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Area Coniferous Forest, Medium to High Density (m^2)") + theme_bw()

visreg_wetland <- visreg(natural_landcover_plus_linear_features_plus_cultivation_cutblocks, 
                         "wetland", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Area Wetlands (m^2)") + theme_bw()

visreg_cutblocks_97to11 <- visreg(natural_landcover_plus_linear_features_plus_cultivation_cutblocks, 
                                  "cutblocks_97to11", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Area Cutblocks 5 to 20 years old (m^2)") + theme_bw()

visreg_global_intercept <- plot_grid(visreg_seis_lines, visreg_seis_grids, visreg_cutblocks_97to11, visreg_broadleaf_regen, visreg_conifer_medhigh, visreg_wetland)
save_plot("visreg_global_intercept.jpeg", visreg_global_intercept, base_width = 10, base_height = 8)


##### Section 12 B -- Visreg StepAIC *Intercept* plots -------
v_seis_lines_stepAIC_intercept <- visreg(natural_landcover_linear_block_stepAIC_model, 
                                         "seismic_linetrail_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Seismic Line Distance (m)") + theme_classic()

v_seis_grids_stepAIC_intercept <- visreg(natural_landcover_linear_block_stepAIC_model, 
                                         "seismic_grid_dist", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Seismic Grid Distance (m)") + theme_bw()

v_broadleaf_regen_stepAIC_intercept <- visreg(natural_landcover_linear_block_stepAIC_model, 
                                              "broadleaf_regen", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Area Broadleaf Regeneration (m^2)") + theme_bw()

v_conifer_medhigh_stepAIC_intercept <- visreg(natural_landcover_linear_block_stepAIC_model, 
                                              "conifer_medhigh", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Area Coniferous Forest, Medium to High Density (m^2)") + theme_bw()

v_wetland_stepAIC_intercept <- visreg(natural_landcover_linear_block_stepAIC_model, 
                                      "wetland", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Area Wetlands (m^2)") + theme_bw()

v_cutblocks_pre97_stepAIC_intercept <- visreg(natural_landcover_linear_block_stepAIC_model, 
                                              "cutblocks_pre97", scale = "response", ylim = c(0,1), gg=TRUE) + 
  xlab("Area Cutblocks over 20 years old (m^2)") + theme_bw()


visreg_stepAIC_intercept <- plot_grid(v_seis_lines_stepAIC_intercept, v_seis_grids_stepAIC_intercept, 
                                      v_cutblocks_pre97_stepAIC_intercept, v_broadleaf_regen_stepAIC_intercept, 
                                      v_conifer_medhigh_stepAIC_intercept, v_wetland_stepAIC_intercept)
save_plot("visreg_stepAIC_intercept.jpeg", visreg_stepAIC_intercept, base_width = 10, base_height = 8)



##### SECTION 13: Mapping --- Add location data  ------
backup_deer_data_Dec12 <- deer_data

# Obtain row names for attaching location information
nrow(deer_data) # 14632
length(unique(deer_data$Point_ID)) # 12174
# Point cannot be used to merge location data.

# Split deer_data by use 0/1 then merge location data, assign a unique row_ID (1-14632), and re-merge?

## Note, this is messy AF, so here is the start-point, end-point (goal) and steps. 
# Goal: add spatial locations back to the deer_data file. (I had removed spatial attributes from deer_data to make it work with some modelling step above)
# Start: locations for used and available (real and random) points, and a table with habitat variables but no spatial data (deer_data file)

# Steps:
# Load shapefiles and convert to csv's with identical columns - produce a real_deer and random_deer csv with identical columns
# Split the deer landcover data into Real and Random (Use == 1 and Use == 0)
# Join real_deer and random_deer tables with their respective habitat data, based on Point_ID
# Join real_deer and random_deer locations back together, with the data that was joined to them

# Load shapefiles 
real_deer <- st_read("Data/realdeer_locations.shp")
random_deer <- st_read("Data/randomdeer_locations.shp")

# Export shapefiles to csv and gpkg
st_write(real_deer, dsn = "real_deer_locations.gpkg", layer = "real_deer_locations")
st_write(random_deer, dsn = "random_deer_locations.gpkg", layer = "random_deer_locations")

## Export shapefiles to table, then .csv --
# Extract UTM Easting and Northing and add to the sf
real_deer_withUTM <- do.call(rbind, st_geometry(real_deer)) %>% 
  as_tibble() %>% setNames(c("Easting","Northing"))
real_deer$Easting = real_deer_withUTM$Easting
real_deer$Northing = real_deer_withUTM$Northing
## Check that added Eastings and Northings match Geometry column

# Remove geometry and save as .csv
real_deer %>% as_tibble() %>% dplyr::select(-geometry) %>% write_csv(., "real_deer_with_UTM.csv")

random_deer_withUTM <- do.call(rbind, st_geometry(random_deer)) %>% 
  as_tibble() %>% setNames(c("Easting","Northing"))
random_deer$Easting = random_deer_withUTM$Easting
random_deer$Northing = random_deer_withUTM$Northing
## Check that added Eastings and Northings match Geometry column

# Remove geometry and save as .csv
random_deer %>% as_tibble() %>% dplyr::select(-geometry) %>% write_csv(., "random_deer_with_UTM.csv")

## Clean csv's - select the same columns and rename existing columns so real_deer and random_deer can be joined 
real_deer <- real_deer %>% dplyr::select(OBJECTID, Easting, Northing, Latitude, Longitude)
names(real_deer)

names(random_deer)
random_deer <- random_deer %>% dplyr::select(OBJECTI, Easting, Northing)

random_deer <- rename(random_deer, Longitude = POINT_X)
random_deer <- rename(random_deer, Latitude = POINT_Y)

# Remove excess rows from Random Deer 
random_deer <- random_deer[1:12162, ]

# Split deer_data
real_deer_data <- deer_data[deer_data$Use == 1, ]
random_deer_data <- deer_data[deer_data$Use == 0, ]

# Merge location data with split data
real_deer_locmerge <- bind_cols(real_deer_data, real_deer)
random_deer_locmerge <- bind_cols(random_deer_data, random_deer)

### Repeat with tibbles not sf -- joining sf previously removes lat/long information so keep as tibble then convert to sf (what does this mean???)
nrow(real_deer_locmerge)
nrow(random_deer_locmerge)

real_deer_locmerge$Point_ID = 1:2470
real_deer_locmerge <- select(real_deer_locmerge, Point_ID, everything())

random_deer_locmerge$Point_ID = 2471:14632
random_deer_locmerge <- select(random_deer_locmerge, Point_ID, everything())

deer_data_loc <- bind_rows(real_deer_locmerge, random_deer_locmerge)
View(deer_data_loc)

deer_data_loc <- deer_data_loc[, -39]
deer_data_loc_sf <- st_as_sf(deer_data_loc, coords = c("Longitude", "Latitude"), crs = 4326)

tmap_mode('view')
tm_shape(deer_data_loc_sf) + 
  tm_dots(col = "darkblue")

### Test if deer_data and deer_data_loc are identical 
 deer_data <- deer_data[
  order(deer_data$Use, decreasing = TRUE), ]

### Deer_Data and Deer_Data_Loc are IDENTICAL for all the data columns. 
# Deer_Data_Loc came from deer_data split based on use, merged with Lat/Long based on Point, and re-bound based on all column IDs.
identical(deer_data[, 1:35], deer_data_loc[, 1:35])  # TRUE

deer_data$Point_ID = 1:14632
deer_data <- deer_data %>% dplyr::select(Point_ID, everything())

# Save files
# backup_deer_data_loc <- deer_data_loc
# write_csv(deer_data_loc, "deer_data_main_with_coordinatesDec13.csv")
# write_csv(deer_data, "deer_data_main_Dec13.csv")
# st_write(deer_data_loc_sf, dsn = "deer_data_loc_sf_Dec13.shp", layer = "deer_data_loc_sf_Dec13")

st_write(deer_data_loc_sf, dsn = "OSM Deer - Input Data/deer_data_loc_sf_Jan15.shp", layer = "deer_data_loc_sf_Jan15")
st_write(deer_data_loc_sf, dsn = "OSM Deer - Input Data/deer_data_loc_sf_Jan15.gpkg", layer = "deer_data_loc_sf_Jan15")

### More Mapping:
tm_shape(nrcan_lc) + 
  tm_raster() + 
tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 0, ]) + 
  tm_dots(col = "lightblue") +
tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 1, ]) + 
  tm_dots(col = "grey19") + 
tm_layout(legend.show = FALSE, 
          frame = TRUE)



### SECTION 19: Presentation Materials  -------------
# AIC visual table 
# Dot-whisker plot
# Model plots
# Map?

# Models ---
m_global
m_natural_landcover
m_natural_landcover_distance_riparian
m_cultivation_cutblocks #  <- block_features
m_linear_features


# AIC Scores from Above
kable(AIC_scores)


### Part 1 - Dot-Whisker Plots -----
m_global
dwplot(m_global) + theme_bw()

dw_global <- dwplot(m_global, whisker_args = aes(color = "royalblue2", size = 0.75, alpha = 0.65), 
                    dot_args = aes(color = "royalblue3", size = 2, shape = 21, fill = "dodgerblue2")) %>% relabel_predictors(c(
                      grassland = "Grassland",
                      herb_shrub_low = "Herb-Shrub Low",
                      shrubland = "Shrubland",
                      broadleaf_regen = "Broadleaf, Regenerating",
                      broadleaf_medhigh = "Broadleaf, Medium to High Density",
                      mixedwood_regen = "Mixedwood, Regenerating",
                      mixedwood_medhigh = "Mixedwood, Medium to High Density",
                      conifer_low = "Conifer, Low Density",
                      conifer_medhigh = "Conifer, Medium to High Density",
                      wetland = "Wetlands",
                      rivers_dist = "Rivers Distance", 
                      lakes_dist = "Lakes Distance",
                      cultivated_low = "Cultivated, Low Biomass",
                      cultivated_med = "Cultivated, Medium Biomass",
                      cultivated_high = "Cultivated, High Biomass",
                      cutblocks_pre97 = "Cutblocks pre-1997", 
                      cutblocks_97to11 = "Cutblocks 1997 to 2011", 
                      cutblockspost11 = "Cutblocks post-2011",
                      roads_dist = "Roads Dist",
                      seismic_grid_dist = "Seismic Grids Dist", 
                      seismic_linetrail_dist = "Seismic Lines Dist", 
                      pipeline_dist = "Pipelines Dist", 
                      transmission_dist = "Transmission Lines Dist", 
                      wellsites_dist = "Wellsites Dist"
                    )) +
  theme_bw() + 
  ggtitle("Coefficient Estimates for Global RSF Model") + 
  geom_vline(xintercept = 0, colour = "grey65", linetype = 2) + 
  theme(plot.title = element_text(size = 11)) + 
  theme(legend.position = "none") + scale_x_continuous(labels = scales::comma)

# Change colours based on value (show distance vs. area variables)
test <- ggplot_build(dw_global)

test$data[[2]] <- test$data[[2]] %>% mutate(colour = ifelse(ymin < 6.8, "indianred3", "royalblue3")) %>% 
  mutate(fill = ifelse(ymin < 6.8, "red3", "dodgerblue2"))

test$data[[1]] <- test$data[[1]] %>% mutate(colour = ifelse(ymin < 6.8, "firebrick3", "royalblue3"))

dw_global <- ggplot_gtable(test)
plot(dw_global)

save_plot("Results/dw_global.jpeg", dw_global, base_heigh = 8.5, base_width = 7)


# Natural features
dw_natural_landcover <- dwplot(m_natural_landcover_distance_riparian, 
                               whisker_args = aes(color = "royalblue2", size = 0.75, alpha = 0.65), 
                               dot_args = aes(color = "royalblue3", size = 2, shape = 21, fill = "dodgerblue2")) %>% relabel_predictors(c(
                                 grassland = "Grassland",
                                 herb_shrub_low = "Herb-Shrub Low",
                                 shrubland = "Shrubland",
                                 broadleaf_regen = "Broadleaf, Regenerating",
                                 broadleaf_medhigh = "Broadleaf, Medium to High Density",
                                 mixedwood_regen = "Mixedwood, Regenerating",
                                 mixedwood_medhigh = "Mixedwood, Medium to High Density",
                                 conifer_low = "Conifer, Low Density",
                                 conifer_medhigh = "Conifer, Medium to High Density",
                                 wetland = "Wetlands",
                                 rivers_dist = "Rivers Distance", 
                                 lakes_dist = "Lakes Distance")) + 
  theme_bw() +
  geom_vline(xintercept = 0, colour = "grey65", linetype = 2) + 
  ggtitle("Coefficient Estimates for the 'Natural Land-Cover' Model") + 
  theme(plot.title = element_text(size = 11)) +
  theme(legend.position = "none") + scale_x_continuous(labels = scales::comma)

ggsave("Results/dw_natural_landcoverer.jpeg", dw_natural_landcover, scale = 1)

# Block features
dw_block_features <- dwplot(m_cultivation_cutblocks, 
                            whisker_args = aes(color = "royalblue2", size = 0.75, alpha = 0.65), 
                            dot_args = aes(color = "royalblue3", size = 2, shape = 21, fill = "dodgerblue2")) %>% relabel_predictors(c(
                              cultivated_low = "Cultivated, Low Biomass",
                              cultivated_med = "Cultivated, Medium Biomass",
                              cultivated_high = "Cultivated, High Biomass",
                              cutblocks_pre97 = "Cutblocks pre-1997", 
                              cutblocks_97to11 = "Cutblocks 1997 to 2011", 
                              cutblockspost11 = "Cutblocks post-2011")) + 
  theme_bw() + 
  geom_vline(xintercept = 0, colour = "grey65", linetype = 2) + 
  ggtitle("Coefficient Estimates for the 'Block Features' Model") + 
  theme(plot.title = element_text(size = 11)) + 
  theme(legend.position = "none") + scale_x_continuous(labels = scales::comma)

ggsave("Results/dw_block_features.jpeg", dw_block_features, scale = 1)

# Linear features
dw_linear_features <- dwplot(m_linear_features, 
                             whisker_args = aes(color = "royalblue2", size = 0.75, alpha = 0.65), 
                             dot_args = aes(color = "royalblue3", size = 2, shape = 21, fill = "dodgerblue2")) %>% relabel_predictors(c(
                               roads_dist = "Roads Dist",
                               seismic_grid_dist = "Seismic Grids Dist", 
                               seismic_linetrail_dist = "Seismic Lines Dist", 
                               pipeline_dist = "Pipelines Dist", 
                               transmission_dist = "Transmission Lines Dist", 
                               wellsites_dist = "Wellsites Dist")) +
  theme_bw() + xlab("Coefficient Estimates") + 
  geom_vline(xintercept = 0, colour = "grey65", linetype = 2) + 
  ggtitle("Coefficient Estimates for the 'Linear Features' Model") + 
  theme(plot.title = element_text(size = 12)) + 
  theme(legend.position = "none")

ggsave("Results/dw_linear_features.jpeg", dw_linear_features, scale = 1)

plot_grid(dw_natural_landcover, dw_block_features, dw_linear_features, nrow = 1)
save_plot("Results/dw_nat_block_lin.jpeg", plot_grid(dw_natural_landcover, dw_block_features, dw_linear_features, nrow = 1), 
          base_width = 15, base_height = 6)




### Part 2 - Visreg Model Plots -----

# b <- visreg(m_linear_features, "seismic_linetrail_dist", scale = "response", ylim = c(0,1), gg=TRUE) + xlab("Seismic Line Distance (m)") + theme_bw() + ggtitle("Subset Model")
# plot_grid(a, b)

pred_broadleaf_regen <- visreg(m_global, "broadleaf_regen", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Regenerating Broadleaf Area (m2)") + theme_bw()
# d <- visreg(m_natural_landcover, "broadleaf_regen", scale = "response", ylim = c(0,1), gg=TRUE) + xlab("Regenerating Broadleaf Forest Area (m2)") + theme_bw() + ggtitle("Subset Model")

pred_broadleaf_medhigh <- visreg(m_global, "broadleaf_medhigh", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Broadleaf Med-High Area (m2)") + theme_bw() + ggtitle("Global Model")
# visreg(m_natural_landcover, "broadleaf_medhigh", scale = "response", ylim = c(0,1), gg=TRUE) + xlab("Regenerating Broadleaf Forest Area (m2)") + theme_bw() + ggtitle("Subset Model")

pred_lakes_dist <- visreg(m_global, "lakes_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Lakes Distance (m)") + theme_bw()
pred_rivers_dist <- visreg(m_global, "rivers_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Rivers Distance (m)") + theme_bw()

# f <- visreg(m_natural_landcover_distance_riparian, "lakes_dist", scale = "response", ylim = c(0,1), gg=TRUE) + xlab("Lakes Distance (m)") + theme_bw() + ggtitle("Subset Model")

pred_conifer_medhigh <- visreg(m_global, "conifer_medhigh", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Coniferous Med-High Density (m2)") + theme_bw()
pred_conifer_low <- visreg(m_global, "conifer_low", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Coniferous Low Density (m2)") + theme_bw() + ggtitle("Global Model")
pred_wetland <- visreg(m_global, "wetland", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Wetland (m2)") + theme_bw()

pred_grassland <- visreg(m_global, "grassland", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("grassland (m2)") + theme_bw()
pred_herb_shrub_low <- visreg(m_global, "herb_shrub_low", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("herb_shrub_low (m2)") + theme_bw()
pred_shrubland <- visreg(m_global, "shrubland", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("shrubland (m2)") + theme_bw()
pred_mixedwood_regen <- visreg(m_global, "mixedwood_regen", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Mixedwood Regenerating (m2)") + theme_bw()
pred_mixedwood_medhigh <- visreg(m_global, "mixedwood_medhigh", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Mixedwood Med-High (m2)") + theme_bw()


# Seismic and Roads and Pipelines
pred_seis_line <- visreg(m_global, "seismic_linetrail_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Seismic Line Distance (m)") + theme_bw()
pred_roads_dist <- visreg(m_global, "roads_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Roads Distance (m)") + theme_bw() 
pred_wellsites_dist <- visreg(m_global, "wellsites_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Wellsites Distance (m)") + theme_bw()
pred_seis_grid_dist <- visreg(m_global, "seismic_grid_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Seismic Grid Distance (m)") + theme_bw()
pred_transmission_dist <- visreg(m_global, "transmission_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Transmission Line Distance (m)") + theme_bw() 
pred_pipeline_dist <- visreg(m_global, "pipeline_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Pipeline Distance (m)") + theme_bw() 

# Cultivated has decent plots but I don't want to focus on it
pred_cultivated_low <- visreg(m_global, "cultivated_low", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cultivated Low Density (m2)") + theme_bw() 
pred_cultivated_med <- visreg(m_global, "cultivated_med", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cultivated Medium Density (m2)") + theme_bw() 
pred_cultivated_high <- visreg(m_global, "cultivated_high", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cultivated High Density (m2)") + theme_bw() 

# Cutblocks
pred_cutblocks_pre97 <- visreg(m_global, "cutblocks_pre97", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cutblocks pre-1997 (m2)") + theme_bw()
pred_cutblocks_97to11 <- visreg(m_global, "cutblocks_97to11", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cutblocks 1997-2011 (m2)") + theme_bw()
pred_cutblocks_post11 <- visreg(m_global, "cutblockspost11", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cutblocks post-2011 (m2)") + theme_bw()


# Change plot visualization
# test <- ggplot_build(a)
# test$data[[3]]$colour <- "red"
# test$data[[4]]$colour <- "red"
# test <- ggplot_gtable(test)
# plot(test)
# 
# test <- ggplot_build(a)
# test$data[[3]]$alpha <- 0.5
# test$data[[4]]$alpha <- 0.5
# test <- ggplot_gtable(test)
# plot(test)



visreg_human_features <- plot_grid(pred_seis_line, pred_seis_grid_dist, pred_wellsites_dist, pred_cutblocks_97to11, nrow = 1)
visreg_natural_features <- plot_grid(pred_broadleaf_regen, pred_conifer_medhigh, pred_wetland, pred_lakes_dist, nrow = 1)
# Add pred_conifer_low? It's a good plot!

save_plot("visreg_human_features.jpeg", visreg_human_features, base_width = 10, base_height = 4)
save_plot("visreg_natural_features.jpeg", visreg_natural_features, base_width = 10, base_height = 4)

# All the plots --
visreg_natural_features_1 <- plot_grid(pred_broadleaf_regen, pred_conifer_medhigh, pred_wetland, pred_lakes_dist, nrow = 1)
visreg_natural_features_2 <- plot_grid(pred_broadleaf_regen, pred_broadleaf_medhigh, pred_conifer_low, pred_conifer_medhigh, nrow = 1)
visreg_natural_features_3 <- plot_grid(pred_mixedwood_regen , pred_mixedwood_medhigh, pred_wetland, pred_herb_shrub_low, pred_shrubland, pred_grassland, nrow = 2) 
visreg_lakes_rivers <- plot_grid(pred_lakes_dist, pred_rivers_dist)

visreg_seismic_roads <- plot_grid(pred_seis_line, pred_seis_grid_dist, pred_wellsites_dist, pred_roads_dist, pred_pipeline_dist, 
                                  pred_transmission_dist, nrow = 2)
visreg_cultivation_cutblocks <- plot_grid(pred_cutblocks_pre97, pred_cutblocks_97to11, pred_cutblocks_post11, 
                                          pred_cultivated_low, pred_cultivated_med, pred_cultivated_high, nrow = 2)

save_plot("visreg_natural_features_1.jpeg", visreg_natural_features_1, base_width = 10, base_height = 4)
save_plot("visreg_natural_features_2.jpeg", visreg_natural_features_2, base_width = 10, base_height = 4)
save_plot("visreg_natural_features_3.jpeg", visreg_natural_features_3, base_width = 10, base_height = 8)
save_plot("visreg_lakes_rivers.jpeg", visreg_lakes_rivers, base_width = 7, base_height = 4)

save_plot("visreg_seismic_roads.jpeg", visreg_seismic_roads, base_width = 10, base_height = 8)
save_plot("visreg_cultivation_cutblocks.jpeg", visreg_cultivation_cutblocks, base_width = 10, base_height = 8)

save_plot("pred_seis_line.jpeg", pred_seis_line, base_width = 4, base_height = 4)

nrow(deer_data_loc_sf)
nrow(deer_data)
unique(deer_data$Point_ID) %>% length()
unique(deer_data_loc_sf$Point_ID) %>% length()

write_csv(deer_data, "deer_data_export_May5_1.csv")

deer_export <- deer_data_loc_sf
crs(deer_export)
st_write(deer_export, "deerloc_export_May5.shp")


### Part 4 - Spatial Autocorrelation in Residuals -----

# Add spatial data in lat/long
real_deer_table <- as_tibble(real_deer) %>% dplyr::select(-geometry)
random_deer_table <- as_tibble(random_deer) %>% dplyr::select(-geometry)

deer_loc_table <- bind_rows(real_deer_table, random_deer_table)

plot(deer_loc_table$Easting, m_global$residuals)
plot(deer_loc_table$Northing, m_global$residuals)

###Make a bubble plot of the residuals for model0 versus the spatial coordinates:
resid_m_global =  m_global$residuals
p <- ggplot(deer_loc_table) + theme_minimal() +
  geom_point(aes(Easting, Northing, size = abs(resid_m_global), shape = as.character(sign(resid_m_global)),
                 colour = as.character(sign(resid_m_global)))) +
  scale_shape_manual(values = c(1, 16)) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  coord_fixed() +
  scale_size_continuous(range = c(0, 5)) +
  labs(colour = "residual sign", shape = "residual sign",
       size = "residual magnitude")
print(p)
##It worked. Well.

####Calculate Moran's I.
##We can first generate a distance matrix, then take inverse of the matrix values and replace the diagonal entries with zero:

## Slow step - commenting out so I don't accidentally run it again.
# deer.dists <- as.matrix(dist(cbind(deer_loc_table$Easting, deer_loc_table$Northing)))
# Huge table - don't enter!

deer.dists.inv <- 1/deer.dists  # Also a huge table
diag(deer.dists.inv) <- 0

deer.dists.inv[1:5, 1:5]  # Subset

### We have created a matrix where each off-diagonal entry [i, j] in the matrix is equal to 1/(distance between point i and point j). 
# Note that this is just one of several ways in which we can calculate an inverse distance matrix. This is the formulation used by Stata. 
# In SAS, inverse distance matrices have entries equal to 1/(1+ distance between point i and point j) and there are numerous scaling options available.
### We can now calculate Moran’s I using the command Moran.I. (where Month.1 is the number of deer months in the new dataframe Locations.
library(ape)

nrow(deer_loc_table)
nrow(deer.dists.inv)

?Moran.I

# Moran's I needs a vector for "x" (first argument) - can't use the entire deer_loc_table dataset, so subseting single columns and seeing if there are differences
Moran.I(deer_loc_table$Easting, deer.dists.inv)
## $observed
# [1] 0.5957886
## $expected
# [1] -6.834803e-05
## $sd
# [1] 0.0003548655
## $p.value
# [1] 0

Moran.I(deer_loc_table$Northing, deer.dists.inv)
## $observed
# [1] 0.5769335
## $expected
# [1] -6.834803e-05
## $sd
# [1] 0.0003548754
## $p.value
# [1] 0

Moran.I(deer_loc_table$OBJECTID, deer.dists.inv)
## $observed
# [1] 0.01645882
## $expected
# [1] -6.834803e-05
## $sd
# [1] 0.000354896
## $p.value
# [1] 0

Moran.I(deer_loc_table$Latitude, deer.dists.inv)

# p-value consistently 0.
### Based on these results, we can reject the null hypothesis that there is zero spatial autocorrelation present in the variable at alpha = .05.

### Test plotting just the deer locations residuals so the random locations don't clutter over top of the map
resid_m_global =  m_global$residuals
test_plot <- ggplot(deer_loc_table) + theme_minimal() +
  geom_point(aes(Easting, Northing, size = abs(resid_m_global), shape = as.character(sign(resid_m_global)), alpha = as.character(sign(resid_m_global)),
                 colour = as.character(sign(resid_m_global)))) +
  scale_shape_manual(values = c(1, 16)) +
  scale_colour_manual(values = c("steelblue", "darkorange")) +
  scale_alpha_manual(values = c(0, 1)) +
  coord_fixed() +
  scale_size_continuous(range = c(0, 5)) +
  labs(colour = "residual sign", shape = "residual sign",
       size = "residual magnitude")
print(test_plot)

# Re-run Moran.I with just used deer locations - I don't know if Moran.I needs the random locations
deer.dists <- as.matrix(dist(cbind(real_deer_table$Easting, real_deer_table$Northing)))
deer.dists.inv <- 1/deer.dists  # Also a huge table
diag(deer.dists.inv) <- 0
library(ape)
Moran.I(real_deer_table$Latitude, deer.dists.inv)
Moran.I(real_deer_table$Longitude, deer.dists.inv)
# P value still equals zero 


### SECTION 21 - Re-Run analysis with updated Roads Distances -------
backup_deer_merge <- deer_merge 

# Investigate NAs in roads
which(is.na(deer_merge$`Roads Distance (m)`))
plot(deer_merge$`Roads Distance (m)`)

deer_merge_used <- filter(deer_merge, Use == 1) 
deer_merge_available <- filter(deer_merge, Use == 0) 

which(is.na(deer_merge_available$`Roads Distance (m)`))
which(is.na(deer_merge_used$`Roads Distance (m)`))

# Which deer locations are greater than 10 km?
deer_data_used <- filter(deer_data, Use == 1) 
deer_data_available <- filter(deer_data, Use == 0) 

filter(deer_data_used, roads_dist_new > 10000)
filter(deer_data_available, roads_dist_new > 10000)

head(deer_data$roads_dist_new)
head(deer_data$roads_dist)
tail(deer_data$roads_dist_new)
tail(deer_data$roads_dist)

deer_merge <- deer_merge %>% rename(roads_distance = `Roads Distance (m)`)
test <- deer_merge %>% mutate(roads_distance = ifelse(Use == 1 & is.na(roads_distance), 10000, roads_distance))
test %>% filter(Use ==1) %>% is.na() %>% which()
test %>% filter(Use ==0) %>% is.na() %>% which()  # This is saying roads has NAs
deer_merge %>% filter(Use ==0, roads_distance > 0) %>% range(.$roads_distance)

ggplot(deer_merge) + 
  geom_point(aes(x = Use, y = roads_distance)) + ylim(0, 11000) + theme_bw()
ggplot(deer_data) + 
  geom_point(aes(x = Use, y = roads_dist)) + ylim(0, 16000) + theme_bw()


### Load updated distances table 
# Ran distance table in ArcMap, from all ABMI roads to deer locations
new_table <- read_csv("deer_roads_dist_newMay12.csv") 
new_table <- new_table %>% rename(roads_dist_new = NEAR_DIST)
roads_table <- new_table %>% dplyr::select(Pont_ID, roads_dist_new)
roads_table <- roads_table %>% rename(Point_ID = Pont_ID)

deer_data <- left_join(deer_data, roads_table, by = "Point_ID")


### Investigate new roads data
which(is.na(deer_data$roads_dist_new))
range(deer_data$roads_dist_new)  # 0 to 19493

p.roads_dist_new <- ggplot(deer_data) + 
  geom_point(aes(x = Use, y = roads_dist_new)) + ylim(0, 16000) + theme_bw()
# MUCH better plot 
p.roads_dist_old <- ggplot(deer_data) + 
  geom_point(aes(x = Use, y = roads_dist)) + ylim(0, 16000) + theme_bw()

library(cowplot)
plot_grid(p.roads_dist_old, p.roads_dist_new) %>% save_plot("roads_distances_old_new_comparison.jpeg", ., base_width = 8, base_height = 8)



#### START HERE FOR MODELS WITH NEW ROADS DISTANCES, MERGED CULTIVATION, AND NO MIXEDWOOD MED-HIGH ---------
### SECTION 22 - Re-Run analysis with Merged Cultivation and updated Roads Distances -------

backup_deer_merge <- deer_merge 

# Merge all cultivation
deer_data$cultivation_NRCAN_summed

# Confirm this is identical to the three cultivation values summed
test <- deer_data
test <- test %>% mutate(cultivation_total = cultivated_high + cultivated_med + cultivated_low)
identical(test$cultivation_total, test$cultivation_NRCAN_summed)

# Rename cultivation column for clarity
deer_data <- deer_data %>% rename(cultivation_total = cultivation_NRCAN_summed)

### Test for correlation and VIF again -----
library(Hmisc)
coefficients(m_global)
m_global_variable_list <- c("grassland","cultivation_total", "herb_shrub_low", "shrubland", "broadleaf_regen", 
                            "broadleaf_medhigh", "mixedwood_regen", "conifer_low", "conifer_medhigh", "wetland", 
                            "rivers_dist", "lakes_dist", "roads_dist", "seismic_grid_dist", "seismic_linetrail_dist", "pipeline_dist", 
                            "transmission_dist", "wellsites_dist", "cutblocks_pre97", "cutblocks_97to11", "cutblockspost11")

# Note: must use rcorr to produce class "rcorr" correlation matrix for this to work. Don't use cor() function. 
cor_m_global_variable_list <- rcorr(as.matrix((deer_data[, c(m_global_variable_list)])))
cor_m_global_variable_list_tbl <- flattenCorrMatrix(cor_m_global_variable_list$r, cor_m_global_variable_list$P) %>% as_tibble()

# Select all values greater than abs 0.2, and round to 3 digits
cor_m_global_variables_greater_point2 <- filter(cor_m_global_variable_list_tbl, cor >= 0.2 | cor <= -0.2)
cor_m_global_variables_greater_point2 <- cor_m_global_variables_greater_point2[order(cor_m_global_variables_greater_point2$cor), ]
cor_m_global_variables_greater_point2 <- cor_m_global_variables_greater_point2 %>% mutate_if(is.numeric, round, digits = 3)

cor_m_global_variable_list_tbl %>% arrange(desc(row)) %>% print(n = nrow(.))
cor_m_global_variable_list_tbl %>% arrange(desc(column)) %>% print(n = nrow(.))

# Vif
print(vif(deer_data[, c(m_global_variable_list)]))
# Removing cultivated_low resolves this...



###  Models -------

# Global model without mixedwood
m_global <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen 
                + conifer_low + conifer_medhigh + wetland + cultivation_total +
                + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist
                + seismic_linetrail_dist + pipeline_dist + transmission_dist + wellsites_dist, 
                family = "binomial", data = deer_data, na.action ="na.fail")

plot(m_global)


m_natural <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + mixedwood_medhigh + conifer_low + 
                   conifer_medhigh + wetland + lakes_dist + rivers_dist, family = "binomial", data = deer_data, na.action ="na.fail")

m_block <- glm(Use ~ cultivation_total + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11,  
               family = "binomial", data = deer_data, na.action ="na.fail")

m_linear <- glm(Use ~ roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + 
                  pipeline_dist + transmission_dist + wellsites_dist, family = "binomial", data = deer_data, na.action ="na.fail")

m_linear_block <- glm(Use ~ cultivation_total + roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + 
                        pipeline_dist + transmission_dist + wellsites_dist + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11, 
                      family = "binomial", data = deer_data, na.action ="na.fail")

m_natural_block <-  glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low + 
                          conifer_medhigh + wetland + lakes_dist + rivers_dist + cultivation_total + 
                          cutblocks_pre97 + cutblocks_97to11 + cutblockspost11, family = "binomial", data = deer_data, na.action ="na.fail")

m_natural_linear <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low + 
                          conifer_medhigh + wetland + lakes_dist + rivers_dist +  roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                          transmission_dist + wellsites_dist, family = "binomial", data = deer_data, na.action ="na.fail")

m_linear_plus_distance_riparian <- glm(Use ~  roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                                         transmission_dist + wellsites_dist + lakes_dist + rivers_dist, family = "binomial", data = deer_data, na.action ="na.fail")

## AIC Scores
model_list <- list(m_global,
                   m_natural,
                   m_block,
                   m_linear,
                   m_linear_block,
                   m_natural_block,
                   m_natural_linear,
                   m_linear_plus_distance_riparian)

names(model_list) <- c("global", "natural features", "block features", 
                       "linear features", "linear and block features",  "natural and block features", "natural and linear features", "linear and riparian features")

AIC_scores <- aictab(model_list)
kable(AIC_scores)
stargazer(AIC_scores, summary = FALSE, type = "text", out = "AIC_scores_global_models_new_cultivation_Mar08.html")

# Coefficients - Global Model
tidy(m_global) %>% by_2sd(deer_data) %>% mutate_if(is.numeric, round, digits = 3) %>% stargazer(., summary = FALSE, type = "text", out = "coefficients_global_model.html")


### Coefficient Plot
dw_global <- dwplot(m_global, whisker_args = aes(color = "royalblue2", size = 0.75, alpha = 0.65), dot_args = 
                      aes(color = "royalblue3", size = 2, shape = 21, fill = "dodgerblue2")) %>% 
  relabel_predictors(c(grassland = "Grassland",
                       herb_shrub_low = "Herb-Shrub Low",
                       shrubland = "Shrubland",
                       broadleaf_regen = "Broadleaf, Regenerating",
                       broadleaf_medhigh = "Broadleaf, Medium to High Density",
                       mixedwood_regen = "Mixedwood, Regenerating",
                       conifer_low = "Conifer, Low Density",
                       conifer_medhigh = "Conifer, Medium to High Density",
                       wetland = "Wetlands",
                       rivers_dist = "Rivers Distance", 
                       lakes_dist = "Lakes Distance",
                       cultivation_total = "Cultivation",
                       cutblocks_pre97 = "Cutblocks pre-1997", 
                       cutblocks_97to11 = "Cutblocks 1997 to 2011", 
                       cutblockspost11 = "Cutblocks post-2011",
                       roads_dist_new = "Roads Dist",
                       seismic_linetrail_dist = "Sesimic Lines Dist",
                       seismic_grid_dist = "Seismic Grids Dist",
                       pipeline_dist = "Pipelines Dist", 
                       transmission_dist = "Transmission Lines Dist", 
                       wellsites_dist = "Wellsites Dist")) +
  theme_bw() + xlab("Strength of selection / Effect Size") +
  ggtitle("Coefficient Estimates for Global RSF Model") + 
  geom_vline(xintercept = 0, colour = "grey65", linetype = 2) + 
  theme(plot.title = element_text(size = 11)) + 
  theme(legend.position = "none") + xlim(-1.5, 1.5)

# Change sign of distance variables on the main plot
test <- ggplot_build(dw_global)

test$data[[2]] <- test$data[[2]] %>% mutate(x = ifelse(y == 11 | y == 12 | y >= 1 & y <= 6, x*(-1), x))

test$data[[1]] <- test$data[[1]] %>% mutate(xmin = ifelse(y == 11 | y == 12 | y >= 1 & y <= 6, xmin*(-1), xmin)) %>% 
  mutate(xmax = ifelse(y == 11 | y == 12 | y >= 1 & y <= 6, xmax*(-1), xmax))

test <- ggplot_gtable(test)
plot(test)

p.global_coefficients <- test

plot_grid(p.global_coefficients_former, p.global_coefficients) %>% save_plot("global_coefficients_comparison.jpeg", ., base_width = 14, base_height = 8)
plot_grid(p.global_coefficients) %>% save_plot("global_coefficients.jpeg", ., base_width = 8, base_height = 8)

### Coefficient table for global model ----
# coefficient_table <- m_global %>% tidy() %>% by_2sd(deer_data) %>% print(n = nrow(.)) %>% mutate_if(is.numeric, round, digits = 3)
# stargazer(coefficient_table, summary = FALSE, type = "html", out = "coefficients_global_model_new.csv")


### Part 3 - Prediction Plotting -----
pred_roads_dist <- visreg(m_global, "roads_dist_new", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Distance Roads (km)") + theme_bw() + scale_x_reverse(labels = function(x) format(x/1000))
pred_lakes_dist <- visreg(m_global, "lakes_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Distance Lakes (km)") + theme_bw() + scale_x_reverse(labels = function(x) format(x/1000))
pred_seis_line <- visreg(m_global, "seismic_linetrail_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Distance Seismic Lines (km)") + theme_bw() + scale_x_reverse(labels = function(x) format(x/1000))
pred_seis_grid_dist <- visreg(m_global, "seismic_grid_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Distance Seismic Grids (km)") + theme_bw() + scale_x_reverse(labels = function(x) format(x/1000))

pred_broadleaf_regen <- visreg(m_global, "broadleaf_regen", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Regenerating Broadleaf Area (km2)") + theme_bw() + scale_x_continuous(labels = function(x) format(x/1000000))
# pred_rivers_dist <- visreg(m_global, "rivers_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
#   xlab("Rivers Distance (m)") + theme_bw()
pred_conifer_medhigh <- visreg(m_global, "conifer_medhigh", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Coniferous Med-High Density (km2)") + theme_bw() + scale_x_continuous(labels = function(x) format(x/1000000))
# pred_conifer_low <- visreg(m_global, "conifer_low", scale = "response", ylim = c(0,1), gg=TRUE) +
#   xlab("Coniferous Low Density (m2)") + theme_bw()
pred_wetland <- visreg(m_global, "wetland", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Wetland (km2)") + theme_bw() + scale_x_continuous(labels = function(x) format(x/1000000))
pred_cultivation_total <- visreg(m_global, "cultivation_total", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cultivation Area (km2)") + theme_bw() + scale_x_continuous(labels = function(x) format(x/1000000))

# pred_cutblocks_pre97 <- visreg(m_global, "cutblocks_pre97", scale = "response", ylim = c(0,1), gg=TRUE) +
#   xlab("Cutblocks pre-1997 (m2)") + theme_bw()
# pred_cutblocks_97to11 <- visreg(m_global, "cutblocks_97to11", scale = "response", ylim = c(0,1), gg=TRUE) +
#   xlab("Cutblocks 1997-2011 (m2)") + theme_bw()
# pred_cutblocks_post11 <- visreg(m_global, "cutblockspost11", scale = "response", ylim = c(0,1), gg=TRUE) +
#   xlab("Cutblocks post-2011 (m2)") + theme_bw()

# p.predict_natural_features <- plot_grid(pred_broadleaf_regen, pred_lakes_dist, pred_conifer_medhigh, pred_wetland, nrow = 1)
# save_plot("p.predict_natural_features.jpeg", p.predict_natural_features, base_width = 10)
# 
# p.predict_human_features <- plot_grid(pred_cultivation_total, pred_seis_line, pred_seis_grid_dist, pred_roads_dist, nrow = 1)
# save_plot("p.predict_human_features.jpeg", p.predict_human_features, base_width = 10)
# 
# p.predict_all_features <- plot_grid(pred_broadleaf_regen, pred_lakes_dist, pred_conifer_medhigh, pred_wetland, pred_cultivation_total, 
#                                     pred_seis_line, pred_seis_grid_dist, pred_roads_dist, nrow = 2, 
#                                     labels = c("a", "b", "c", "d", "e", "f", "g", "h"))
# save_plot("p.predict_all_features.jpeg", p.predict_all_features, base_width = 8, base_height = 12)


### Reconfigure plot --
p.predict_all_features <- plot_grid(pred_broadleaf_regen, pred_conifer_medhigh, pred_wetland, pred_cultivation_total, 
                                    pred_lakes_dist, pred_seis_line, pred_seis_grid_dist, pred_roads_dist, nrow = 4, ncol = 2,
                                    labels = c("a", "b", "c", "d", "e", "f", "g", "h"), hjust = -1.5)
save_plot("p.predict_all_featuresMar09.jpeg", p.predict_all_features, base_width = 8, base_height = 12)



### Part 4 - Spatial Autocorrelation in Residuals -----

# Add spatial data in lat/long
real_deer_table <- as_tibble(real_deer) %>% dplyr::select(-geometry)
random_deer_table <- as_tibble(random_deer) %>% dplyr::select(-geometry)
deer_loc_table <- bind_rows(real_deer_table, random_deer_table)

plot(deer_loc_table$Easting, m_global$residuals)
plot(deer_loc_table$Northing, m_global$residuals)


### Plot just the deer locations residuals so the random locations don't clutter over top of the map
resid_m_global =  m_global$residuals
test_plot <- ggplot(deer_loc_table) + theme_minimal() +
  geom_point(aes(Easting, Northing, size = abs(resid_m_global), shape = as.character(sign(resid_m_global)), alpha = as.character(sign(resid_m_global)),
                 colour = as.character(sign(resid_m_global)))) +
  scale_shape_manual(values = c(1, 16)) +
  scale_colour_manual(values = c("steelblue", "darkorange")) +
  scale_alpha_manual(values = c(0, 1)) +
  coord_fixed() +
  scale_size_continuous(range = c(0, 5)) +
  labs(colour = "residual sign", shape = "residual sign",
       size = "residual magnitude")
print(test_plot)

save_plot("residual_spatial_correlation_no_latlong.jpeg", test_plot, base_height = 8, base_width = 8)

# Slow step -
deer.dists <- as.matrix(dist(cbind(deer_loc_table$Easting, deer_loc_table$Northing)))
deer.dists.inv <- 1/deer.dists  # Also a huge table
diag(deer.dists.inv) <- 0
library(ape)

Moran.I(m_global$residuals, deer.dists.inv)

# Add scaled northing and scaled easting to global model
# Once that variance is accounted for, hopefully all other variables remain important
# Re-test for auto-correlation in the global model 
# Re-plot the coefficients and hopefully they are spread 
# Apply to all models

Moran.I(deer_loc_table$Longitude, deer.dists.inv)
Moran.I(deer_loc_table$Longitude, deer.dists.inv)
Moran.I(deer_loc_table$OBJECTID, deer.dists.inv)

# Re-run Moran.I with just used deer locations - I don't know if Moran.I needs the random locations
deer.dists <- as.matrix(dist(cbind(real_deer_table$Easting, real_deer_table$Northing)))
deer.dists.inv <- 1/deer.dists  # Also a huge table
diag(deer.dists.inv) <- 0
library(ape)
Moran.I(real_deer_table$Longitude, deer.dists.inv)
Moran.I(real_deer_table$Longitude, deer.dists.inv)
Moran.I(real_deer_table$OBJECTID, deer.dists.inv)
# P value still equals zero 




### Part 5 - Model Testing ---------

### Deviance Explained, and Dispersion ----
names(model_list)

## Register function for adjusted D-squared, following Franklin SDM book p121:
aDsq <- function(x) (1-(summary(x)$df.null/summary(x)$df.residual)*
                       (1-(summary(x)$null.deviance - summary(x)$deviance)/summary(x)$null.deviance))*100

# Deviance Explained 
aDsq(m_global)   # 16.983
aDsq(m_natural_linear)  # 15.754
aDsq(m_natural_block) # 10.804
aDsq(m_natural)  # 9.792
aDsq(m_linear)  # 7.687
aDsq(m_linear_block)  # 9.223
aDsq(m_block)  # 1.693
aDsq(m_linear_plus_distance_riparian)  # 8.775


# Calculate overdispersion
sigma = sum(residuals(m_global, type = "pearson")^2) / (m_global$df.residual)
sum(residuals(m_natural_linear, type = "pearson")^2) / (m_natural_linear$df.residual)
sum(residuals(m_natural_block, type = "pearson")^2) / (m_natural_block$df.residual)
sum(residuals(m_natural, type = "pearson")^2) / (m_natural$df.residual)
sum(residuals(m_block, type = "pearson")^2) / (m_block$df.residual)
sum(residuals(m_linear_block, type = "pearson")^2) / (m_linear_block$df.residual)
sum(residuals(m_linear_plus_distance_riparian, type = "pearson")^2) / (m_linear_plus_distance_riparian$df.residual)
sum(residuals(m_linear, type = "pearson")^2) / (m_linear$df.residual)




### Create spatial blocks ----
?spatialBlock()
?spatialAutoRange()

# Load NRCAN Raster data 
nrcan_lc <- raster("NRCAN_reclass.tif")

# Set projections
projection(nrcan_lc, asText = TRUE)
deer_data_loc_sf <- st_transform(deer_data_loc_sf, crs = 3400)

x11(15, 15)

# Plot raster
tm_shape(nrcan_lc) + 
  tm_raster() + 
  tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 0, ]) + 
  tm_dots(col = "lightblue") +
  tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 1, ]) + 
  tm_dots(col = "grey19")

# tm_shape(nrcan_lc) + 
# tm_raster(palette = pal2, n = 17) + 
# tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 0, ]) + 
# tm_dots(col = "lightblue") +
# tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 1, ]) + 
# tm_dots(col = "grey19")
pal1 <- tmaptools::get_brewer_pal("Accent", n = 17)
pal2 <- tmaptools::get_brewer_pal("Paired", n = 17)

# Explore block range
rangeExplorer(rasterLayer = nrcan_lc,
              speciesData = deer_data_loc_sf, # response data (optional)
              minRange = 30000, # limit the search domain
              maxRange = 100000)
# 32924 looks good. 

# Test spatial blocking
sb_32924 <- spatialBlock(speciesData = deer_data_loc_sf, 
                         species = "Use", 
                         rasterLayer = nrcan_lc, 
                         theRange = 32924, 
                         k = 10, 
                         iteration = 250, 
                         numLimit = 0, 
                         showBlocks = TRUE, 
                         progress = TRUE, 
                         verbose = TRUE)

foldExplorer(sb_32924, nrcan_lc, deer_data_loc_sf)


### Cross Validation Results ----
# Methods follow http://htmlpreview.github.io/?https://github.com/rvalavi/blockCV/blob/master/vignettes/BlockCV_for_SDM.html
# "Evaluating SDMs with block cross-validation: examples -- maxnet"
# Use blocks layer sb_32924

# Create a vector of 1 and 0 for presence and background
Use <- deer_data$Use

# Create a vector of the FoldID for each point in the data 
folds <- sb_32924$foldID 
# length(folds) = 14632 - same as number of deer used and available points

# Create an empty vector to store the AUC of each fold
# AUCs <- vector(mode = "numeric")
AUCs_updated_global <- vector(mode = "numeric")

# Cross validation with 10 blocks - global model 
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen 
                    + conifer_low + conifer_medhigh + wetland + cultivation_total +
                      + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist
                    + seismic_linetrail_dist + pipeline_dist + transmission_dist + wellsites_dist,  family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - natural and linear features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ grassland + herb_shrub_low + shrubland
                    + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low
                    + conifer_medhigh + wetland + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - natural and block features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ grassland + herb_shrub_low + shrubland
                    + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low
                    + conifer_medhigh + wetland + rivers_dist + lakes_dist + cultivated_low + cultivated_med + cultivated_high
                    + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - natural features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ grassland + herb_shrub_low + shrubland
                    + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low
                    + conifer_medhigh + wetland + rivers_dist + lakes_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - block features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ cultivated_low + cultivated_med + cultivated_high
                    + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - linear features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - linear and riparian features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist + lakes_dist + rivers_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))   # 0.6826
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - linear and block features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ cultivated_low + cultivated_med + cultivated_high
                    + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11
                    + roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)



### SECTION 23 - Add Lat/Long or UTM mE mN data ---------

### Join deer data with location data ---- 
left_join(deer_loc_table, deer_data$grassland, by = "Point_ID")
lapply(deer_data, class)
lapply(deer_loc_table, class)
class(deer_data)
class(deer_loc_table)

habitat_vars <- deer_data %>% dplyr::select(1, 3:36)
# class(habitat_vars$Point_ID)
# 
# # backup_deer_data <- deer_data
# # deer_data <- backup_deer_data
# 
# n.deer_data <- left_join(deer_loc_table, habitat_vars, by = "Point_ID")
# 
# ls(deer_data)%in% ls(n.deer_data)  # Everything except Point is present 
# compare(deer_loc_table$Point_ID, deer_data$Point_ID)
# # FALSE
# 
# list <- n.deer_data$broadleaf_regen %in% deer_data$broadleaf_regen
# which(list == FALSE) %>% length()
# list <- n.deer_data$Point_ID %in% deer_data$Point_ID
# which(list == FALSE) %>% length()
# list <- n.deer_data$broadleaf_medhigh %in% deer_data$broadleaf_medhigh
# which(list == FALSE) %>% length()
# list <- n.deer_data$cultivation_total %in% deer_data$cultivation_total
# which(list == FALSE) %>% length()
# compare(n.deer_data$cultivation_total, deer_data$cultivation_total)
# # There is a problem with %in% the way I'm using it.
# 
# 
# list <- n.deer_data$cultivation_total %in% deer_data$Point_ID
# which(list == FALSE) %>% length()
# 
# # Test to see if old and new variables are identical
# habitat_vars <- deer_data %>% dplyr::select(4:36) 
# all_variables <- n.deer_data %>% dplyr::select(7:39)
# ls(habitat_vars) %in% ls(all_variables) # TRUE - all column names the same in original data columns to join, and columns extracted from the joined data  
# 
# deer_data[, cultiation_NRCAN_summed]
# 
# # Create an empty list
# list <- vector(mode = "list", length = 34)
# 
# for(i in 1:length(habitat_vars)){
#   old_variable <- habitat_vars %>% dplyr::select(i)
#   old_variable_list <- pull(old_variable[, 1])
#   new_variable <- all_variables %>% dplyr::select(i)
#   new_variable_list <- pull(new_variable[, 1])
#   list[i] <- compare(new_variable_list, old_variable_list)
# }
# 
# identical(new_variable_list, old_variable_list)
# habitat_vars$test %in% all_variables$test
# 
# test1 <- pull(habitat_vars[, 33])
# test2 <- pull(all_variables[, 33])
# 
# identical(test1, test2)  # identical() sucks
# # install.packages("compare")
# library(compare)
# compare(test1, test2, coerce = TRUE, round = 2, ignoreAttrs = TRUE, equal = TRUE)
# 
# true_false <- test1 %in% test2
# which(true_false == FALSE) %>% length()
# test1[12163]
# test2[12163]
# # Definitely not identical...
# 
# habitat_vars$test <- 23
# all_variables$test <- 23
# 
# # When I take the correlation coefficient of the data in habitat_vars and deer_data, the values are different from n.deer_data
# # even though n.deer_data is habitat_vars joined with deer locations. WTF?
# 
# 

### There is a problem with Point_ID in deer_loc_table
# No shit. Because it's not OBJECTID
deer_loc_table <- deer_loc_table %>% rename(Object_ID = Point_ID)

left_join(deer_data, deer_data_loc$Latitude, by = "Point_ID")
# This. 

deer_lat_long <- deer_data_loc %>% dplyr::select(Point_ID, Latitude, Longitude)
left_join(deer_data, deer_lat_long, by = "Point_ID")
# Ah. The earlier data didn't have Point_ID. Fraaaaack.
identical(backup_deer_data, deer_data)

n.deer_data <- left_join(deer_data, deer_lat_long, by = "Point_ID")
# We have liftoff


### Test for correlation and VIF  -----
library(Hmisc)
coefficients(m_global)
m_global_variable_list <- c("grassland","cultivation_total", "herb_shrub_low", "shrubland", "broadleaf_regen", 
                            "broadleaf_medhigh", "mixedwood_regen", "conifer_low", "conifer_medhigh", "wetland", 
                            "rivers_dist", "lakes_dist", "roads_dist", "seismic_grid_dist", "seismic_linetrail_dist", "pipeline_dist", 
                            "transmission_dist", "wellsites_dist", "cutblocks_pre97", "cutblocks_97to11", "cutblockspost11", "Latitude", "Longitude")

m_global_variable_list <- c("grassland","cultivation_total", "herb_shrub_low", "shrubland", "broadleaf_regen", 
                            "broadleaf_medhigh", "mixedwood_regen", "conifer_low", "conifer_medhigh", "wetland", 
                            "rivers_dist", "lakes_dist", "roads_dist", "seismic_grid_dist", "seismic_linetrail_dist", "pipeline_dist", 
                            "transmission_dist", "wellsites_dist", "cutblocks_pre97", "cutblocks_97to11", "cutblockspost11")

# Note: must use rcorr to produce class "rcorr" correlation matrix for this to work. Don't use cor() function. 
cor_m_global_variable_list <- rcorr(as.matrix((n.deer_data[, c(m_global_variable_list)])))
cor_m_global_variable_list_tbl <- flattenCorrMatrix(cor_m_global_variable_list$r, cor_m_global_variable_list$P) %>% as_tibble()

# Select all values greater than abs 0.2, and round to 3 digits
cor_m_global_variables_greater_point2 <- filter(cor_m_global_variable_list_tbl, cor >= 0.2 | cor <= -0.2)
cor_m_global_variables_greater_point2 <- cor_m_global_variables_greater_point2[order(cor_m_global_variables_greater_point2$cor), ]
cor_m_global_variables_greater_point2 <- cor_m_global_variables_greater_point2 %>% mutate_if(is.numeric, round, digits = 3)

cor_m_global_variable_list_tbl %>% arrange(desc(row)) %>% print(n = nrow(.))
cor_m_global_variable_list_tbl %>% arrange(desc(column)) %>% print(n = nrow(.))

# Vif - faraway 
print(faraway::vif(n.deer_data[, c(m_global_variable_list)]))
# <Note> Lat and Long not strongly correlated with other variables. Good.

# Vif - Car 
install.packages("car")
library(car)
car::vif(m_global)




#### ACTUALLY START HERE? #### 
### These include LAT and LONG ----
###  Models -------

# Global model without mixedwood
m_global <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen 
                + conifer_low + conifer_medhigh + wetland + cultivation_total +
                  + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist
                + seismic_linetrail_dist + pipeline_dist + transmission_dist + wellsites_dist + scale(Latitude) + scale(Longitude), 
                family = "binomial", data = n.deer_data, na.action ="na.fail")

m_global_unscaled <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen 
                + conifer_low + conifer_medhigh + wetland + cultivation_total +
                  + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist
                + seismic_linetrail_dist + pipeline_dist + transmission_dist + wellsites_dist + Latitude + Longitude, 
                family = "binomial", data = n.deer_data, na.action ="na.fail")


m_natural <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + mixedwood_medhigh + conifer_low + 
                   conifer_medhigh + wetland + lakes_dist + rivers_dist + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

m_block <- glm(Use ~ cultivation_total + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + scale(Latitude) + scale(Longitude),  
               family = "binomial", data = n.deer_data, na.action ="na.fail")

m_linear <- glm(Use ~ roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + 
                  pipeline_dist + transmission_dist + wellsites_dist + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

m_linear_block <- glm(Use ~ cultivation_total + roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + 
                        pipeline_dist + transmission_dist + wellsites_dist + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + scale(Latitude) + scale(Longitude), 
                      family = "binomial", data = n.deer_data, na.action ="na.fail")

m_natural_block <-  glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low + 
                          conifer_medhigh + wetland + lakes_dist + rivers_dist + cultivation_total + 
                          cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

m_natural_linear <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low + 
                          conifer_medhigh + wetland + lakes_dist + rivers_dist +  roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                          transmission_dist + wellsites_dist + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

m_linear_plus_distance_riparian <- glm(Use ~  roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                                         transmission_dist + wellsites_dist + lakes_dist + rivers_dist + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

## AIC Scores
model_list <- list(m_global,
                   m_natural,
                   m_block,
                   m_linear,
                   m_linear_block,
                   m_natural_block,
                   m_natural_linear,
                   m_linear_plus_distance_riparian)

names(model_list) <- c("global", "natural features", "block features", 
                       "linear features", "linear and block features",  "natural and block features", "natural and linear features", "linear and riparian features")

AIC_scores <- aictab(model_list)
kable(AIC_scores)
stargazer(AIC_scores, summary = FALSE, type = "text", out = "AIC_scores_global_models_new_cultivation.html")

# Coefficients - Global Model
tidy(m_global) %>% by_2sd(deer_data) %>% mutate_if(is.numeric, round, digits = 3) %>% stargazer(., summary = FALSE, type = "text", out = "coefficients_global_model.html")


### Coefficient Plot
dw_global <- dwplot(m_global, whisker_args = aes(color = "royalblue2", size = 0.75, alpha = 0.65), dot_args = 
                      aes(color = "royalblue3", size = 2, shape = 21, fill = "dodgerblue2")) %>% 
  relabel_predictors(c(grassland = "Grassland",
                       herb_shrub_low = "Herb-Shrub Low",
                       shrubland = "Shrubland",
                       broadleaf_regen = "Broadleaf, Regenerating",
                       broadleaf_medhigh = "Broadleaf, Medium to High Density",
                       mixedwood_regen = "Mixedwood, Regenerating",
                       conifer_low = "Conifer, Low Density",
                       conifer_medhigh = "Conifer, Medium to High Density",
                       wetland = "Wetlands",
                       rivers_dist = "Rivers Distance", 
                       lakes_dist = "Lakes Distance",
                       cultivation_total = "Cultivation",
                       cutblocks_pre97 = "Cutblocks pre-1997", 
                       cutblocks_97to11 = "Cutblocks 1997 to 2011", 
                       cutblockspost11 = "Cutblocks post-2011",
                       roads_dist_new = "Roads Dist",
                       seismic_linetrail_dist = "Sesimic Lines Dist",
                       seismic_grid_dist = "Seismic Grids Dist",
                       pipeline_dist = "Pipelines Dist", 
                       transmission_dist = "Transmission Lines Dist", 
                       wellsites_dist = "Wellsites Dist")) +
  theme_bw() + xlab("Strength of selection / Effect Size") +
  ggtitle("Coefficient Estimates for the Global RSF Model \n with Latitude and Longitude") + 
  geom_vline(xintercept = 0, colour = "grey65", linetype = 2) + 
  theme(plot.title = element_text(size = 11)) + 
  theme(legend.position = "none")

# Change sign of distance variables on the main plot
test <- ggplot_build(dw_global)

test$data[[2]] <- test$data[[2]] %>% mutate(x = ifelse(y == 13 | y == 14 | y <= 8 & y > 2, x*(-1), x))

test$data[[1]] <- test$data[[1]] %>% mutate(xmin = ifelse(y == 13 | y == 14 | y <= 8 & y > 2, xmin*(-1), xmin)) %>% 
  mutate(xmax = ifelse(y == 13 | y == 14 | y <= 8 & y > 2, xmax*(-1), xmax))

test <- ggplot_gtable(test)
plot(test)

p.global_coefficients <- test
plot(p.global_coefficients)

save_plot("dwplot_global_coefficients_Mar10.jpg", p.global_coefficients, base_height = 8, base_width = 6)

plot_grid(p.global_coefficients_former, p.global_coefficients) %>% save_plot("global_coefficients_comparison.jpeg", ., base_width = 14, base_height = 8)
plot_grid(p.global_coefficients) %>% save_plot("global_coefficients_withlat_scaled.jpeg", ., base_width = 8, base_height = 8)

### Coefficient table for global model ----
# coefficient_table <- m_global %>% tidy() %>% by_2sd(deer_data) %>% print(n = nrow(.)) %>% mutate_if(is.numeric, round, digits = 3)
# stargazer(coefficient_table, summary = FALSE, type = "html", out = "coefficients_global_model_new.csv")


### Part 3 - Prediction Plotting -----

# Functions for changing x scales 
formatter1000 <- function(x){ 
  x/1000 
}

formatter1000000 <- function(x){ 
  x/1000000 
}

pred_roads_dist <- visreg(m_global, "roads_dist_new", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Roads Distance (km)") + theme_bw() + scale_x_continuous(labels = formatter1000)
# plot_grid(pred_roads_dist, pred_roads_dist_new) %>% save_plot("roads_visreg_predictions_old_new_comparison.jpeg", ., base_width = 14, base_height = 8)

pred_broadleaf_regen <- visreg(m_global, "broadleaf_regen", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Regenerating Broadleaf Area (km2)") + theme_bw() + scale_x_continuous(labels = formatter1000000)
pred_lakes_dist <- visreg(m_global, "lakes_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Lakes Distance (km)") + theme_bw() + scale_x_continuous(labels = formatter1000)
# pred_rivers_dist <- visreg(m_global, "rivers_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
#   xlab("Rivers Distance (m)") + theme_bw()
pred_conifer_medhigh <- visreg(m_global, "conifer_medhigh", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Coniferous Med-High Density Area (km2)") + theme_bw() + scale_x_continuous(labels = formatter1000000)
pred_conifer_low <- visreg(m_global, "conifer_low", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Coniferous Low Density (km2)") + theme_bw() + scale_x_continuous(labels = formatter1000000)
pred_wetland <- visreg(m_global, "wetland", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Wetland Area (km2)") + theme_bw() + scale_x_continuous(labels = formatter1000000)

pred_seis_line <- visreg(m_global, "seismic_linetrail_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Seismic Line Distance (km)") + theme_bw() + scale_x_continuous(labels = formatter1000)
pred_seis_grid_dist <- visreg(m_global, "seismic_grid_dist", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Seismic Grid Distance (km)") + theme_bw() + scale_x_continuous(labels = formatter1000)

pred_cultivation_total <- visreg(m_global, "cultivation_total", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cultivation Area (km2)") + theme_bw() + scale_x_continuous(labels = formatter1000000) 

pred_cutblocks_pre97 <- visreg(m_global, "cutblocks_pre97", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cutblocks pre-1997 (km2)") + theme_bw() + scale_x_continuous(labels = formatter1000000)
pred_cutblocks_97to11 <- visreg(m_global, "cutblocks_97to11", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cutblocks 1997-2011 (km2)") + theme_bw() + scale_x_continuous(labels = formatter1000000)
pred_cutblocks_post11 <- visreg(m_global, "cutblockspost11", scale = "response", ylim = c(0,1), gg=TRUE) +
  xlab("Cutblocks post-2011 (km2)") + theme_bw() + scale_x_continuous(labels = formatter1000000)

p.predict_natural_features <- plot_grid(pred_broadleaf_regen, pred_lakes_dist, pred_conifer_medhigh, pred_wetland, nrow = 1)
save_plot("p.predict_natural_features.jpeg", p.predict_natural_features, base_width = 10)

p.predict_human_features <- plot_grid(pred_cultivation_total, pred_seis_line, pred_seis_grid_dist, pred_roads_dist, nrow = 1)
save_plot("p.predict_human_features.jpeg", p.predict_human_features, base_width = 10)

p.predict_all_features <- plot_grid(pred_broadleaf_regen, pred_lakes_dist, pred_conifer_medhigh, pred_wetland, 
                                    pred_cultivation_total, pred_seis_line, pred_seis_grid_dist, pred_roads_dist, nrow = 2,
                                      labels = c("a", "b", "c", "d", "e", "f", "g", "h"))
save_plot("p.predict_all_features.jpeg", p.predict_all_features, base_width = 14, base_height = 10)



### Part 4 - Spatial Autocorrelation in Residuals -----

# Add spatial data in lat/long
n.deer_data 
deer_loc_table

### Plot just the deer locations residuals so the random locations don't clutter over top of the map
resid_m_global =  m_global$residuals
test_plot <- ggplot(deer_loc_table) + theme_minimal() +
  geom_point(aes(Easting, Northing, size = abs(resid_m_global), shape = as.character(sign(resid_m_global)), alpha = as.character(sign(resid_m_global)),
                 colour = as.character(sign(resid_m_global)))) +
  scale_shape_manual(values = c(1, 16)) +
  scale_colour_manual(values = c("steelblue", "darkorange")) +
  scale_alpha_manual(values = c(0, 1)) +
  coord_fixed() +
  scale_size_continuous(range = c(0, 5)) +
  labs(colour = "residual sign", shape = "residual sign",
       size = "residual magnitude")
print(test_plot)

save_plot("residual_spatial_correlation_with_latlong.jpeg", test_plot, base_width = 8, base_height = 8)

# Slow step -
deer.dists <- as.matrix(dist(cbind(deer_loc_table$Easting, deer_loc_table$Northing)))
deer.dists.inv <- 1/deer.dists  # Also a huge table
diag(deer.dists.inv) <- 0
library(ape)

Moran.I(m_global$residuals, deer.dists.inv)


deer.dists <- as.matrix(dist(cbind(n.deer_data$Longitude, n.deer_data$Latitude)))
deer.dists.inv <- 1/deer.dists  # Also a huge table
diag(deer.dists.inv) <- 0
library(ape)

Moran.I(m_global$residuals, deer.dists.inv)

# Add scaled northing and scaled easting to global model
# Once that variance is accounted for, hopefully all other variables remain important
# Re-test for auto-correlation in the global model 
# Re-plot the coefficients and hopefully they are spread 
# Apply to all models

Moran.I(deer_loc_table$Longitude, deer.dists.inv)
Moran.I(deer_loc_table$Longitude, deer.dists.inv)
Moran.I(deer_loc_table$OBJECTID, deer.dists.inv)


# Re-run Moran.I with just used deer locations - I don't know if Moran.I needs the random locations
deer.dists <- as.matrix(dist(cbind(real_deer_table$Easting, real_deer_table$Northing)))
deer.dists.inv <- 1/deer.dists  # Also a huge table
diag(deer.dists.inv) <- 0
library(ape)
Moran.I(real_deer_table$Longitude, deer.dists.inv)
Moran.I(real_deer_table$Longitude, deer.dists.inv)
Moran.I(real_deer_table$OBJECTID, deer.dists.inv)
# P value still equals zero 




### Part 5 - Model Testing ---------

### Deviance Explained, and Dispersion ----
names(model_list)

## Register function for adjusted D-squared, following Franklin SDM book p121:
aDsq <- function(x) (1-(summary(x)$df.null/summary(x)$df.residual)*
                       (1-(summary(x)$null.deviance - summary(x)$deviance)/summary(x)$null.deviance))*100

# Deviance Explained 
aDsq(m_global)   # 16.983
aDsq(m_natural_linear)  # 15.754
aDsq(m_natural_block) # 10.804
aDsq(m_natural)  # 9.792
aDsq(m_linear)  # 7.687
aDsq(m_linear_block)  # 9.223
aDsq(m_block)  # 1.693
aDsq(m_linear_plus_distance_riparian)  # 8.775


# Calculate overdispersion
sigma = sum(residuals(m_global, type = "pearson")^2) / (m_global$df.residual)
sum(residuals(m_natural_linear, type = "pearson")^2) / (m_natural_linear$df.residual)
sum(residuals(m_natural_block, type = "pearson")^2) / (m_natural_block$df.residual)
sum(residuals(m_natural, type = "pearson")^2) / (m_natural$df.residual)
sum(residuals(m_block, type = "pearson")^2) / (m_block$df.residual)
sum(residuals(m_linear_block, type = "pearson")^2) / (m_linear_block$df.residual)
sum(residuals(m_linear_plus_distance_riparian, type = "pearson")^2) / (m_linear_plus_distance_riparian$df.residual)
sum(residuals(m_linear, type = "pearson")^2) / (m_linear$df.residual)




### Create spatial blocks ----
?spatialBlock()
?spatialAutoRange()

# Load NRCAN Raster data 
nrcan_lc <- raster("NRCAN_reclass.tif")

# Set projections
projection(nrcan_lc, asText = TRUE)
deer_data_loc_sf <- st_transform(deer_data_loc_sf, crs = 3400)

x11(15, 15)

# Plot raster
tm_shape(nrcan_lc) + 
  tm_raster() + 
  tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 0, ]) + 
  tm_dots(col = "lightblue") +
  tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 1, ]) + 
  tm_dots(col = "grey19")

# tm_shape(nrcan_lc) + 
# tm_raster(palette = pal2, n = 17) + 
# tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 0, ]) + 
# tm_dots(col = "lightblue") +
# tm_shape(deer_data_loc_sf[deer_data_loc_sf$Use == 1, ]) + 
# tm_dots(col = "grey19")
pal1 <- tmaptools::get_brewer_pal("Accent", n = 17)
pal2 <- tmaptools::get_brewer_pal("Paired", n = 17)

# Explore block range
rangeExplorer(rasterLayer = nrcan_lc,
              speciesData = deer_data_loc_sf, # response data (optional)
              minRange = 30000, # limit the search domain
              maxRange = 100000)
# 32924 looks good. 

# Test spatial blocking
sb_32924 <- spatialBlock(speciesData = deer_data_loc_sf, 
                         species = "Use", 
                         rasterLayer = nrcan_lc, 
                         theRange = 32924, 
                         k = 10, 
                         iteration = 250, 
                         numLimit = 0, 
                         showBlocks = TRUE, 
                         progress = TRUE, 
                         verbose = TRUE)

foldExplorer(sb_32924, nrcan_lc, deer_data_loc_sf)


### Cross Validation Results ----
# Methods follow http://htmlpreview.github.io/?https://github.com/rvalavi/blockCV/blob/master/vignettes/BlockCV_for_SDM.html
# "Evaluating SDMs with block cross-validation: examples -- maxnet"
# Use blocks layer sb_32924

# Create a vector of 1 and 0 for presence and background
Use <- deer_data$Use

# Create a vector of the FoldID for each point in the data 
folds <- sb_32924$foldID 
# length(folds) = 14632 - same as number of deer used and available points

# Create an empty vector to store the AUC of each fold
# AUCs <- vector(mode = "numeric")
AUCs_updated_global <- vector(mode = "numeric")

# Cross validation with 10 blocks - global model 
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ grassland + herb_shrub_low + shrubland + cultivated_low + cultivated_med + cultivated_high
                    + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 +
                      + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low
                    + conifer_medhigh + wetland + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)


# Cross validation with 10 blocks - natural and linear features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ grassland + herb_shrub_low + shrubland
                    + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low
                    + conifer_medhigh + wetland + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - natural and block features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ grassland + herb_shrub_low + shrubland
                    + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low
                    + conifer_medhigh + wetland + rivers_dist + lakes_dist + cultivated_low + cultivated_med + cultivated_high
                    + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - natural features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ grassland + herb_shrub_low + shrubland
                    + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low
                    + conifer_medhigh + wetland + rivers_dist + lakes_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - block features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ cultivated_low + cultivated_med + cultivated_high
                    + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - linear features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - linear and riparian features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist + lakes_dist + rivers_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))   # 0.6826
AUCs_updated_global
round(AUCs_updated_global, 4)

# Cross validation with 10 blocks - linear and block features
for(k in seq_len(10)){
  trainSet <- which(folds != k) # training set indices
  testSet <- which(folds == k) # testing set indices
  glm_global <- glm(Use[trainSet] ~ cultivated_low + cultivated_med + cultivated_high
                    + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11
                    + roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                      transmission_dist + wellsites_dist, family = "binomial", data = deer_data[trainSet, ])
  testTable <- deer_data[testSet, ]
  testTable$pred <- predict(glm_global, deer_data[testSet, ], type = "response")
  precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Use)
  AUCs_updated_global[k] <- auc(precrec_obj)[1,4]
}
print(mean(AUCs_updated_global))
AUCs_updated_global
round(AUCs_updated_global, 4)


### Make a subset of the southern data ---------------
mapview(deer_data_loc_sf, zcol = "Point_ID")
Lat_Long <- n.deer_data %>% dplyr::select(Point_ID, Latitude, Longitude)

deer_data_loc_sf <- left_join(deer_data_loc_sf, Lat_Long)
mapview(filter(deer_data_loc_sf,Latitude < 55.2788))

deer_south <- n.deer_data %>% filter(Latitude < 55.2788)


library(Hmisc)
coefficients(m_global)
m_global_variable_list <- c("grassland","cultivation_total", "herb_shrub_low", "shrubland", "broadleaf_regen", 
                            "broadleaf_medhigh", "mixedwood_regen", "conifer_low", "conifer_medhigh", "wetland", 
                            "rivers_dist", "lakes_dist", "roads_dist", "seismic_grid_dist", "seismic_linetrail_dist", "pipeline_dist", 
                            "transmission_dist", "wellsites_dist", "cutblocks_pre97", "cutblocks_97to11", "cutblockspost11", "Latitude", "Longitude")

# Note: must use rcorr to produce class "rcorr" correlation matrix for this to work. Don't use cor() function. 
cor_m_global_variable_list <- rcorr(as.matrix((deer_south[, c(m_global_variable_list)])))
cor_m_global_variable_list_tbl <- flattenCorrMatrix(cor_m_global_variable_list$r, cor_m_global_variable_list$P) %>% as_tibble()

# Select all values greater than abs 0.2, and round to 3 digits
cor_m_global_variables_greater_point2 <- filter(cor_m_global_variable_list_tbl, cor >= 0.2 | cor <= -0.2)
cor_m_global_variables_greater_point2 <- cor_m_global_variables_greater_point2[order(cor_m_global_variables_greater_point2$cor), ]
cor_m_global_variables_greater_point2 <- cor_m_global_variables_greater_point2 %>% mutate_if(is.numeric, round, digits = 3)

cor_m_global_variable_list_tbl %>% arrange(desc(row)) %>% print(n = nrow(.))
cor_m_global_variable_list_tbl %>% arrange(desc(column)) %>% print(n = nrow(.))

# Vif
print(vif(n.deer_data[, c(m_global_variable_list)]))

# <Note> Lat and Long not strongly correlated with other variables. Good.



###  Models -------

# Global model without mixedwood
m_global <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen 
                + conifer_low + conifer_medhigh + wetland + cultivation_total +
                  + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist
                + seismic_linetrail_dist + pipeline_dist + transmission_dist + wellsites_dist + scale(Latitude) + scale(Longitude), 
                family = "binomial", data = deer_south, na.action ="na.fail")

m_global_unscaled <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen 
                         + conifer_low + conifer_medhigh + wetland + cultivation_total +
                           + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + rivers_dist + lakes_dist + roads_dist_new + seismic_grid_dist
                         + seismic_linetrail_dist + pipeline_dist + transmission_dist + wellsites_dist + Latitude + Longitude, 
                         family = "binomial", data = n.deer_data, na.action ="na.fail")


m_natural <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + mixedwood_medhigh + conifer_low + 
                   conifer_medhigh + wetland + lakes_dist + rivers_dist + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

m_block <- glm(Use ~ cultivation_total + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + scale(Latitude) + scale(Longitude),  
               family = "binomial", data = n.deer_data, na.action ="na.fail")

m_linear <- glm(Use ~ roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + 
                  pipeline_dist + transmission_dist + wellsites_dist + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

m_linear_block <- glm(Use ~ cultivation_total + roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + 
                        pipeline_dist + transmission_dist + wellsites_dist + cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + scale(Latitude) + scale(Longitude), 
                      family = "binomial", data = n.deer_data, na.action ="na.fail")

m_natural_block <-  glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low + 
                          conifer_medhigh + wetland + lakes_dist + rivers_dist + cultivation_total + 
                          cutblocks_pre97 + cutblocks_97to11 + cutblockspost11 + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

m_natural_linear <- glm(Use ~ grassland + herb_shrub_low + shrubland + broadleaf_regen + broadleaf_medhigh + mixedwood_regen + conifer_low + 
                          conifer_medhigh + wetland + lakes_dist + rivers_dist +  roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                          transmission_dist + wellsites_dist + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

m_linear_plus_distance_riparian <- glm(Use ~  roads_dist_new + seismic_grid_dist + seismic_linetrail_dist + pipeline_dist + 
                                         transmission_dist + wellsites_dist + lakes_dist + rivers_dist + scale(Latitude) + scale(Longitude), family = "binomial", data = n.deer_data, na.action ="na.fail")

## AIC Scores
model_list <- list(m_global,
                   m_natural,
                   m_block,
                   m_linear,
                   m_linear_block,
                   m_natural_block,
                   m_natural_linear,
                   m_linear_plus_distance_riparian)

names(model_list) <- c("global", "natural features", "block features", 
                       "linear features", "linear and block features",  "natural and block features", "natural and linear features", "linear and riparian features")

AIC_scores <- aictab(model_list)
kable(AIC_scores)
stargazer(AIC_scores, summary = FALSE, type = "text", out = "AIC_scores_global_models_new_cultivation.html")

# Coefficients - Global Model
tidy(m_global) %>% by_2sd(deer_data) %>% mutate_if(is.numeric, round, digits = 3) %>% stargazer(., summary = FALSE, type = "text", out = "coefficients_global_model.html")


### Coefficient Plot
dw_global <- dwplot(m_global, whisker_args = aes(color = "royalblue2", size = 0.75, alpha = 0.65), dot_args = 
                      aes(color = "royalblue3", size = 2, shape = 21, fill = "dodgerblue2")) %>% 
  relabel_predictors(c(grassland = "Grassland",
                       herb_shrub_low = "Herb-Shrub Low",
                       shrubland = "Shrubland",
                       broadleaf_regen = "Broadleaf, Regenerating",
                       broadleaf_medhigh = "Broadleaf, Medium to High Density",
                       mixedwood_regen = "Mixedwood, Regenerating",
                       conifer_low = "Conifer, Low Density",
                       conifer_medhigh = "Conifer, Medium to High Density",
                       wetland = "Wetlands",
                       rivers_dist = "Rivers Distance", 
                       lakes_dist = "Lakes Distance",
                       cultivation_total = "Cultivation",
                       cutblocks_pre97 = "Cutblocks pre-1997", 
                       cutblocks_97to11 = "Cutblocks 1997 to 2011", 
                       cutblockspost11 = "Cutblocks post-2011",
                       roads_dist_new = "Roads Dist",
                       seismic_linetrail_dist = "Sesimic Lines Dist",
                       seismic_grid_dist = "Seismic Grids Dist",
                       pipeline_dist = "Pipelines Dist", 
                       transmission_dist = "Transmission Lines Dist", 
                       wellsites_dist = "Wellsites Dist")) +
  theme_bw() + xlab("Strength of selection / Effect Size") +
  ggtitle("Coefficient Estimates for Global RSF Model") + 
  geom_vline(xintercept = 0, colour = "grey65", linetype = 2) + 
  theme(plot.title = element_text(size = 11)) + 
  theme(legend.position = "none") + xlim(-1.5, 1.5)

# Change sign of distance variables on the main plot
test <- ggplot_build(dw_global)

test$data[[2]] <- test$data[[2]] %>% mutate(x = ifelse(y == 13 | y == 14 | y <= 8 & y > 2, x*(-1), x))

test$data[[1]] <- test$data[[1]] %>% mutate(xmin = ifelse(y == 13 | y == 14 | y <= 8 & y > 2, xmin*(-1), xmin)) %>% 
  mutate(xmax = ifelse(y == 13 | y == 14 | y <= 8 & y > 2, xmax*(-1), xmax))

test <- ggplot_gtable(test)
plot(test)

p.global_coefficients <- test

plot_grid(p.global_coefficients_former, p.global_coefficients) %>% save_plot("global_coefficients_comparison.jpeg", ., base_width = 14, base_height = 8)
plot_grid(p.global_coefficients) %>% save_plot("global_coefficients_withlat_scaled.jpeg", ., base_width = 8, base_height = 8)

### Coefficient table for global model ----
# coefficient_table <- m_global %>% tidy() %>% by_2sd(deer_data) %>% print(n = nrow(.)) %>% mutate_if(is.numeric, round, digits = 3)
# stargazer(coefficient_table, summary = FALSE, type = "html", out = "coefficients_global_model_new.csv")


### Part 4 - Spatial Autocorrelation in Residuals -----

# Add spatial data in lat/long
n.deer_data 
deer_loc_table

### Plot just the deer locations residuals so the random locations don't clutter over top of the map
resid_m_global =  m_global$residuals
test_plot <- ggplot(n.deer_data) + theme_minimal() +
  geom_point(aes(Longitude, Latitude, size = abs(resid_m_global), shape = as.character(sign(resid_m_global)), alpha = as.character(sign(resid_m_global)),
                 colour = as.character(sign(resid_m_global)))) +
  scale_shape_manual(values = c(1, 16)) +
  scale_colour_manual(values = c("steelblue", "darkorange")) +
  scale_alpha_manual(values = c(1, 1)) +
  coord_fixed() +
  scale_size_continuous(range = c(0, 5)) +
  labs(colour = "residual sign", shape = "residual sign",
       size = "residual magnitude")
print(test_plot)

save_plot("residual_spatial_correlation_with_latlong.jpeg", test_plot, base_width = 8, base_height = 8)

# Slow step -
deer.dists <- as.matrix(dist(cbind(deer_south$Latitude, deer_south$Longitude)))
deer.dists.inv <- 1/deer.dists  # Also a huge table
diag(deer.dists.inv) <- 0
library(ape)
?Moran.I
Moran.I(m_global$residuals, deer.dists.inv)

# 2 pannel plot - locations to certain habitat types 
# "multi-panel plot based on the appropriate variables"
# interaction term - "if the response to cultivated land depended on latitude"
# if the relationship to one variable is mediated by another
# spatial autocorrelation could be "intrinsic" due to a grouping process, or "apparent" due to a missing thing that may be clustered in space 
# systematic difference in habitat quality vs. something that is more ephemral clustering process
# "intrinsic" random process or more difficult to observe

# GAM - non logistic shapes or spatial clustering
# An apparent problem in one thing can be caused by a problem almost anywhere else 
# Model checking assumptions of logistic 
# GAM fxn ?mgcv? library - GAM functions have their own function that will display the response for you
# Modelling assumption - curve for each of your predictor variables and they get added together to give your predictor term
# Assumptions similar to GLM but not restricted to jsut getting logistic s-curve - there's some 
# things you can tweak to curve flexibility, but the default settings are often good - uses a form of cross-validation
# to avoid the most intense forms of over-fitting.
# Visualizing multi-pannel 

# Bubble plots relative to eastings and northings, or *any other two variables*.
# To capture any non-logistic curves 

# EG - seismic grids not homogeneously distributed therefore some groupig of under-prediction - how to model this?

# GAM more flexible to predictor also include smooting term relative to lat/long - don't want to smooth too much - overfit

# Influence of seismic grid distance positive at near distances and 
# resoonse variables - mgcv - GAM function
# special issues when using RSF compared to distribution model - interpretations different

# "Well written paragraph"
# Use GAM to model diagnostics or conform something you know
# more thoroughly looking at what might be causing spatial correlation, what's going on, smoothers relative to space
# "the combination of sensible interpretation combined with colourful data plots is not to be underestimated"
# Following up your model assumptions and not overinterpeting model
# More residual plots to see if there's patters or structure 










# Autocorrelation seems too strong for observed map and coefficient plots
# 1) Try Moran.I with other packages
# 2) Try other spatial autocorrelation tests

qqnorm(residuals(m_global)) 
qqline(residuals(m_global))
# Residuals are *bonkers* on one end


plot(m_global)

library(nlme)
semivario <- Variogram(m_global$residuals, distance = deer.dists.inv)
plot(semivario, smooth = TRUE)
?Variogram
# Not working 

# Geary's ----
library(spdep)
geary.test(m_global$residuals, )

deer_south_sf <- st_as_sf(deer_south, coords = c("Longitude", "Latitude"), crs = 4326)
deer_south_sf <- deer_south_sf %>% st_transform(crs = crs(deer_data_loc_sf))
deer_south_sf %>% nrow()

deer_south_buffer <- st_buffer(deer_south_sf, dist = 3)

nb <- poly2nb(deer_south_buffer, queen = TRUE)

ls <- nb2listw(nb, style="B", zero.policy=TRUE)

moran.test(m_global$residuals, )

## No method found for dealing with points. Looks like these need polygons with intersecting edges.
