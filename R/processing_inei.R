library(sf)
library(readr)
library(dplyr)
library(mapview)
library(tmap)

#read csv
temp_dir_csv <- tempdir()
url_csv <- "https://heibox.uni-heidelberg.de/f/8afe01ef2e7247a983e0/?dl=1"
# File path to save the downloaded file
file_path_csv <- file.path(temp_dir, "inei.csv")
# Download the csv file
download.file(url_csv, file_path_csv)
# Read the csv file
df <- read_csv(file_path_csv)


#select rows thar are relevant
cols <- c(17, 18, 19, 20, 21)

#filter data
sel <- df %>%
  filter(P612N %in% cols & P612 == 1)

#remove leading 0
sel$UBIGEO <- sub("^0+", "", sel$UBIGEO)

#read peru shapefile with adm3 data
# Create a temporary directory
temp_dir <- tempdir()

url <- "https://heibox.uni-heidelberg.de/f/bea7512d5cae4eaeac7e/?dl=1"

# File path to save the downloaded file
file_path <- file.path(temp_dir, "peru.gpkg")

# Download the GeoPackage file
download.file(url, file_path)

# Read the GeoPackage file
data <- st_read(file_path)


#filter lima geometries (districts)
lima_met <- data %>% 
  filter(ADM2_ES == "Lima")

#remove leading PE
lima_met$ADM3_PCODE <- gsub("PE", "", lima_met$ADM3_PCODE)

#extract data from the INEI dataframe based on the districts dataframe

distritos <- as.list(lima_met$ADM3_PCODE)


sel_lima <- sel %>%
  filter(UBIGEO %in% distritos)


# Create a new column to generate a unique ID
sel_lima <- sel_lima %>%
  mutate(id = paste(CONGLOME, VIVIENDA, HOGAR, UBIGEO, sep = "_"))

#subselect dataframe

sel_lima <- sel_lima %>% 
  select(id, UBIGEO, P612N)


#17 = owns a car
#18 owns a motorcycle
#20 owns a tuktuk
#21 owns a truck

sel_lima <- sel_lima %>% 
  filter(P612N == 17)

#aggregate data
sel_lima_count <- sel_lima %>%
  group_by(UBIGEO) %>%
  summarise(count=n())

#prepare data to create a join
lima_met <- lima_met %>% 
  select(ADM3_ES, ADM3_PCODE)

#join geometry and table
df_merge <- merge(x = lima_met, y = sel_lima_count[ , c("UBIGEO", "count")], by.x = "ADM3_PCODE", by.y = "UBIGEO",
                  all.x=TRUE)

#quick plot
tm_shape(df_merge) +
  tm_polygons("count")








