library(sf)
library(readr)
library(dplyr)
library(mapview)
library(tmap)
library(patchwork)


file_path_csv <- "https://heibox.uni-heidelberg.de/f/73b5a90673e446a8ab01/?dl=1"
# Download the csv file

# Read the csv file
df <- read_csv(file_path_csv)



sel <- df

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
  mutate(id = paste(CONGLOME, VIVIENDA, HOGAR, sep = "_"))

#select rows thar are relevant
cols <- c(17, 18, 19, 20, 21)

#filter data
sel <- sel_lima %>%
  filter(P612N %in% cols)

sel <- sel %>% 
  select(P612N, P612, UBIGEO, id)

sel_pivot <- sel %>%
  tidyr::pivot_wider(names_from = P612N, values_from = P612)



car <- sel_pivot %>% 
  select(UBIGEO, id, `17`)


# Summarize the dataframe by the 'region' column
summary_df <- car %>%
  group_by(UBIGEO) %>%
  summarize(
    yes_count = sum(`17` == 1, na.rm = TRUE),
    no_count = sum(`17` == 2, na.rm = TRUE),
    no_data_count = sum(`17` == 9, na.rm = TRUE),
    total_count = n()
  )

lima_met <- lima_met %>% 
  select(ADM3_PCODE)

#join geometry and table
df_merge <- merge(x = lima_met, y = summary_df, by.x = "ADM3_PCODE", by.y = "UBIGEO",
                  all.x=TRUE)

df_merge <- df_merge %>% 
  mutate(yes_perc = (yes_count/total_count)*100) %>% 
  mutate(no_perc = (no_count/total_count)*100) %>% 
  mutate(no_data_perc = (no_data_count/total_count)*100)

library(ggplot2)


# Plot for 'yes'
# Plot for 'yes'
plot_yes <- ggplot(df_merge) +
  geom_sf(aes(fill = yes_perc), color = "black") +
  scale_fill_gradient(low = "white", high = "green") +
  labs(title = "Yes") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

plot_yes

# Plot for 'no'
plot_no <- ggplot(df_merge) +
  geom_sf(aes(fill = no_perc), color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "No") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Plot for 'no_data'
plot_no_data <- ggplot(df_merge) +
  geom_sf(aes(fill = no_data_perc), color = "black") +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "No Data") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

# Plot for 'no_data'
plot_count <- ggplot(df_merge) +
  geom_sf(aes(fill = total_count), color = "black") +
  scale_fill_gradient(low = "white", high = "yellow") +
  labs(title = "Total count") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

plot_no


combined_plot <- plot_yes + plot_no + plot_no_data + plot_count + plot_layout(ncol = 2)

# Print the combined plot

combined_plot
ggsave()

