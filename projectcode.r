# Chart 1: 

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(dplyr)
library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)

df <- data %>%
  filter(RACE == 2,
         AGE >= 25, AGE <= 64,
         YEAR >= 1920, YEAR <= 2021,
         !is.na(OCCSCORE),
         !is.na(PERWT))

df2 <- data %>%
  filter(RACE == 1,
         AGE >= 25, AGE <= 64,
         YEAR >= 1920, YEAR <= 2021,
         !is.na(OCCSCORE),
         !is.na(PERWT))

trend <- df %>%
  group_by(YEAR) %>%
  summarise(
    mean_score = weighted.mean(OCCSCORE, PERWT, na.rm = TRUE),
    group = "Black"
  )

trend2 <- df2 %>%
  group_by(YEAR) %>%
  summarise(
    mean_score = weighted.mean(OCCSCORE, PERWT, na.rm = TRUE),
    group = "White"
  )

trend_all <- bind_rows(trend, trend2)

graph <- ggplot(trend_all, aes(x = YEAR, y = mean_score, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Black" = "blue", "WHite" = "red")) + 
  labs(
    title = "Mean OCCSCORE of African Americans vs White Americans",
    x = "Census Year",
    y = "Mean Occupational Income Score",
    color = "Group"
  )
theme_minimal(base_size = 14) 

print(graph)


# Chart 2:

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(dplyr)
library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)

df <- data %>%
  filter(RACE == 2,
         AGE >= 25, AGE <= 64,
         YEAR >= 1920, YEAR <= 2021,
         !is.na(SEI),
         !is.na(PERWT))

df2 <- data %>%
  filter(RACE == 1,
         AGE >= 25, AGE <= 64,
         YEAR >= 1920, YEAR <= 2021,
         !is.na(SEI),
         !is.na(PERWT))

trend <- df %>%
  group_by(YEAR) %>%
  summarise(
    mean_score = weighted.mean(SEI, PERWT, na.rm = TRUE),
    group = "Black"
  )

trend2 <- df2 %>%
  group_by(YEAR) %>%
  summarise(
    mean_score = weighted.mean(SEI, PERWT, na.rm = TRUE),
    group = "White"
  )

trend_all <- bind_rows(trend, trend2)

graph <- ggplot(trend_all, aes(x = YEAR, y = mean_score, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Black" = "blue", "WHite" = "red")) + 
  labs(
    title = "Mean SEI of African Americans vs White Americans",
    x = "Census Year",
    y = "Mean Duncan Socioeconomic Index",
    color = "Group"
  )
theme_minimal(base_size = 14) 

print(graph)

# Chart 3:

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(dplyr)
library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)

df <- data %>%
  filter(RACE == 2,
         AGE >= 25, AGE <= 64,
         YEAR >= 1920, YEAR <= 2021,
         !is.na(EDUC),
         !is.na(PERWT))

df2 <- data %>%
  filter(RACE == 1,
         AGE >= 25, AGE <= 64,
         YEAR >= 1920, YEAR <= 2021,
         !is.na(EDUC),
         !is.na(PERWT))

df <- df %>% 
  filter(EDUC != 99)

df2 <- df2 %>% 
  filter(EDUC != 99)

trend <- df %>%
  group_by(YEAR) %>%
  summarise(
    mean_score = weighted.mean(EDUC, PERWT, na.rm = TRUE),
    group = "Black"
  )

trend2 <- df2 %>%
  group_by(YEAR) %>%
  summarise(
    mean_score = weighted.mean(EDUC, PERWT, na.rm = TRUE),
    group = "White"
  )

trend_all <- bind_rows(trend, trend2)

graph <- ggplot(trend_all, aes(x = YEAR, y = mean_score, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Black" = "blue", "WHite" = "red")) + 
  labs(
    title = "Mean Educational Attainment of African Americans vs White Americans",
    x = "Census Year",
    y = "Mean Educational Attainment",
    color = "Group"
  )
theme_minimal(base_size = 14) 

print(graph)

# Chart 4:

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(dplyr)
library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)

# Filter dataset
df <- data %>%
  filter(
    AGE >= 25, AGE <= 64,
    YEAR %in% c(1920, 1940, 1960, 1980, 2000, 2021),   # choose key years
    RACE %in% c(1, 2),                                 # 1 = White, 2 = Black
    !is.na(EDUC),
    !is.na(OCCSCORE),
    !is.na(PERWT)
  ) %>%
  mutate(
    race_label = ifelse(RACE == 1, "White", "Black")
  )

df <- df %>% 
  filter(EDUC != 99)

# Compute weighted mean OCCSCORE by YEAR × EDU × RACE
edu_trends <- df %>%
  group_by(YEAR, EDUC, race_label) %>%
  summarise(
    occ_mean = weighted.mean(OCCSCORE, PERWT, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Graph: OCCSCORE vs EDU, colored by race, faceted by YEAR
graph <- ggplot(edu_trends, aes(x = EDUC, y = occ_mean, color = race_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Black" = "blue", "White" = "red")) +
  labs(
    title = "Returns to Education by Race Across Time",
    x = "Education Level (EDUC)",
    y = "Mean Occupational Income Score (OCCSCORE)",
    color = "Race"
  ) +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_minimal(base_size = 14)

print(graph)

# Chart 5:

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(dplyr)
library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)

# Filter dataset
df <- data %>%
  filter(
    AGE >= 25, AGE <= 64,
    YEAR %in% c(1920, 1940, 1960, 1980, 2000, 2021),   # choose key years
    RACE %in% c(1, 2),                                 # 1 = White, 2 = Black
    !is.na(EDUC),
    !is.na(SEI),
    !is.na(PERWT)
  ) %>%
  mutate(
    race_label = ifelse(RACE == 1, "White", "Black")
  )

df <- df %>% 
  filter(EDUC != 99)

# Compute weighted mean OCCSCORE by YEAR × EDU × RACE
edu_trends <- df %>%
  group_by(YEAR, EDUC, race_label) %>%
  summarise(
    occ_mean = weighted.mean(SEI, PERWT, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Graph: Duncan Socioeconomic Index vs EDU, colored by race, faceted by YEAR
graph <- ggplot(edu_trends, aes(x = EDUC, y = occ_mean, color = race_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Black" = "blue", "White" = "red")) +
  labs(
    title = "Status versus Education by Race Across Time",
    x = "Education Level (EDUC)",
    y = "SEI (Duncan Socioeconomic Index)",
    color = "Race"
  ) +
  facet_wrap(~ YEAR, scales = "free_y") +
  theme_minimal(base_size = 14)

print(graph)

# Chart 6 + 7:

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(dplyr)
library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)


df <- data %>%
  filter(
    AGE >= 25, AGE <= 64,
    YEAR %in% c(1920, 1940, 1960, 1980, 2000, 2021),   # choose key years
    RACE %in% c(1, 2),           # 1 = White, 2 = Black
    SEX %in% c(1, 2),            # 1 = Male, 2 = Female
    EDUC < 90,                   # remove 99/NIU and all unknowns
    !is.na(OCCSCORE),
    !is.na(PERWT)
  ) %>%
  mutate(
    race_label = ifelse(RACE == 1, "White", "Black"),
    sex_label  = ifelse(SEX == 1, "Male", "Female")
  )

edu_gender <- df %>%
  group_by(YEAR, EDUC, race_label, sex_label) %>%
  summarise(
    occ_mean = weighted.mean(OCCSCORE, PERWT, na.rm = TRUE),
    .groups = "drop"
  )

white_plot <- edu_gender %>%
  filter(race_label == "White") %>%
  ggplot(aes(x = EDUC, y = occ_mean, color = sex_label)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ YEAR, scales = "free_y") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Whites: Returns by Education and Gender",
    x = "Educational Attainment",
    y = "Mean Occupational Income Score",
    color = "Gender"
  ) +
  theme_minimal(base_size = 14)



black_plot <- edu_gender %>%
  filter(race_label == "Black") %>%
  ggplot(aes(x = EDUC, y = occ_mean, color = sex_label)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ YEAR, scales = "free_y") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Blacks: Returns by Education and Gender",
    x = "Educational Attainment",
    y = "Mean Occupational Income Score",
    color = "Gender"
  ) +
  theme_minimal(base_size = 14)

print(black_plot)
print(white_plot)

# Chart 8 + 9:

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

library(dplyr)
library(tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)


df <- data %>%
  filter(
    AGE >= 25, AGE <= 64,
    YEAR %in% c(1920, 1940, 1960, 1980, 2000, 2021),   # choose key years
    RACE %in% c(1, 2),           # 1 = White, 2 = Black
    SEX %in% c(1, 2),            # 1 = Male, 2 = Female
    EDUC < 90,                   # remove 99/NIU and all unknowns
    !is.na(SEI),
    !is.na(PERWT)
  ) %>%
  mutate(
    race_label = ifelse(RACE == 1, "White", "Black"),
    sex_label  = ifelse(SEX == 1, "Male", "Female")
  )

edu_gender <- df %>%
  group_by(YEAR, EDUC, race_label, sex_label) %>%
  summarise(
    occ_mean = weighted.mean(SEI, PERWT, na.rm = TRUE),
    .groups = "drop"
  )

white_plot <- edu_gender %>%
  filter(race_label == "White") %>%
  ggplot(aes(x = EDUC, y = occ_mean, color = sex_label)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ YEAR, scales = "free_y") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Whites: Status by Education and Gender",
    x = "Educational Attainment",
    y = "SEI (Mean Duncan Socioeconomic Index)",
    color = "Gender"
  ) +
  theme_minimal(base_size = 14)



black_plot <- edu_gender %>%
  filter(race_label == "Black") %>%
  ggplot(aes(x = EDUC, y = occ_mean, color = sex_label)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ YEAR, scales = "free_y") +
  scale_color_manual(values = c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Blacks: Status by Education and Gender",
    x = "Educational Attainment",
    y = "SEI (Mean Duncan Socioeconomic Index)",
    color = "Gender"
  ) +
  theme_minimal(base_size = 14)

print(white_plot)
print(black_plot)

# Chart 10:

# ============================
#   LOAD PACKAGES
# ============================

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ipumsr)
library(maps)
library(gganimate)
library(viridis)

# ============================
#   LOAD IPUMS MICRODATA
# ============================

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)

# ============================
#   STATEFIP → STATE LOOKUP
# ============================

state_lookup <- tibble::tribble(
  ~STATEFIP, ~state, ~state_lower,
  1, "Alabama", "alabama",
  2, "Alaska", "alaska",
  4, "Arizona", "arizona",
  5, "Arkansas", "arkansas",
  6, "California", "california",
  8, "Colorado", "colorado",
  9, "Connecticut", "connecticut",
  10, "Delaware", "delaware",
  11, "District of Columbia", "district of columbia",
  12, "Florida", "florida",
  13, "Georgia", "georgia",
  15, "Hawaii", "hawaii",
  16, "Idaho", "idaho",
  17, "Illinois", "illinois",
  18, "Indiana", "indiana",
  19, "Iowa", "iowa",
  20, "Kansas", "kansas",
  21, "Kentucky", "kentucky",
  22, "Louisiana", "louisiana",
  23, "Maine", "maine",
  24, "Maryland", "maryland",
  25, "Massachusetts", "massachusetts",
  26, "Michigan", "michigan",
  27, "Minnesota", "minnesota",
  28, "Mississippi", "mississippi",
  29, "Missouri", "missouri",
  30, "Montana", "montana",
  31, "Nebraska", "nebraska",
  32, "Nevada", "nevada",
  33, "New Hampshire", "new hampshire",
  34, "New Jersey", "new jersey",
  35, "New Mexico", "new mexico",
  36, "New York", "new york",
  37, "North Carolina", "north carolina",
  38, "North Dakota", "north dakota",
  39, "Ohio", "ohio",
  40, "Oklahoma", "oklahoma",
  41, "Oregon", "oregon",
  42, "Pennsylvania", "pennsylvania",
  44, "Rhode Island", "rhode island",
  45, "South Carolina", "south carolina",
  46, "South Dakota", "south dakota",
  47, "Tennessee", "tennessee",
  48, "Texas", "texas",
  49, "Utah", "utah",
  50, "Vermont", "vermont",
  51, "Virginia", "virginia",
  53, "Washington", "washington",
  54, "West Virginia", "west virginia",
  55, "Wisconsin", "wisconsin",
  56, "Wyoming", "wyoming"
)

# ============================
#   CLEAN + AGGREGATE DATA
# ============================

df_clean <- data %>%
  filter(
    AGE >= 25, AGE <= 64,
    YEAR >= 1920, YEAR <= 2021,
    EDUC != 99,
    !is.na(OCCSCORE),
    !is.na(PERWT),
    STATEFIP %in% state_lookup$STATEFIP
  ) %>%
  group_by(YEAR, STATEFIP) %>%
  summarise(
    mean_occ = weighted.mean(OCCSCORE, PERWT, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  left_join(state_lookup, by = "STATEFIP")

# ============================
#   LOAD MAP DATA
# ============================

states_map <- map_data("state") %>%
  rename(state_lower = region)

# ============================
#   JOIN MAP + DATA
# ============================

map_df <- states_map %>%
  left_join(df_clean, by = "state_lower")

# ============================
#   ANIMATED HEATMAP (SLIDER)
# ============================

p <- ggplot(map_df) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = mean_occ),
    color = "white", linewidth = 0.25
  ) +
  coord_map() +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Mean OCCSCORE by State — Year: {closest_state}",
    subtitle = "Ages 25–64, Weighted by PERWT",
    fill = "Mean OCCSCORE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  transition_states(YEAR, transition_length = 1, state_length = 1) +
  ease_aes("linear")

# Render
anim <- animate(p, width = 900, height = 600, fps = 6, duration = 20,
                renderer = gifski_renderer())

anim_save("state_occscore_animation.gif", animation = anim)

# Chart 11:

# ============================
#   LOAD PACKAGES
# ============================

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ipumsr)
library(maps)
library(gganimate)
library(viridis)

# ============================
#   LOAD IPUMS MICRODATA
# ============================

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)

# ============================
#   STATEFIP → STATE LOOKUP
# ============================

state_lookup <- tibble::tribble(
  ~STATEFIP, ~state, ~state_lower,
  1, "Alabama", "alabama",
  2, "Alaska", "alaska",
  4, "Arizona", "arizona",
  5, "Arkansas", "arkansas",
  6, "California", "california",
  8, "Colorado", "colorado",
  9, "Connecticut", "connecticut",
  10, "Delaware", "delaware",
  11, "District of Columbia", "district of columbia",
  12, "Florida", "florida",
  13, "Georgia", "georgia",
  15, "Hawaii", "hawaii",
  16, "Idaho", "idaho",
  17, "Illinois", "illinois",
  18, "Indiana", "indiana",
  19, "Iowa", "iowa",
  20, "Kansas", "kansas",
  21, "Kentucky", "kentucky",
  22, "Louisiana", "louisiana",
  23, "Maine", "maine",
  24, "Maryland", "maryland",
  25, "Massachusetts", "massachusetts",
  26, "Michigan", "michigan",
  27, "Minnesota", "minnesota",
  28, "Mississippi", "mississippi",
  29, "Missouri", "missouri",
  30, "Montana", "montana",
  31, "Nebraska", "nebraska",
  32, "Nevada", "nevada",
  33, "New Hampshire", "new hampshire",
  34, "New Jersey", "new jersey",
  35, "New Mexico", "new mexico",
  36, "New York", "new york",
  37, "North Carolina", "north carolina",
  38, "North Dakota", "north dakota",
  39, "Ohio", "ohio",
  40, "Oklahoma", "oklahoma",
  41, "Oregon", "oregon",
  42, "Pennsylvania", "pennsylvania",
  44, "Rhode Island", "rhode island",
  45, "South Carolina", "south carolina",
  46, "South Dakota", "south dakota",
  47, "Tennessee", "tennessee",
  48, "Texas", "texas",
  49, "Utah", "utah",
  50, "Vermont", "vermont",
  51, "Virginia", "virginia",
  53, "Washington", "washington",
  54, "West Virginia", "west virginia",
  55, "Wisconsin", "wisconsin",
  56, "Wyoming", "wyoming"
)

# ============================
#   CLEAN + AGGREGATE DATA
# ============================

df_clean <- data %>%
  filter(
    AGE >= 25, AGE <= 64,
    RACE == 2,
    YEAR >= 1920, YEAR <= 2021,
    EDUC != 99,
    !is.na(OCCSCORE),
    !is.na(PERWT),
    STATEFIP %in% state_lookup$STATEFIP
  ) %>%
  group_by(YEAR, STATEFIP) %>%
  summarise(
    mean_occ = weighted.mean(OCCSCORE, PERWT, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  left_join(state_lookup, by = "STATEFIP")

# ============================
#   LOAD MAP DATA
# ============================

states_map <- map_data("state") %>%
  rename(state_lower = region)

# ============================
#   JOIN MAP + DATA
# ============================

map_df <- states_map %>%
  left_join(df_clean, by = "state_lower")

# ============================
#   ANIMATED HEATMAP (SLIDER)
# ============================

p <- ggplot(map_df) +
  geom_polygon(
    aes(x = long, y = lat, group = group, fill = mean_occ),
    color = "white", linewidth = 0.25
  ) +
  coord_map() +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Mean Black OCCSCORE by State — Year: {closest_state}",
    subtitle = "Ages 25–64, Weighted by PERWT",
    fill = "Mean OCCSCORE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  transition_states(YEAR, transition_length = 1, state_length = 1) +
  ease_aes("linear")

# Render
anim <- animate(p, width = 900, height = 600, fps = 6, duration = 20,
                renderer = gifski_renderer())

anim_save("state_occscore_animation.gif", animation = anim)