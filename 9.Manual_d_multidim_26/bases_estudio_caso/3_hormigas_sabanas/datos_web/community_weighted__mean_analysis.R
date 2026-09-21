### Calculate CWM for each trait

library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(tidyr)
library(readr)


# 1. Load the data
### File used: "data_traits.csv"
data <- read_csv2("data_traits.csv")  # separator ";", Standard UTF-8 encoding.
data

# 2. Filter and transform columns
# Replace "NI" with NA and convert to numeric
traits <- c("Eye_length_(mm)", "Mandible_length_(mm)", "Scape_length_(mm)", "Leg_length_(mm)", "Weber_length_(mm)")
traits


data <- data %>%
  mutate(across(all_of(traits), ~na_if(., "NI"))) %>%
  mutate(across(all_of(traits), as.numeric),
         Abundance = as.numeric(Abundance),
         Status2 = factor(Status2),
         Region = factor(Region)) %>%
  drop_na(Abundance, any_of(traits))

data

# 3. Calculate CWM for each area
cwm_area <- data %>%
  group_by(Area) %>%
  summarise(across(all_of(traits), ~weighted.mean(., Abundance), .names = "CWM_{.col}"))

print(cwm_area)


###USING THE FD PACKAGE

# Install if necessary
# install.packages("FD")

library(FD)

# 1. Load the data
### Files used: "matrix_abundance_FD.csv" and "matrix_traits_FD.csv"
abund <- read.csv("matrix_abundance_FD.csv", row.names = 1, check.names = FALSE)
traits <- read.csv("matrix_traits_FD.csv", row.names = 1)

# 2. Calculate CWM using the functcomp function
# CWM will be returned by community (area)
cwm_result <- functcomp(traits, t(abund), CWM.type = "all")

# 3. View results
print(cwm_result)

# 4. (Optional) Save for later analysis
write.csv(cwm_result, "resultados_CWM_FD.csv")

#USING INCIDENCE DATA

# Load the data
### Trait file used: "traits_matrix.csv"
### Incidence matrix file used: "incidence_matrix.csv"
traits <- read.csv("traits_matrix.csv", row.names = 1)        # Trait matrix (species × traits) 
incidence <- read.csv("incidence_matrix.csv", row.names = 1)  # Presence/absence matrix (areas × species)

# Ensure that species are aligned
common_species <- intersect(colnames(incidence), rownames(traits))
incidence <- incidence[, common_species]
traits <- traits[common_species, ]

# Convert to matrix
traits_mat <- as.matrix(traits)
incidence_mat <- as.matrix(incidence)

# Calculate CWM using incidence data
cwm_result <- functcomp(traits_mat, incidence_mat, CWM.type = "all")

# View results
print(cwm_result)

# Calculate CWM based on presence/absence data
cwm_result <- functcomp(traits, incidence, CWM.type = "all")

# View results
print(cwm_result)

# Save as a CSV file
write.csv(cwm_result, "CWM_incidence_results.csv")
