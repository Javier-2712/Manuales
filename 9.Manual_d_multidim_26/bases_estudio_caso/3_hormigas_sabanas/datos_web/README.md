# Data from: Functional and phylogenetic responses of arboreal ants to land-use change in Neotropical savannas

Dataset DOI: 10.5061/dryad.8kprr4z3b

# Overview

This repository contains the datasets, phylogenetic files, and R scripts used to evaluate the functional and phylogenetic responses of arboreal ant communities to land-use change in Neotropical savannas (Brazilian Cerrado), including preserved, rural, and urban habitats in northern Minas Gerais, Brazil.

The repository includes raw species occurrence matrices, functional trait matrices, phylogenetic trees, and scripts used to calculate community-weighted means (CWM), functional diversity, and phylogenetic diversity metrics.

# File List

## Main datasets

### ant_traits_raw_data.xlsx

Main dataset containing abundance, environmental classification, and morphological trait information used in the analyses.

Each row corresponds to one ant species occurrence in a sampling area.

#### Variables

Code: Sampling area identifier. 

Region: Geographic region where the sampling area is located.

Area: Habitat/environment category.

* Open cerrado = preserved open savanna vegetation
* Closed cerrado = preserved closed savanna vegetation
* Rural = anthropogenic rural habitat
* Urban = anthropogenic urban habitat

Abundance: Number of individuals recorded for each ant species in a sampling area.

Eye_length_(mm): Eye length measured in millimeters.

Mandible_length_(mm): Mandible length measured in millimeters.

Scape_length_(mm): Antennal scape length measured in millimeters.

Leg_length_(mm): Leg length measured in millimeters.

Weber_length_(mm): Weber length used as a proxy for body size, measured in millimeters.

### incidence_matrix.csv

Presence/absence matrix used for functional and phylogenetic analyses.

Rows: Sampling areas.

Columns: Ant species.

Values:

* 0 = absence
* 1 = presence

### community_species_matrix.txt

Species abundance matrix used in phylogenetic diversity analyses.

Rows: Sampling areas.

Columns: Ant species.

Values: Species abundance recorded in each sampling area.

### traits_matrix.csv

Functional trait matrix used for community-weighted mean (CWM) and functional diversity analyses.

Rows: Ant species.

Columns: Functional morphological traits.

#### Variables

WL: Weber length (mm).

EL: Eye length (mm).

ML: Mandible length (mm).

SL: Scape length (mm).

LL: Leg length (mm).

### species_metadata.txt

Taxonomic metadata for all species included in the analyses.

#### Variables

species: Species name.

genus: Genus name.

subfamily: Ant subfamily.

family: Ant family.

## Additional functional diversity matrices

### data_traits.csv

Raw dataset containing species-level morphological trait measurements and abundance information used for functional diversity analyses and community-weighted mean (CWM) calculations.

Each row corresponds to one species occurrence within a sampling area.

Note: This CSV file is delimited by semicolons (;), rather than commas. This formatting should be considered when importing the file into spreadsheet software or statistical environments (e.g., Excel, R, or Python) to ensure correct column separation and data interpretation.

#### Variables

Area: Sampling area identifier.

Region: Geographic region where the sampling area is located.

Status2: Habitat/environment category.

* Open cerrado = preserved open savanna vegetation
* Closed cerrado = preserved closed savanna vegetation
* Rural = anthropogenic rural habitat
* Urban = anthropogenic urban habitat

Species: Ant species identity.

Abundance: Number of individuals recorded for each species in a sampling area.

Eye_length_(mm): Eye length measured in millimeters.

Mandible_length_(mm): Mandible length measured in millimeters.

Scape_length_(mm): Antennal scape length measured in millimeters.

Leg_length_(mm): Leg length measured in millimeters.

Weber_length_(mm): Weber length used as a proxy for body size, measured in millimeters.

### matrix_abundance_FD.csv

Species abundance matrix used for functional diversity analyses with the FD package in R.

Rows: Ant species.

Columns: Sampling areas.

Values: Species abundance recorded in each sampling area.

This matrix was used for the calculation of:

* community-weighted means (CWM),
* functional richness,
* functional evenness,
* functional divergence,
* and other functional diversity metrics.

### matrix_traits_FD.csv

Functional trait matrix used in the FD package analyses.

Rows: Ant species.

Columns: Morphological traits.

#### Variables

WL: Weber length (body size proxy), measured in millimeters.

EL: Eye length, measured in millimeters.

ML: Mandible length, measured in millimeters.

SL: Scape length, measured in millimeters.

LL: Leg length, measured in millimeters.

Trait values were used to calculate community-weighted means and multidimensional functional diversity metrics.

# Phylogenetic files

### moreau_bell_phylogeny_2013.nex

Original phylogenetic tree from Moreau & Bell (2013) used as the backbone phylogeny.

### phylogeny_analysis_step_12182.nex

Final phylogenetic tree used in downstream analyses after species insertion and pruning procedures.

# Scripts

### phylogenetic_diversity_analysis.R

R script used to:

* import and prune phylogenetic trees;
* insert missing species into the phylogeny;
* calculate phylogenetic diversity metrics;
* calculate PD, MPD, MNTD, PSV, and PSR indices.

Main R packages:

* ape
* picante
* geiger

### community_weighted_mean_analysis.R

R script used to calculate community-weighted means (CWM) and functional diversity metrics.

Main R packages:

* dplyr
* ggplot2
* lme4
* lmerTest
* tidyr
* readr
* FD

# Derived results

### community_weighted_mean_results.csv

Results of community-weighted mean analyses calculated from incidence matrices and functional trait matrices.

Note: This CSV file is delimited by semicolons (;), rather than commas. This formatting should be considered when importing the file into spreadsheet software or statistical environments (e.g., Excel, R, or Python) to ensure correct column separation and data interpretation.

Rows: Sampling areas.

Columns: Community-weighted mean values calculated for each functional trait.

CWM: Community-weighted mean.

# Sampling design

Ant communities were sampled in Neotropical savannas distributed across preserved cerrado, rural, and urban habitats in northern Minas Gerais, Brazil.

Sampling included arboreal and ground-dwelling ants collected in multiple sampling areas.

# Trait definitions

Morphological traits used in this study:

| Acronym | Description     | Unit |
| ------- | --------------- | ---- |
| WL      | Weber length    | mm   |
| EL      | Eye length      | mm   |
| LL      | Leg length      | mm   |
| ML      | Mandible length | mm   |
| SL      | Scape length    | mm   |

Trait descriptions follow Parr et al. (2017).

# Glossary of abbreviations and variables

| Acronym / Variable         | Definition                                                                              |
| -------------------------- | --------------------------------------------------------------------------------------- |
| WL                         | Weber length                                                                            |
| EL                         | Eye length                                                                              |
| LL                         | Leg length                                                                              |
| ML                         | Mandible length                                                                         |
| SL                         | Scape length                                                                            |
| CWM                        | Community-weighted mean                                                                 |
| PD                         | Phylogenetic diversity                                                                  |
| MPD                        | Mean pairwise phylogenetic distance                                                     |
| MNTD                       | Mean nearest taxon distance                                                             |
| PSV                        | Phylogenetic species variability                                                        |
| PSR                        | Phylogenetic species richness                                                           |
| FEve                       | Functional evenness                                                                     |
| FD                         | Functional diversity                                                                    |
| MFD                        | Mean functional diversity                                                               |
| Area                       | Sampling area/site                                                                      |
| Region                     | Geographic region used in the analyses                                                  |
| Status2                    | Habitat/environment type                                                                |
| Abundance                  | Number of individuals recorded per species                                              |
| incidence\_matrix          | Presence/absence matrix of ant species across sampling areas                            |
| community\_species\_matrix | Species abundance matrix used for phylogenetic analyses                                 |
| traits\_matrix             | Matrix containing morphological traits of ant species                                   |
| species\_metadata          | Taxonomic information for all analyzed species                                          |
| Arboreal ants (A)          | Ant species sampled in vegetation strata                                                |
| Ground-dwelling ants (G)   | Ant species sampled on the ground strata                                                |
| Rural                      | Anthropogenic rural habitat                                                             |
| Urban                      | Anthropogenic urban habitat                                                             |
| Open cerrado               | Preserved open savanna vegetation                                                       |
| Closed cerrado             | Preserved closed savanna vegetation                                                     |
| NA                         | Missing or unavailable data                                                             |
| GLMM                       | Generalized linear mixed model                                                          |
| R                          | Statistical programming language used in the analyses                                   |
| .nex                       | Nexus phylogenetic file format                                                          |
| Presence/absence data      | Binary data indicating whether species occur (1) or do not occur (0) in a sampling area |

# Missing values

Missing values are represented by NA.

# Software requirements

Analyses were performed in:

* R version 4.3.3

### phylogenetic_diversity__analysis.R

R script used to perform phylogenetic diversity analyses.

### community_weighted__mean_analysis.R

R script used to calculate community-weighted means (CWM) and functional diversity metrics.

# Related publication

Morais, P.N. et al. Functional and phylogenetic responses of arboreal ants to land-use change in Neotropical savannas. Insect Conservation and Diversity. In press.
