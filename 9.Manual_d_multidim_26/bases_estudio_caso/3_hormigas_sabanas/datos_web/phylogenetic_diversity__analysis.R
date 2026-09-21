rm(list= ls())

library(ape)
library(picante)
library(geiger)


# Read the Nexus file
### File used: "moreau_bell_phylogeny_2013.nex"
tree <- read.nexus("moreau_bell_phylogeny_2013.nex", tree.names = NULL, force.multi = FALSE)
tree1<-tree[["Moreau&Bell_Likelihood_partitioned"]]
tree1

## Names
names(tree1)

##Plots
plot(tree[["Moreau&Bell_Likelihood_partitioned"]],main = "Imported Phylogeny")


plot.phylo (tree[["Moreau&Bell_Likelihood_partitioned"]], type = "phylogram", show.tip.label = TRUE, 
            show.node.label = TRUE, edge.color = "black", edge.width = 1.5, 
            tip.color = "black", cex = 0.45, label.offset = 2) 

plot.phylo (tree[["Moreau&Bell_Likelihood_partitioned"]], type = "fan", show.tip.label = TRUE, 
            show.node.label = TRUE, edge.color = "blue", edge.width = 1.5, 
            tip.color = "black", cex = 0.45, label.offset = 2) 

## Species names - to see how many species are in the original phylogeny
tree1$tip.label

###List of new species to be inserted (species not present in the original phylogeny)
Ectatomma <- c("Ectatomma_tuberculatum")
Gnamptogenys <- c("Gnamptogenys_sp.1")
Megalomyrmex <- c("Megalomyrmex_sp.1")
Solenopsis <- c("Solenopsis_sp.1")
Atta <- c("Atta_sp.1", "Atta_sp.2")
Azteca <- c("Azteca_sp.1", "Azteca_sp.2", "Azteca_sp.3", "Azteca_sp.4", "Azteca_sp.5")
Brachymyrmex <- c("Brachymyrmex_sp.1", "Brachymyrmex_sp.2", "Brachymyrmex_sp.3")
Camponotus <- c("Camponotus_sp.1", "Camponotus_sp.10", "Camponotus_sp.11", "Camponotus_sp.12", "Camponotus_sp.2", "Camponotus_sp.3", "Camponotus_sp.4", "Camponotus_sp.5", "Camponotus_sp.6", "Camponotus_sp.7", "Camponotus_sp.8", "Camponotus_sp.9")
Cephalotes <- c("Cephalotes_atratus", "Cephalotes_betoi", "Cephalotes_brogmeiri", "Cephalotes_depressus", "Cephalotes_grandinosus", "Cephalotes_liepini", "Cephalotes_maculatus", "Cephalotes_minutus", "Cephalotes_pellans", "Cephalotes_persimilis", "Cephalotes_pusilhos", "Cephalotes_sp.11")
Crematogaster <- c("Crematogaster_sp.1", "Crematogaster_sp.2", "Crematogaster_sp.3", "Crematogaster_sp.4", "Crematogaster_sp.5", "Crematogaster_sp.6")
Dolichoderus <- c("Dolichoderus_sp.1", "Dolichoderus_sp.2", "Dolichoderus_sp.3")
Dorymyrmex <- c("Dorymyrmex_sp.1", "Dorymyrmex_sp.2", "Dorymyrmex_sp.3", "Dorymyrmex_sp.4", "Dorymyrmex_sp.5", "Dorymyrmex_sp.6", "Dorymyrmex_sp.7", "Dorymyrmex_sp.8", "Dorymyrmex_sp.9")
Myrmelachista <- c("Myrmelachista_sp.1", "Myrmelachista_sp.2")
Pseudomyrmex <- c("Pseudomyrmex_termitarius", "Pseudomyrmex_kuenckeli","Pseudomyrmex_sp.1", "Pseudomyrmex_sp.2", "Pseudomyrmex_sp.3", "Pseudomyrmex_sp.4", "Pseudomyrmex_sp.5", "Pseudomyrmex_sp.6", "Pseudomyrmex_sp.7", "Pseudomyrmex_sp.8", "Pseudomyrmex_sp.9", "Pseudomyrmex_tenuis")

#### Adding the new species to the phylogeny - first add those with only one new species per genus
new_phylogeny <- add.species.to.genus(force.ultrametric(tree1, message = FALSE), Ectatomma)
new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE), Gnamptogenys)
new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE), Megalomyrmex)
new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE), Solenopsis)

### Names in the phylogeny - to see how many names are in the new phylogen
new_phylogeny$tip.label

#### Adding the new species to the phylogeny - now add those with two or more new species per genus - for this we use a loop
for(i in 1:length(Atta)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE), Atta[i], where = "root")

for(i in 1:length(Azteca)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                          Azteca[i], where = "root")

for(i in 1:length(Brachymyrmex)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                          Brachymyrmex[i], where = "root")

for(i in 1:length(Camponotus)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                          Camponotus[i], where = "root")

for(i in 1:length(Cephalotes)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                          Cephalotes[i], where = "root")

for(i in 1:length(Crematogaster)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                          Crematogaster[i], where = "root")

for(i in 1:length(Dolichoderus)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                           Dolichoderus[i], where = "root")

for(i in 1:length(Dorymyrmex)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                           Dorymyrmex[i], where = "root")

for(i in 1:length(Myrmelachista)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                           Myrmelachista[i], where = "root")

for(i in 1:length(Pseudomyrmex)) 
  new_phylogeny <- add.species.to.genus(force.ultrametric(new_phylogeny, message = FALSE),
                                           Pseudomyrmex[i], where = "root")

### Names in the phylogeny - to see how many names are in the new phylogeny
new_phylogeny$tip.label

### Names in the original phylogeny
tree1$tip.label


####NOW WE NEED TO REMOVE OTHER SPECIES FROM THE PHYLOGENY AND KEEP ONLY THE STUDY SPECIES
pruned_phylogeny <- drop.tip(new_phylogeny, c(
    "Acanthognathus_ocellatus",
    "Acanthomyops_laticeps_CSM",
    "Acanthoponera_minor",
    "Acanthostichus_kirbyi",
    "Acromyrmex_versicolor",
    "Acropyga_acutiventris",
    "Acropyga_epedana_CSM",
    "Adetomyrma_MAD02",
    "Adetomyrma_sp.2b_CSM",
    "Aenictogiton_ZAM02",
    "Aenictus_ceylonicus",
    "Aenictus_eugenii",
    "Aenictus_sp.1_CSM",
    "Aenictus_sp.2_CSM",
    "Aglyptacros_cf_sulcatus",
    "Amblyopone_mutica",
    "Amblyopone_pallipes",
    "Amblyopone_pallipes_CSM",
    "Aneuretus_simoni",
    "Aneuretus_simoni_CSM",
    "Anochetus_madagascarensis",
    "Anochetus_mayri_CSM",
    "Anonychomyrma_gilberti",
    "Anonychomyrma_nitidiceps_CSM",
    "Anoplolepis_gracilipes",
    "Anoplolepis_gracillipes_CSM",
    "Aphaenogaster_albisetosa",
    "Aphaenogaster_occidentalis",
    "Aphaenogaster_swammerdami",
    "Aphaenogaster_texana_CSM",
    "Apis_mellifera",
    "Apomyrma_stygia",
    "Apomyrma_stygia_CSM",
    "Aporus_niger",
    "Apterostigma_auriculatum",
    "Apterostigma_sp._CSM",
    "Atopomyrmex_mocquerysi_CSM",
    "Atta_sp._CSM",
    "Azteca_ovaticeps",
    "Azteca_sp._CSM",
    "Basiceros_manni",
    "Bothriomyrmex_hispanicus_CSM",
    "Brachymyrmex_depilis",
    "Brachymyrmex_sp._CSM",
    "Calomyrmex_albertisi",
    "Calyptomyrmex_beccari_CSM",
    "Camponotus_BCA01",
    "Camponotus_conithorax",
    "Camponotus_hyatti",
    "Camponotus_maritimus",
    "Camponotus_ocreatus_CSM",
    "Cardiocondyla_emeryi_CSM",
    "Cardiocondyla_mauritanica",
    "Carebara_sp._CSM",
    "Cataglyphis_ibericus_CSM",
    "Cataulacus_MAD02",
    "Cataulacus_sp._CSM",
    "Centromyrmex_feae_CSM",
    "Centromyrmex_sellaris",
    "Cephalotes_unimaculatus_CSM",
    "Cerapachys_augustae",
    "Cerapachys_augustae_CSM",
    "Cerapachys_larvatus",
    "Cerapachys_sexspinus",
    "Cerapachys_Yunodorylus_sexspinus_CSM",
    "Chalybion_californicum",
    "Cheliomyrmex_cf_morosus",
    "Chyphotes_mellipes",
    "Chyphotes_sp._CSM",
    "Colobostruma_unicorna_CSM",
    "Concoctio_concenta",
    "Concoctio_concenta_CSM",
    "Crematogaster_emeryana",
    "Crematogaster_navajoa_CSM",
    "Cryptopone_gilva_CSM",
    "Cylindromyrmex_striatus",
    "Cyphomyrmex_sp._CSM",
    "Daceton_armigerum",
    "Dasymutilla_aureola",
    "Diacamma_sp._CSM",
    "Dilobocondyla_sp._CSM",
    "Dinoponera_gigantea_CSM",
    "Discothyrea_MAD07",
    "Discothyrea_sp._CSM",
    "Dolichoderus_imitator_CSM",
    "Dolichoderus_scabridus",
    "Dorylus_helvolus",
    "Dorylus_laevigatus",
    "Dorylus_mayri_CSM",
    "Dorylus_wilverthi_CSM",
    "Dorymyrmex_bicolor",
    "Dorymyrmex_elegans_CSM",
    "Eciton_hamatum_CSM",
    "Eciton_vagans",
    "Ectatomma_opaciventre",
    "Ectatomma_quadridens_CSM",
    "Eurhopalothrix_bolaui",
    "Eurhopalothrix_sp._CSM",
    "Eutetramorium_mocquerysi",
    "Eutetramorium_sp._CSM",
    "Evagetes_sp._CSM",
    "Forelius_pruinosus",
    "Forelius_sp._CSM",
    "Formica_moki",
    "Formica_wheeleri_CSM",
    "Formicoxenus_provancheri_CSM",
    "Froggattella_latispina_CSM",
    "Gnamptogenys_sp._CSM",
    "Gnamptogenys_striatula",
    "Goniomma_hispanicum_CSM",
    "Heteroponera_microps_CSM",
    "Heteroponera_panamensis",
    "Heteroponera_panamensis_CSM",
    "Hypoponera_inexorata_CSM",
    "Hypoponera_opacior",
    "Hypoponera_sakalava",
    "Iridomyrmex_spadius_CSM",
    "Labidus_spininodis_CSM",
    "Lasius_alienus_CSM",
    "Lasius_californicus",
    "Leptanilla_GRE01",
    "Leptanilla_RSA01",
    "Leptanilla_sp._CSM",
    "Leptanilloides_mckennae",
    "Leptanilloides_nomada",
    "Leptanilloides_nomada_CSM",
    "Leptanilloides_nubecula_CSM",
    "Leptogenys_diminuta",
    "Leptogenys_sp._CSM",
    "Leptomyrmex_AUS01",
    "Leptomyrmex_erythrocephalus",
    "Leptomyrmex_sp._CSM",
    "Leptothorax_muscorum_complex",
    "Leptothorax_sp._CSM",
    "Linepithema_humile",
    "Linepithema_keiteli_CSM",
    "Liometopum_apiculatum",
    "Liometopum_luctuosum_CSM",
    "Liometopum_occidentale",
    "Loboponera_politula",
    "Lophomyrmex_striatulus_CSM",
    "Manica_bradleyi",
    "Martialis_heureka_CR",
    "Mayriella_ebbei",
    "Mayriella_transfuga_CSM",
    "Megalomyrmex_latreillei_CSM",
    "Melissotarsus_sp._CSM",
    "Melophorus_sp._CSM",
    "Meranoplus_cf_radamae",
    "Meranoplus_mayri_CSM",
    "Messor_andrei",
    "Messor_denticornis",
    "Messor_julianus_CSM",
    "Metapolybia_cingulata",
    "Metapone_madagascarica",
    "Metapone_madagascarica_CSM",
    "Microdaceton_tibialis",
    "Mischocyttarus_flavitarsis",
    "Monomorium_destructor_CSM",
    "Monomorium_ergatogyna",
    "Myopias_lobosa_CSM",
    "Myopopone_castanea_CSM",
    "Myrcidris_epicharis",
    "Myrmecia_fulviculis_CSM",
    "Myrmecia_pyriformis",
    "Myrmecina_graminicola",
    "Myrmecocystus_flaviceps",
    "Myrmecocystus_mexicanus_CSM",
    "Myrmecorhynchus_sp._CSM",
    "Myrmelachista_JTL01",
    "Myrmelachista_sp._CSM",
    "Myrmica_incompleta_CSM",
    "Myrmica_striolagaster",
    "Myrmica_tahoensis",
    "Myrmicaria_brunnea_CSM",
    "Myrmicaria_exigua",
    "Myrmicocrypta_cf_infuscata",
    "Myrmoteras_iriodum",
    "Myrmoteras_williamsi_CSM",
    "Mystrium_mysticum",
    "Mystrium_rogeri_CSM",
    "Neivamyrmex_nigrescens",
    "Neivamyrmex_nigrescens_CSM",
    "Nesomyrmex_echinatinodis",
    "Nomamyrmex_esenbecki_CSM",
    "Nothomyrmecia_macrops",
    "Notoncus_capitatus",
    "Notoncus_sp._CSM",
    "Notostigma_carazzii",
    "Ochetellus_glaber_CSM",
    "Ocymyrmex_picardi_CSM",
    "Odontomachus_clarus_CSM",
    "Odontomachus_coquereli",
    "Odontophotopsis_sp._CSM",
    "Odontoponera_transversa",
    "Odontoponera_transversa_CSM",
    "Oecophylla_smaragdina",
    "Oecophylla_smaragdina_CSM",
    "Oligomyrmex_sp._CSM",
    "Onychomyrmex_hedleyi",
    "Onychomyrmex_hedleyi_CSM",
    "Opisthopsis_respiciens",
    "Opisthopsis_sp._CSM",
    "Orectognathus_versicolor",
    "Oxyopomyrmex_insularis_CSM",
    "Pachycondyla_sikorae",
    "Pachycondyla_stigma_CSM",
    "Papyrius_nitidus",
    "Papyrius_nitidus_CSM",
    "Paraponera_clavata",
    "Paraponera_clavata_CSM",
    "Paratrechina_hystrix",
    "Paratrechina_sp._CSM",
    "Pheidole_clydei",
    "Pheidole_hyatti",
    "Pheidole_rhea_CSM",
    "Pheidologeton_affinis",
    "Pheidologeton_sp._CSM",
    "Philanthus_sp._CSM",
    "Philidris_cordatus",
    "Philidris_cordatus_CSM",
    "Pilotrochus_besmerus",
    "Plagiolepis_sp._CSM",
    "Platythyrea_mocquerysi",
    "Platythyrea_punctata",
    "Platythyrea_punctata_CSM",
    "Plectroctena_ugandensis",
    "Podomyrma_sp._CSM",
    "Pogonomyrmex_maricopa_CSM",
    "Pogonomyrmex_subdentatus",
    "Polyergus_breviceps",
    "Polyergus_briviceps_CSM",
    "Polyrhachis_c.f._vindex_CSM",
    "Polyrhachis_Cyrto01",
    "Polyrhachis_Hagio01",
    "Prenolepis_albimaculata",
    "Prenolepis_imparis",
    "Prenolepis_imparis_CSM",
    "Prionopelta_MAD01",
    "Prionopelta_sp.2_CSM",
    "Pristocera_MAD01",
    "Pristomyrmex_sp._CSM",
    "Proatta_butteli_CSM",
    "Probolomyrmex_sp._CSM",
    "Probolomyrmex_tani",
    "Proceratium_MAD08",
    "Proceratium_sp.1_CSM",
    "Proceratium_stictum",
    "Procryptocerus_batesi_CSM",
    "Procryptocerus_scabriusculus",
    "Proformica_nasuta_CSM",
    "Prolasius_sp._CSM",
    "Protanilla_JAP01",
    "Protanilla_sp._CSM",
    "Psalidomyrmex_procerus",
    "Pseudolasius_australis",
    "Pseudolasius_typhlops_CSM",
    "Pseudomyrmex_apache_CSM",
    "Pyramica_hoplites",
    "Pyramica_pulchella_CSM",
    "Rhopalomastix_janeti_CSM",
    "Rhopalomastix_rothneyi",
    "Rhytidoponera_chalybaea",
    "Rhytidoponera_metallica_CSM",
    "Sapyga_pumila",
    "Scolia_verticalis",
    "Sericomyrmex_sp._CSM",
    "Simopelta_cf_pergandei",
    "Simopone_marleyi",
    "Solenopsis_invicta_CSM",
    "Solenopsis_molesta",
    "Solenopsis_xyloni",
    "Sphinctomyrmex_sp._CSM",
    "Sphinctomyrmex_steinheili",
    "Stenamma_dyscheres",
    "Stenamma_snellingi_CSM",
    "Stigmacros_sp._CSM",
    "Strumigenys_dicomas",
    "Strumigenys_sp._CSM",
    "Tapinoma_opacum_CSM",
    "Tapinoma_sessile",
    "Tatuidris_ECU01",
    "Tatuidris_tatusia_CSM",
    "Technomyrmex_albipes_CSM",
    "Technomyrmex_difficilis",
    "Technomyrmex_MAD05",
    "Temnothorax_rugatulus",
    "Temnothorax_tricarinatus_CSM",
    "Terataner_MAD02",
    "Terataner_sp._CSM",
    "Tetramorium_caespitum",
    "Tetramorium_hispidum_CSM",
    "Tetramorium_validiusculum",
    "Tetraponera_punctulata",
    "Tetraponera_rufonigra",
    "Tetraponera_sp._CSM",
    "Thaumatomyrmex_atrox",
    "Trachymyrmex_arizonensis",
    "Trachymyrmex_jamaicensis_CSM",
    "Tranopelta_subterranea_CSM",
    "Turneria_bidentata",
    "Typhlomyrmex_rogenhoferi",
    "Typhlomyrmex_rogenhoferi_CSM",
    "Vespula_sp._CSM",
    "Vollenhovia_emeryi",
    "Vollenhovia_sp._CSM",
    "Wasmannia_auropunctata",
    "Wasmannia_sp._CSM",
    "Xenomyrmex_floridanus",
    "Xenomyrmex_floridanus_CSM"))
pruned_phylogeny

#NAMES IN THE TRIMMED PHYLOGENY - FINAL PHYLOGENY WITH ONLY OUR SPECIE
pruned_phylogeny$tip.label

##Plots
plot(pruned_phylogeny, cex = 0.5, no.margin = TRUE)

plot.phylo (pruned_phylogeny, type = "phylogram", show.tip.label = TRUE, 
            show.node.label = TRUE, edge.color = "black", edge.width = 1.5, 
            tip.color = "black", cex = 0.45, label.offset = 2) 

plot.phylo (pruned_phylogeny, type = "fan", show.tip.label = TRUE, 
            show.node.label = TRUE, edge.color = "blue", edge.width = 1.5, 
            tip.color = "black", cex = 0.45, label.offset = 2) 

#OPEN THE SPECIES LIST by AREAS - species by area matrix
### File used: "community_species_matrix.txt"
area_data <- read.table("community_species_matrix.txt", h=T)
area_data

## Check if the species names in the phylogeny and in the matrix are correct
name.check(pruned_phylogeny, t(area_data))

## Arrange the species names in the data frame in the same order as they appear in the phylogeny
area_data_P <- match.phylo.comm(phy = pruned_phylogeny, comm = area_data)$comm

## Phylogenetic diversity (PD)
# Calculating the phylogenetic diversity metric proposed by Faith (1992)
PD_results <- pd(area_data_P, pruned_phylogeny)
PD_results

## Phylogenetic Species Richness (PSR)
# Analysis using species composition data in the communities.
PSR_results <- psr(area_data_P, pruned_phylogeny)
PSR_results

## Mean Pairwise Distance (MPD)
# Analysis using species incidence data in the communities.
MPD_PA_results <- mpd(area_data_P, cophenetic(pruned_phylogeny), 
                         abundance.weighted = FALSE)
MPD_PA_results 


## Mean Nearest Taxon Distance (MNTD)
# Analysis using species presence-absence data in the communities.
MNTD_PA_results <- mntd(area_data_P, cophenetic(pruned_phylogeny), 
                           abundance.weighted = FALSE)
MNTD_PA_results 

## Phylogenetic Species Variability (PSV)
# Analysis using species presence-absence data in the communities.
PSV_results <- psv(area_data_P, pruned_phylogeny)
PSV_results 
