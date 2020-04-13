Here we share the raw data and the code to reproduce all the analysis of our study.

<b>Dataset.csv:</b>
“Dataset.csv” provides the raw data we used. Each line is an infection or a non-infected bird individual. That means that, individual birds detected with more than one parasite lineage (double and triple infections), are repeated in different lines. <br>
<br>
Information provided for birds are: Order, Family, Genus, and Species.
<br>
For the community is provided: Latitude, Longitude, and Biome.
<br>
For the parasite lineages: Genus (PL: Plamodium, HA: Haemoproteus, PA: Parahaemoproteus), Morphospecies, ID, Name, Accession, and nucleotide sequence of Cyt. B.
<br>
<br>
Dataset comprises data originally from 3 studies: 
<br>
<br>
Lacorte GA, Flix GMF, Pinheiro RRB, Chaves A V., Almeida-Neto G, Neves FS, Leite LO, Santos FR, Braga ÉM. 2013 Exploring the Diversity and Distribution of Neotropical Avian Malaria Parasites - A Molecular Survey from Southeast Brazil. PLoS One 8, 1–9. (doi:10.1371/journal.pone.0057770)
<br>
<br>
Fecchio A et al. 2018 Diversification by host switching and dispersal shaped the diversity and distribution of avian malaria parasites in Amazonia. Oikos 127, 1233–1242. (doi:10.1111/oik.05115)
<br>
<br>
Fecchio A et al. 2019 Avian host composition, local speciation and dispersal drive the regional assembly of avian malaria parasites in South American birds. Mol. Ecol. , mec.15094. (doi:10.1111/mec.15094)
<br>
<br>
<b>script1_exploring_dataset.R:</b><br>
Commented code to explore the dataset. Extract the basic information for each community (e.g., latitude, longitude, richness of bird species sampled, phylogenetic diversity of birds, etc…) and for each biome. Additionally, produces a map with all communities plotted and distinguished by biome (similarly to Figure 1 in the paper).
<br>
<br>
<b>script2_measuring_specialization:</b><br>
Commented code to calculate the specialization indices for each local community: connectance, H2’, and DSI.
<br>
<br>
<b>script3_correlations.R:</b><br>
Commented code to perform the correlations between specialization indices and latitude. Also perform the correlations using residuals of Generalized Linear Models where parasite and host richness are predictors and specialization is the response.
<br>
<br>
<b>script4_sem.R:</b><br>
Commented code to the modelling of the structural equation. It includes the building of the initial model and the reduction towards the minimum model. In the minimum model que calculated the effects as the explained deviance of each predictor in each response variable.
<br>
<br>
<b>com_data.RData:</b><br>
Table with information of each local community sampled (e.g., latitude and specialization indices values).
<br>
<br>
<b>biome_data.RData:</b><br>
Table with basic information of each biome (e.g., bird richness sampled)
<br>
<br>
<b>sem_effects.RData:</b><br>
Table with the effect (explained deviance) of each predictor in each response in the final structural equation model.
<br>
<br>
<b>bird_phylogeny.tree:</b><br>
Phylogeny of bird species within our database
