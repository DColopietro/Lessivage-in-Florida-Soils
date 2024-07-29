# Lessivage-in-Florida-Soils
Determining the depth and degree of lessivage (clay translocation) in Florida soils using the FSSC database and the USDA Soil Taxonomy definiton of an Argillic horizon. 

To differentiate textural changes made from distinct depositional events from pedogenically-induced changes called lessivage, we used the uniformity index (UI) created by Creemans and Mokma (1986). To further support the UI analysis, we looked at the sand size distribution by horizon for each profile which was described as having an argillic horizon. If no depositional event occurred, then the difference between the sand size distribution should be small, since quartz is highly resistant to weathering and the sand fraction is immobile.

The depth of lessivage was defined as the top of the argillic horizon (illuvial zone). 

The degree of lessivage was calculated with a modified I/E Index (Cremeens & Mokma, 1986) on the basis the profile developed from a single parent material.

Random forest classification and regression was used with the implementation of the R package ‘randomForest’ version 4.7-1.1 (Liaw & Wiener, 2022) in the base environment of R version 4.1.0 (R Core Team, 2021). 

To investigate the importance of soil properties, the variable importance (VI) was calculated using the R package ‘pdp’ version 0.8.1 (Greenwell, 2022).

The assumption of independence for PDPs was checked and the resultant correlation matrix using Pearson’s correlation was created using the R package ‘corrplot’ version 0.92 (Wei et al., 2021). 
