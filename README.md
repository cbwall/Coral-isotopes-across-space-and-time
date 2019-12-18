# Coral-isotopes-across-space-and-time  
  
*CB Wall, M Kaluhiokalani, BN Popp, MJ Donahue, RD Gates. (2019). Divergent symbiont communities determine the physiology and nutrition of a reef coral across a light-availability gradient. The ISME Journal.* 
  
Reef corals are mixotrophic organisms relying on symbiont-derived photoautotrophy and water column heterotrophy. Coral endosymbionts (Family: Symbiodiniaceae), while typically considered mutualists, display a range of species-specific and environmentally mediated opportunism in their interactions with coral hosts, potentially requiring corals to rely more on heterotrophy to avoid declines in performance. To test the influence of symbiont communities on coral physiology (tissue biomass, symbiont density, photopigmentation) and nutrition (δ<sup>13</sup>C, δ<sup>15</sup>N), we sampled *Montipora capitata* colonies dominated by a specialist symbiont *Cladocopium* spp. or a putative opportunist *Durusdinium glynnii* (hereafter, C- or D-colonies) from Kāne‘ohe Bay, Hawai‘i, across gradients in photosynthetically active radiation (PAR) (1 - 10 m depth) during summer and winter.
  
  
### Directory information   
  
#### PanKBay-isotopes_pub.html  
*html file for Rmd output*  
  
#### PanKBay-isotopes_pub.Rmd  
*R markdown for project*  
  
#### PanKBay isotopes.Rproj  
*R project to run Rmd and directory*  
  
#### DATA 
*houses the following folders and files*  
  
 **coast_n83.shp** : shape files for making site maps  
 **environmental** :  
   - folder **sea level** for sea-level correction  
   - folder **temp and light** for raw data collected on the reef  
   - 'PanKBay_nutrients.csv' for dissolved inorganic nutrients  
   - 'Reefs_lat_long.csv' for mapping sampling sites  
     
 **qPCR**
  - eds files (for StepOne Platform) and associated .txt files with qPCR plate reads  
  - *isotopes_SW_all times.csv*  
    - seawater and plankton isotopes collected at the sample sites in each season  
  - *mastersheet_PanKBAY.csv*  
    - all response variables from physiology and isotope analyses (prior to calculation of C-D dominance)  
  - *PanKBay_summer_qPCR.csv*  
    - compiled qPCR data for summer samples (post pipeline [detailed in Rmd file])  
  - *PanKBay_winter_qPCR*  
    - compiled qPCR data for winter samples (post pipeline [detailed in Rmd file])  
     
#### FIGURES ####  *houses exported figures*  
  
**environmental** :  
  - folder: **unused** are figures used in analysis but not published  
     - pdfs:  
      - daily light integral (DLI) and photosynthetically active radiation (PAR) at the 2m depth only (for comparison)  
      - coefficent plots for DLI and PAR fitting  
          
  - *all.nutrients.pdf* (DIN)  
  - *DLI.bar.pdf* (mean +/-SE bar plot)  
  - *DLIcalc.alldepths.pdf* (DLI at 3 depth zones)  
  - *iso.sources.KBay.pdf* (mean +/- SE, isotope sources, end members)  
  - *KBaymap.pdf* (map and sites)  
  - *Temp.allsites.pdf* (temperature at 2m for each site)  
   
 **isotopes** :  
  - *isotope_multipanel.pdf* (carbon isotope figures)  
  - *suppm.CNboxplot.pdf* (boxplots for C:N data, host and symbionts)  
  - *suppm.Cskel.pdf* (skeletal carbon and oxygen isotope figures)  
  - *suppm.nitrogen.pdf* (nitrogen isotope figures)  
    
 **models** :  
  - *variance.pdf* (model random effect [location] proportion of variance)  
    
 **multivariate** :  
  - *PCx4.pdf* (PCA with 4 plots of differing categories)  
   
 **physiology** :   
  - *physio_multipanel.pdf* (multipanel of all physiology metrics)  
    
 **regressions** :  
  - *symb.isotope_by.symb.phys.pdf* (regression of symbiont physiology vs. symbiont carbon isotope values)  
  - *symb.by_HS.d13C.pdf* (regression of symbiont physiology vs. carbon host-symbiont isotopes values)  
    
 **symbionts** :  
  - *Light by Depth_symbs.pdf* (calculations for DLI at the coral surface)  
  - *Symb_Season_Light_logistic.pdf* (logistic regression histograms of symbiont distributions)  
  - *Symbionts_by_Season_depth.pdf* (probability of *Durusdinium* across depth)  
      
  *M capitata_KBay_Wall.png* (coral figure)

#### OUTPUT 
*houses exported outputs and compiled data*
   - *C_D sample size.csv* (sample size for C- and D-dominated corals)  
   - *data.all.csv* (all physiology, isotope, and symbiont community data)  
   - *kd.all.csv* (light attenuation coefficients for each site)  
   - *Light attenuation_table.csv* (light DLI mean +/-SE at <1m and attenuation relative to this at 2m and 8m-depth)  
   - *light coeffs_factor.csv* (coefficients of models for ligh-at-depth calculations)  
   - *season.DLI.csv* (DLI mean +/- SE at each site, in each season)  
   
   
   
 
