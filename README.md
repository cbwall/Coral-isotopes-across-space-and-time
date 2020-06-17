# Symbiont communities shape coral physiology and nutrition  

[![DOI](https://zenodo.org/badge/149528148.svg)](https://zenodo.org/badge/latestdoi/149528148)  
  
*CB Wall, M Kaluhiokalani, BN Popp, MJ Donahue, RD Gates. (2019). Divergent symbiont communities determine the physiology and nutrition of a reef coral across a light-availability gradient. The ISME Journal.* 
  
Reef corals are mixotrophic organisms relying on symbiont-derived photoautotrophy and water column heterotrophy. Coral endosymbionts (Family: Symbiodiniaceae), while typically considered mutualists, display a range of species-specific and environmentally mediated opportunism in their interactions with coral hosts, potentially requiring corals to rely more on heterotrophy to avoid declines in performance. To test the influence of symbiont communities on coral physiology (tissue biomass, symbiont density, photopigmentation) and nutrition (δ<sup>13</sup>C, δ<sup>15</sup>N), we sampled *Montipora capitata* colonies dominated by a specialist symbiont *Cladocopium* spp. or a putative opportunist *Durusdinium glynnii* (hereafter, C- or D-colonies) from Kāne‘ohe Bay, Hawai‘i, across gradients in photosynthetically active radiation (PAR) (1 - 10 m depth) during summer and winter.
  
  
### Directory information   
  
#### PanKBay-isotopes_pub.html  
*html file for Rmd output*  
  
#### PanKBay-isotopes_pub.Rmd  
*R markdown for project*  
  
#### PanKBay isotopes.Rproj  
*R project to run Rmd and directory*  
  
#### DATA   *houses the following folders and files*  
 **coast_n83.shp** : shape files for making site maps  
 **environmental** :  
   - folder **sea level** for sea-level correction  
       *columns*  
        *stationID* = Moku o Lo'e weather station   
        *datum* = mean sea level (MSL)  
        *TimeUTC* = time in UTC  
        *TideHT* = tidal height in meters  
        (other columns NA)  
        
   - folder **temp and light** for raw data collected on the reef  
   - *PanKBay_nutrients.csv* for dissolved inorganic nutrients  
       *columns*  
        *Date* = date of collection  
        *Reef* = reef IDs, F=fringe, R=patch reef  
        *Location* = northwest NW, northeast NE, central west CW, southeast SE, southwest SW  
        *phosphate, silicate, N+N (nitrate+nitrite), ammonium* = all in μmol L-1  
      
   - *Reefs_lat_long.csv* for mapping sampling sites  
      *columns*  
       *Site* = Reef ID as cardinal directions and Reef number  
       *latitude and longitude* = in decimal degrees  
     
 **qPCR**
  - eds files (for StepOne Platform) and associated .txt files with qPCR plate reads  
  - *isotopes_SW_all times.csv*  
    seawater and plankton isotopes collected at the sample sites in each season  
     *columns*  
      *Season* = summer or winter (northern hemisphere)  
      *Sampling.date* = date as "yyyymmdd"  
      *Reef* = F for fringe reef, R for patch reef, and reef number  
      *Location* = NW, SE, NE, SW for north (N) south (S) and east (E) or west (W)  
      *Region* = northern or southern Kāne'ohe Bay  
      *Habitat.type* = fringe or patch reef  
      *North.South.ID* = arbitrary ID #  
      *SW.fraction..um* = the size fraction of seawater in μm  
      *ug.N* = ug of nitrogen  
      *d15N* = δ<sup>15</sup>N in permil  
      *ug.C* = ug of carbon  
      *d13C* = δ<sup>13</sup>C in permil  
      
  - *mastersheet_PanKBAY.csv*   
    all response variables from physiology and isotope analyses (prior to calculation of C-D dominance)  
      *columns*  
      *Date*  = date of collection as mm/dd/yy  
      *Season* = summer or winter (northern hemisphere)  
      *Reef* = F for fringe reef, R for patch reef, and reef number  
      *Location* = NW, SE, NE, SW for north (N) south (S) and east (E) or west (W)  
      *Reef.type* = fringe or patch reef 
      *Bay.region* = northern or southern Kāne'ohe Bay   
      *Time.of.collection* = time for tidal correction  
      *Depth..m* = colony depth in meters  
      *Sample.ID* = arbitrary sample ID  
      *total.blastate.ml* = total tissue extrat (blastate) in mL  
      *surface.area.cm2* = colony surface area in cm2  
      *cells.ml* = symbiont densities in cells per mL  
      *ug.chl.a.ml* = μg of chlorophyll a per ml blastate  
      *ug.chl.c2.ml* = μg of chlorophyll a per ml blastate  
      *mg.biomass.ml* = mg of host biomass per ml blastate  
      *host..mass.mg* = lyophilized host biomass for isotope analysis  
      *host..ug.N* = μg of nitrogen in host tissue  
      *host..d15N* = δ15N of host tissues in permil  
      *host..ug.C* = μg of carbon in host tissue  
      *host..d13C* = δ13C of host tissues in permil  
      *host..C.N* = molar ratio of carbon : nitrogen in symbiont tissues  
      *symb..mass.mg* = lyophilized symbiont biomass for isotope analysis  
      *symb..ug.N* = μg of nitrogen in symbiont tissue  
      *symb..d15N* = δ15N of symbiont tissues in permil  
      *symb..ug.C* = μg of carbon in host tissue  
      *symb..d13C* = δ13C of symbiont tissues in permil  
      *symb..C.N* = molar ratio of carbon : nitrogen in symbiont tissues  
      *d15N..host.symb* = difference in δ15N of host minus symbiont in permil  
      *d13C..host.symb* = difference in δ13C of host minus symbiont in permil  
      *d13C..skel* = δ13C of coral skeletal carbonates in permil  
      *d18O.skel* = δ18O of coral skeletal carbonates in permil  

  - *PanKBay_summer_qPCR.csv*  
    compiled qPCR data for summer samples (post pipeline [detailed in Rmd file])  
    
  - *PanKBay_winter_qPCR*  
    compiled qPCR data for winter samples (post pipeline [detailed in Rmd file])  
      *columns*  
      *Colony* = arbitrary ID  
      *Date* = mm/dd/yy  
      *Depth..m* = coral depth in meters  
      *Latitude and Longitude* = in decimal degrees  
      *Time* = hh:mm  
      *Season* = summer and winter (northern hemisphere)  
      *Reef.type* = fringe or patch reef   
      *Bay.region* = northern or southern Kāne'ohe Bay    
      *Reef* = reef ID  
      *Location* = NE, NW, SE, SW  
      *Sample.ID* = arbitrary ID for colony at each reef location  
      *File.Name* = the qPCR file where data originated  
      *C.CT.mean* = mean CT value for 2 reps for *Cladocopium*    
      *D.CT.mean* = mean CT value for 2 reps for *Durusdinium*  
      *C.CT.sd* = standard deviation CT value for 2 reps for *Cladocopium*  
      *D.CT.sd* = standard deviation CT value for 2 reps for *Durusdinium*  
      *propC* = proportion of community as percentage *Cladocopium*  
      *propD* = proportion of community as percentage *Durusdinium*  
      *Dom* = dominant symbiont at > 50%  
      *Mix* = mixture of symbionts as all C, all D, D>C or C>D  
     
#### FIGURES  *houses exported figures*
**environmental** :
  - folder: **unused** are figures used in analysis but not published   
   - pdfs for daily light integral (DLI) and photosynthetically active radiation (PAR) at the 2m depth only (for comparison)  
   - pdf for coefficent plots for DLI and PAR fitting  
          
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

#### OUTPUT   *houses exported outputs and compiled data*
   - *C_D sample size.csv* (sample size for C- and D-dominated corals)  
   - *data.all.csv* (all physiology, isotope, and symbiont community data)  
   - *kd.all.csv* (light attenuation coefficients for each site)  
   - *Light attenuation_table.csv* (light DLI mean +/-SE at <1m and attenuation relative to this at 2m and 8m-depth)  
   - *light coeffs_factor.csv* (coefficients of models for ligh-at-depth calculations)  
   - *season.DLI.csv* (DLI mean +/- SE at each site, in each season)  
   
   
   
 
