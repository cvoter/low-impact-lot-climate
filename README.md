# low_impact_lot_climate
Materials supporting submitted manuscript about how climate regulates the hydrologic outcomes of infiltration-based LID practices.

Please also see https://github.com/cvoter/PFscripts for files used to run ParFlow on UW-Madison Center for High Throughput Computing resources.

Throughout this README, emphasis is used to highlight **filenames** and *variables*.

# data

## data/climate
Includes 30 years of climate information for each city (WY1981-WY2010, starting time 00:00 UTC October 1, 1980). Each subdirectory is named after the city population rank (with the form 'loc%02d') and includes a file **nldas.1hr.clm.txt** with hourly meteorological parameters. Columns are 1) *DSWR*, shortwave radiation (W/m<sup.2</sup>), 2) *DLWR*, longwave radiation (W/m<sup.2</sup>), 3) *APCP*, precipitation (mm/s), 4) *Temp*, air temperature (K), 5) *UGRD*, east-west wind speed (m/s), 6) *VGRD*, north-south wind speed (m/s), 7) *Press*, atmospheric pressure (pa), 8) *SPFH*, specific humidity (kg/kg).

## data/colormaps
Includes saved color map schemes used to visualize inputs and outputs in Matlab.

## data/initial_pressure
Includes initial pressure (m) conditions for silt loam (SiL) and highly-compacted silt loam (SiL10c) baseline soil conditions in each of 51 cities explored (denoted with *loc*). Each **\*.mat** file includes the following variables:  
  * *pWY*: complete record of pressure head (m) on October 1 for 300 year spinup simulation  
  * *pWY30*: record of pressure head (m) on October 1 at end of each 30-year loop of weather (i.e., every 30 years)  
  * *sWY*: as pSP, but saturation (-)  
  * *sWY30*: as pSP30, but saturation (-)  
  * *wyIC*: pressure head on October 1 of last simulated year, used as initial conditions for model  

## data/layouts
Includes information about 2D layout of parcel features and microtopography elevations for **baseline** and **low_impact** conditions. Taken from data/layouts/Lot0000 and data/layouts/Lot1111 in my related repository, "low_impact_lot_practices". Files in each subdirectory include:  
  * **domainInfo.mat**: domain information including the following variables:
    * *dx*,*dy*,*dz*: discretization in x, y, z (0.5m horizontal, 0.1m vertical)
    * *nx*,*ny*,*nz*: number of elements in x, y, z (44, 75, 100)
    * *x*,*y*,*z*: vectors with the center x, y, or z coordinate of each element
    * *P*,*Q*,*R*: number of processors in x, y, z (4, 5, 1)
    * *domainArea*: surface area (m<sup>2</sup>) of domain
    * *elev*,*DScalc*: final elevation (m) for each pixel and calculated depression storage based on random roughness approach (Onstad, 1984)
    * *parcelCover*,*fc*: parcel cover indicating feature cover and coordinates of the impervious features (see data/layouts)
    * *NaNimp*,*pervX*,*pervY*: matrix with NaNs at impervious pixels, for viewing results as well as row and column of random pervious element (sometimes helpful to have during post-processing visualization)
    * *slopeX*,*slopeY*: matrices with slope in x and slope in y directions
  * **parameters.txt**: text file used to convey parameters to **runParflow.tcl** executable. Printed info (by line) includes 1) *xL* 2) *yL* 3) *zL* 4) *nx* 5) *ny* 6) *nz* 7) *dx* 8) *dy* 9) *dz* 10) *xU* 11) *yU* 12) *zU* 13) *P* 14) *Q* 15) *R*
  * **slopex.sa**, **slopey.sa**, **subsurfaceFeature.sa**: same as matlab variables of the same name, but in *.sa format for input as parflow pfsa filetype.
  * **drv_vegm.dat**: CLM input file with vegetation type for each model element.
  * **GreyParcelCover.fig**, **Slopes.fig**: Matlab figures depicting lot layout and slopes.

## data/locations
Includes information about the location, elevation, and population of cities chosen for this analysis. Rank and line number correlate with the runname format of 'loc%02d', where the integer represents the rank/line of the city in these files. Files include:
  * **US_cities_census_population_2015.xls**: raw data file from US Census with census and estimated populations of all US cities from 2010-2015.
  * **locations.csv**: Largest 50 US cities, with rank, census name, city, state, 2010 population, latitude, longitude.
  * **location.txt**: latitude and longitude for 50 largest cites, by rank (Madison, WI is on line 51).
  * **locations_with_elev.csv**: rank, city, state, latitude, longitude, and elevation.
  * **locations_LTER.csv**: LTER sites, latitude, and longitude

## data/model_inputs
Running **src/model_inputs/m02_model_inputs.m** creates one subdirectory per model run with a complete set of model inputs. In addition to variables described in **data/layouts/domainInfo.mat**, these versions of **domainInfo.mat** include soil information, namely:  
  * *Ks_imperv*,*porosity_imperv*,*Sres_imperv*,*Ssat_imperv*,*VGa_imperv*,*VGn_imperv*: impervious surface hydraulic conductivity (m/hr), porosity (-), residual saturation (-), saturation (-), van Genuchten alpha (1/m), and van Genuchten n (-).
  * *Ks_soil*,*porosity_soil*,*Sres_soil*,*Ssat_soil*,*VGa_soil*,*VGn_soil*: as above, for soil.
  * *mn_grass*,*mn_imperv*: Manning's n (hr\*m<sup>1/3</sup>) for turfgrass and impervious surfaces

## data/soil
Includes subsurface hydraulic parameters for impervious surfaces (imperv), silt loam (SiL), and highly-compacted silt loam (SiL10c) baseline soil conditions. This information is ultimately incorporated into **domainInfo.mat** (see data/model_inputs).

## data/weather
Includes meterological forcing information for each city, as well as non-changing CLM inputs. Each subdirectory is named after the city population rank (with the form 'loc%02d') and includes the following files: 
  * **nldas.1hr.clm.txt**: hourly meteorological inputs needed for CLM. Columns are 1) *DSWR*, shortwave radiation (W/m<sup.2</sup>), 2) *DLWR*, longwave radiation (W/m<sup.2</sup>), 3) *APCP*, precipitation (mm/s), 4) *Temp*, air temperature (K), 5) *UGRD*, east-west wind speed (m/s), 6) *VGRD*, north-south wind speed (m/s), 7) *Press*, atmospheric pressure (pa), 8) *SPFH*, specific humidity (kg/kg).
  * **precip.mat**: hourly precipitation timeseries (m) extracted from **nldas.1hr.clm.txt** for use in Matlab post-processing.
  * **logs/**: includes coordinates, start datetimes, and end datetimes passed to wgrib for retrieval from NLDAS .grb files (**batch.get_nldas**) and ouput log from that operation (**NLDAS.out**).

Non-changing CLM inputs in the top level directory include:
  * **drv_clmin_start.dat**,**drv_clmin_restart.dat**: CLM timing information for new start and restart models.
  * **drv_vegp.dat**: CLM vegetation parameters (LAI, rooting parameters, etc.) for each landcover type.

# results
Includes results processed by scripts in **src/analyze_results** such as:

  * **correlations.csv**: Spearman correlation coefficients for all 89 climate metrics.
  * **in.out.loc.Rda**: climate metrics plus changes in fluxes due to LID practices by city.
  * **in.out.lot.Rda**: climate metrics plus fluxes by lot type (baseline/low impact) for each city.
  * **plsr.fit.Rda**: goodness-of-fit metrics for PLSR models
  * **plsr.model.Rda**: PLSR model objects for reduction in runoff and partitioning angle.
  * **plsr.precitions.Rda**: PLSR estimates for all modeled climate metrics plus long-term climate metrics for each city.
  * **plsr.VIP.Rda**: VIP scores of variables included in final PLSR models
  * **predict.scaled.Rda**: all modeled climate metrics plus long-term climate metrics for each city, centered on their mean and scaled by their standard deviation.
 
## results/figures
Includes all figures generated by .R scripts in **src/results/analyze_results** as well as final forms included in manuscript in both .svg and .png form.

## results/met
Includes climate data and metrics for the 1 year used in ParFlow simulations as well as the 30 years of long-term hourly weather used for the long-term analysis (indicated by ".long.term" appended to filenames). Includes:
  * **all.city.storms.Rda** tracks the number of storms and key storm characteristics including:  
    * *antecedent.hours*: number of dry hours before storm start  
    * *duration*: number of hours from start to end of storm  
    * *wet.hours*: number of hours during defined storm period with precipitation (can have up to 5 continuous dry hours and still count as one storm)  
    * *depth*: total precipitation depth during storm (mm)  
    * *intensity.avg*: mean precipitation intensity during storm (mm/hr)  
    * *intensity.max*: peak precipitation intensity during storm (mm/hr)  
  * **hourly.precipitation.ET0** hourly precipitation (mm) and reference evapotranspiration (ET0; mm) for each city.  
  * **met.summary.Rda** includes all 89 climate metrics evaluated for each city.  

## results/model_outputs
Due to size of output files, only a limited selection of output files are included in this repo (i.e., those used to create manuscript figures and hourly parcel fluxes).

Model subdirectories are named acording to the same convention in data/model_inputs. Each subdirectory may include the following files:

  * **WBstep.mat**: suite of variables with the hourly flux (m3) at each hour for all hydrologic fluxes (can = water stored in the canopy, dd = deep drainge, etS = evaptranssum, ev = evaporation, precip = precipitation, re = recharge, sno = snow water equivalent, sr = surface runoff, Ss = surface storage, Sss = subsurface storage, SssRZ = subsurface storage in the root zone aka top 1m). Files also includes the hourly forcing (force) for each model component (CLM, PF = parflow, O = overall), the hourly ouputs and storage (calc), and the difference between the forcing and calculated fluxes as a volume (absErr) and relative to the forcing (relErr).  
  * **WBcum.mat**: as with WBstep.mat, but with the running cumulative flux at each hour for all hydrologic fluxes. Due to changes in post-processing scripts, fluxes for developed lots are as a volume (m3), while fluxes for vacant lots are as a depth (mm).  
  * **deep_drainage.grid.cum.mat**: matrix (nx X ny) with the cumulative growing season deep drainage (m3) for each model element.
  * **qflx_evap_all.grid.cum.mat**: as deep_drainage.grid.cum.mat, but for evaporation (from leaves and soil).
  * **qflx_tran_veg.grid.cum.mat**: as deep_drainage.grid.cum.mat, but for transpiration.
  * **loc%02d_<baseline or low_impact>_hourly_balance.csv**: as WBstep.mat, but in csv form (processed with **src/model_outputs/m01_extract_hourly_fluxes.m**.
  * **loc%02d_<baseline or low_impact>_hourly_balance.Rda*: as WBstep.mat, but in Rda form (processed with **src/model_outputs/02_process_hourly_fluxes.m**.

# src
Contains all code for formatting model inputs, processing model outputs, and analyzing results.   

## src/analyze_results
All steps required to create PLSR models and generate manuscript figures.

  * **01_merge_weather_and_fluxes.R** merges inputs (climate metrics) and outputs (ParFlow fluxes) generated by src/met and src/model_outputs, respectively.
  * **02_water_balance_triangle.R** plots ternary diagram, extracts vector angle, and scales variables for input into PLSR models.
  * **03_PLSR.R** generates PLSR models
  * **04_PLSR_fit.R** calculates goodness of fit metrics for PLSR models
  * **05_correlations.R** creates full and small correlation matrix for climate metrics.
  * **06_plot_runoff.R** creates plot of PC1 vs. PC2 for runoff reduction PLSR model.
  * **07_plot_angle.R** creates plot of precipitaton vs. PET for PLSR model.
  * **08_plot_maps.R** creates US maps of expected long-term behavior of LID practices
  * **09_plot_example_city_water_balances.R** plots change in runoff, deep drainage, and ET for example cities.
  * **m10_plot_example_city_subsurface.m** plots spatial maps of cumulative change in deep drainage and ET.
  * **11_plot_Budyko.R** plots ParFlow model results on Budyko axes.

## src/met
**01_runall_met_metrics.R** should be run twice: once to generate summary metrics on the 1 year of weather forcings used to run the models, and once to generate summary metrics for 30 years of long-term hourly weather for the long-term analysis. This script uses raw weather inputs from **data/weather** and **data/climate**, process them with the functions in this subdirectory, and saves results to **results/met** as met.summary.Rda and met.summary.long.term.Rda.

## src/model_inputs
**m01_lot_layouts.m** is included for reference to illustrate how lot layouts in data/layouts were created.

**m02_model_inputs.m** uses lot data in **data/layouts** and adds information about soils and weather based on model scenario. After running this script, complete set of input files for all developed models resides in data/model_inputs.

## src/model_outputs
**m01_extract_hourly_fluxes.m** converts summary matlab output files to .csv files for each model run for further post-processing in R (see results/model_outputs). 

**02_process_hourly_fluxes.R** loads csv files for each model run, checks water balance for conversion, and saves summary file to **results/model_outputs** as fluxes_summaries.Rda.