# urbanheat

This repository contains the code for the spatial Bayesian model implemented in the following article (under review): 

#### **Joint-likelihood Bayesian model for urban heat island inference with two crowdsourced datasets**

This work has been partly financed by the Centre National de Recherches Météorologiques and the Centre National de Recherche Scientifique as part of Eva Marques PhD, supervised by Dr. Philippe Naveau, Dr. Valéry Masson and Dr. Olivier Mestre. Contact: eva0marques@gmail.com

## Dependencies
Following code needs `R (>= 4.4.1)`  
Imports: [CrowdQCplus](https://github.com/dafenner/CrowdQCplus) , [INLA](https://www.r-inla.org) , testthat, data.table, devtools, doMC, foreach, dplyr, ggplot2, ggspatial, ggpubr, sf, sftime, stats, terra, tidyr, tidyterra, tidyverse, fields, lubridate

## Data 
Data is not provided as some datasets are not openly available. The data processing functions are not published as they are not relevant. See manuscript for more details.


## Folder structure and files content 
- `generic/` is where the all generic functions are stored. 
  - data formatting:
    - `add_sw.R`: add solar radiation to a dataset from RADOME data (Météo-France)
    - `class_data_bhm.R`: create a class to format data input for the Bayesian model
    - `correct_altitude_gradient.R`: function to normalise the temperature within the city with regard to altitude (to avoid comparing apples and oranges when computing the urban heat island magnitude)
  - Bayesian Hierarchical Model implementation and inference 
    - `store_post_info.R`: function used in the following script to store all model inference information in a table
    - `run_bhm.R`: joint model + car model + cws model
  - output scores and analysis plots
    - `temperature_maps.R`: mapping functions to plot the observations or the urban heat island infered by the model 
    - `evaluate_pred_vs_pro.R`: group of functions to calculate inference scores against independant professional urban network
    - `marginal_density_plots.R`: funcitons to plot marginal posterior densities
    - `residual_analysis.R`: functions to plot residual analysis 
    - `score_density_plot.R`: plot densities of RMSE and residuals
    - `tile_plots.R`: functions for a nice overview of the parameters evolution in time (horizontal: hours of the day, vertical: day of the month)
    - `spider_charts.R`graphic tentative to visualise each model performances throughout the day (joint vs car vs cws)
    - `load_palette.R`: utility function to quickly load color palettes for plots
- `application/` is a directory for Dijon case study on August 2018
  - `run_dijon_201808.R`: run all pipeline from data loading to model inference on the case study
  - `all_plots_functions.R`: functions to generate all analysis plots
  - `plot_dijon_2018.R`: save all plots used for a full analysis (similar to the one detailed in the paper)
