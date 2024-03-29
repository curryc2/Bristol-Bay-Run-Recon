# Bristol Bay Annual Run Reconstruction
Model Creators:
* Dr. Curry J. Cunningham (University of Alaska Fairbanks, College of Fisheries and Ocean Sciences)
* Dr. Trevor Branch (University of Washington, School of Aquatic and Fishery Sciences)

This run reconstruction model is detailed in **A general model for salmon run reconstruction that accounts for interception and and differences in availability to harvest** [Cunningham et al. (2018)](http://www.nrcresearchpress.com/doi/abs/10.1139/cjfas-2016-0360#.W76LUCdRebU).

Original Project Collaborators:
Curry J. Cunningham, Trevor A. Branch, Tyler H. Dann, Matt Smith, James E. Seeb, Lisa W. Seeb, Ray Hilborn, Lowell Fair, Tim Baker, Fred West.

## Purpose
A general run reconstruction model for the sockeye salmon fishery in Bristol Bay, Alaska. This model leverages information from age and genetic composition samples to apportion mixed-stock catches and account for interception among terminal fishing districts.

## Workflow
Each year is estimated **independently** although some information is shared among years.

  1. R code creates ADMB input files in the [/Syrah](https://github.com/curryc2/Bristol-Bay-Run-Recon/tree/master/Syrah) directory, based on input data in the [/R]() directory.
  2. Formal run reconstruction model estimation by AD-Model Builder (ADMB) in [/Syrah](https://github.com/curryc2/Bristol-Bay-Run-Recon/tree/master/Syrah) directory.
  3. Parsing annual ADMB output (.out) files into user-friendly tables and figures in the [Syrah/outputFiles](https://github.com/curryc2/Bristol-Bay-Run-Recon/tree/master/Syrah/outputFiles) directory.

## Syrah Model Versions
Name                            | Description
--------------------------------|-------------------------------
Syrah                           | Original version.
Syrah_v1                        | Original model + small update to estimate RunSize in log space.
Syrah_v2                        | Original model + small update to estimate RunSize in log space + updates from Hamazaki (ADFG) to likelihood formula (and perhaps other changes).

** NOTE: *** all versions are designed to be backward compatible with input (.dat) and output (.out) files. All upstream data input creation R functions, and output processing and data visualization R functions will operate normally, independent of version. 

## Structure

Directory                       | Description
--------------------------------|-------------------------------
R                               | ageComp.annual.csv
R                               | GeneticsComp_updated_Annual.csv
R                               | qry_Annual_ESCAPEMENT.csv
R                               | qry_Annual_CATCH_updated.csv
Syrah/outputFiles/Reallocation  | catchAdd.csv
Syrah/outputFiles/Reallocation  | ageCompAdd.csv
Syrah/outputFiles/Reallocation  | inshoreTogiak.csv
Syrah/outputFiles/Reallocation  | offshoreCatchAdd.csv
Syrah                           | Directory for running ADMB estimation model.
Syrah/outputFiles               | Holds ADMB estimation model output (.out files) and code to generate output figures                                 |and tables.

