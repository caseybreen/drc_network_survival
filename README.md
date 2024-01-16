# remote_mortality_study

Code, survey instruments, irb protocols, and other resource for DRC mortality study. 

## Code 

The code under the `/code` repository is meant to be run in order: 

* `00_data_cleaning.Rmd` - cleans files from REACH (renames variables, drops a few records, etc.) 
* `01_geography_maps.Rmd` – creates map for paper 
* `02_weighting_targets.Rmd` – constructs weighting targets from worldpop data 
* `03_construct_weights.Rmd` – constructs using both raking and post-stratification. (We only use post-stratification weights here)
* `04_cdr_estimation.Rmd` – estimates set of pre-registered estimates 


## Google Drive folders

The data/ and out/ folders are stored in a shared Google Drive folder called 

- 'drc-mortality/data' 
- 'drc-mortality/out'. 

 Create symlink:

**On a Mac** you can do this by typing `ln -s PATH_TO_GOOGLE_DRIVE_FOLDER`. 



