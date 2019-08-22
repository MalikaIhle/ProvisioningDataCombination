
[![DOI](https://zenodo.org/badge/49877181.svg)](https://zenodo.org/badge/latestdoi/49877181)

# REPO PURPOSE

1) extract all provisioning data from 2004 until 2015
2) select and analyse the data with regard to parental coordination


---


# 1) Compilation provisioning .R : DATA EXTRACTION


## extract raw data in excel files and Database and give list of errors in the original excel files

give 'Tstart' 'Tend' 'Sex' for all excel files

for old files, also gives comments 'S', 'IN', 'O' with color of 'O' visits

query the DB to get MY_tblParentalCare, MY_tblBrood, MY_tblDVDInfo, create variables on all data

data 'All' contains info on whether the bird were seen around the nest (Shinichi's protocol), 'FeedingVisits' only contains feeding visits

#### creates all tables of all extracted data and store then in R_ExtractedData folder: these can be used by other members of the team, working on provisioning and not necessarily behavioural compatibility (i.e. this is the full set of data)


---


# 2) Behavioural compatibility in Provisioning : DATA ANALYSES

## Coordination Data Selection and Randomization

call data from data extraction code

data selection

perform the different types of randomization and plot observed vs expected coordination

#### creates all tables needed for data analyses of the coordination in provisioning project and store them in the R_Selected&RandomizedData folder

## Simulation Analyses Coordination 

simulation of data and modelling of coordination according to different models to choose from + graph 

## Simulation Analyses Coordination Iceberg

processing the simulations from the code above on a high power computing server (Iceberg) and summarizing the results of a 1000 run per simulation


## Coordination Data Analyses

call selected data for this project from the folder 'R_Selected&RandomizedData' folder

data analyses (observed vs expected coordination, predictors of alternation, synchrony, fitness, divorce)

## modChickMassandVariance_stan

one specific analysis using a double hierarchical model that runs in STAN

---


# R_input

## .txt

a few temporary txt files, resulting of temporary queries of the DB.
ultimately, everything will be called directly from the DB with the code compilation_provisioning.R




# R_ExtractedData


### R_RawAllVisits_forDB

this as all the raw data until time of code run (last: 20160323)
this is the raw data I will do my analyses on (no data selection yet), this is also what Ian Stevensson could import into the DB
(historically, it first included files from 2004 to 2014, then 206 missing files from this period, then videos from 2015 from Andrew)


## R_MY_tables 

data, meta data, and summary of data I calculated (data cleaned but no selection relative to project on compatibility yet: this can be used by Joel working on provisioning itself)


### R_MY_RawFeedingVisits

OF directly followed by IN are merged feeding visits  
Interval calculated


### R_MY_DVDInfo

metada only for analysed video, i.e. with excelfile with raw data available, and with an entry in zzz_OldtblparentalCare


### R_MY_tblParentalCare

summary stats for all analyzed videos, i.e. with excelfile with raw data available, and with an entry in zzz_OldtblparentalCare



### R_MY_tblBroods

all broods where at least one social parent identified, even those not videotaped




# R_Selected & Randomized Data

data after selection for behavioural compatiblity project, included results of various randomizations

### MY_TABLE_perDVD

### MY_TABLE_perBrood

### MY_TABLE_perChick

### MY_TABLE_perBirdYear


---


# Side .R codes Folder

## Alternation & Synchrony
OLD code which got split into selection and simulation vs analyses
source compilation_provisioning to get the data
select data valid for studing parental coordination
simulation of random alternation for given provisioning rates
Predictors and benefits of alternation
Predictors and benefits of provisioning rate
Predictors and benefits of synchrony

## simulation correlation Alternation - Synchrony
to understand mathematical and necessary relationships between variables

## Alternation with standardized intervals
to try out an idea how to remove the effect of both birds being potentially influenced simulatenously by the environment and
for each nest wacth, use one random sex as the bird who knows how to react to the environment
set all its intervisit intervals to its mean
rescaled the other bird intervisit intervals according to the standardizing bird
calculate observed and expected alternation (after randomization) in those scaled nest watches

## simulation correlation Alternation - Total P - Diff P
to understand mathematical and necessary relationships between variables


---


# ErrorChecking Folder: relative to data extraction


## ProvisioningErrorCheck: to standardize file, correct chronology, fill in missing info


### for files with Old Template

I am assuming that 
'O' blue means feeding from the outside
'O' grey means hanging out of the nest box
'S' is hanging out at the nest box before entering it or after leaving it
and therefore I changed files accordingly (specifically files for 2010-2011 with old template)

we corrected all illogical chronology with the videos unless specified (i.e. for a few missing DVDs, I just indicated a logical time that had the least impact on the original data)

we removed all comments other than 'O' 'G' 'S' 'IN' to the next empty column or the 'activity' column
and replaced it appropriately (i.e. following the definitions above)

we standardized  all time when bird was 'IN' at begining (Tin = 0) and at the end (Tout = tape length)

the data is contained in the second sheet (created sheet1 otherwise)

#### importantly: we corrected the raw data of each excel files but not their summary. This will be directly corrected in the DB when Ian Stevensson import the R_output with the raw data, and when he creates a sys_query recreating this summary



### for files with New Template

Checked that all Tout were filled in, chronology logical, no comments within the raw data


## LogChangesExcelFiles

all changes made to the original excel files from the dropbox according to the rules above, the queries in ProvisioningErroCheck, and the code in compilation_provisioning, were logged in here.

the original copies were backedup on the HS google drive

all original xls files were converted to xlsx





## R_Compare_DB_and_R_output

### R_Compare_tblParentalCare

this compare tblParentalCare from the database (which included typos and other mistakes), and the same table generated by my code combination_provisionning.R > my code is correct in all cases checked.


### R_Compare_NbChicksDuringRecording

this compare the number of chicks calculated to be alive at time of recording, the number of chicks during a visit on the same day as the recording, and the number of chicks given in tblDVDInfo > nothing is matching, but:
- tblDVDInfo number of chicks is correct for 2004 2005 (Shinichi checked the nest), but death dates are not (indicated as being on the last day seen alive, instead of the day found to be missing)
- tblDVDInfo number of chicks is estimated in subsequent years (from previous nest check) but about correct...









