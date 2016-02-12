# ProvisioningDataCombination



## compilation_provisioning: extract raw data and give list of errors in the original excel files

give 'Tin' 'Tout' 'Sex' for all files

for old files, also gives comments 'S', 'IN', 'O' with color of 'O' visits

create variables: synchrony, duration dbl attended, duration unattended, duration Mvisit, duration Fvisit, alternation, feeding rate, duration activity around the nest...






## ProvisioningErrorCheck: to standardize file, correct chronology, fill in missing info

### for files with Old Template

I am assuming that 
'O' blue means feeding from the outside
'O' grey means hanging out of the nest box
'S' is hanging out at the nest box before entering it or after leaving it
and therefore I changed files accordingly (specifically files for 2010-2011 with old template)

we corrected all illogical chronology with the videos unless specified (a few missing DVDs)

we removed all comments other than 'O' 'G' 'S' 'IN' to the next empty column or the 'activity' column
and replaced it appropriately (i.e. following the definitions above)

we standardized  all time when bird was 'IN' at begining (Tin = 0) and at the end (Tout = tape length)

the data is contained in the second sheet (create sheet1 otherwise)



### for files with New Template

Checked that all Tout were filled in, chronology logical, no comments within the raw data




## LogChangesExcelFiles

all changes made to the original excel files from the dropbox according to the rules above, the queries in ProvisioningErroCheck, and the code in compilation_provisioning, were logged in here.

the original copies were backedup on the HS google drive

all original xls files were converted to xlsx