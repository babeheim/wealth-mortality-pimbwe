
# Imputation proceedure

rm(list = ls())

print(paste0(Sys.time(), " - project initialized"))

source('./code/project_functions.r')
module_init('./impute_data', verbose=TRUE)
files <- list.files('./inputs', full.names=TRUE)
file.copy(files, './impute_data/inputs')
files <- c('./code/impute_data.r', './code/project_functions.r')
file.copy(files, './impute_data/code')
setwd('./impute_data')
source('./code/impute_data.r')
setwd('..') # about 5 minutes

dir_init('./imputed_data')
files <- list.files('./impute_data/output', full.names=TRUE)
file.copy(files, './imputed_data')

source('./code/project_functions.r')
module_init('./fit_models', verbose=TRUE)
files <- list.files('./imputed_data', full.names=TRUE)
file.copy(files, './fit_models/inputs')
files <- c('./code/fit_models.r', './code/project_functions.r')
file.copy(files, './fit_models/code')
setwd('./fit_models')
source('./code/fit_models.r')
setwd('..')

dir_init('./output')
files <- list.files('./imputed_data', full.names=TRUE)
file.copy(files, './output')
files <- list.files('./fit_models/output', full.names=TRUE)
file.copy(files, './output')

if(!save_temp){
    folders <- c(
        './impute_data',
    	'./imputed_data',
        './fit_models'
    )
    if(length(folders)>0) unlink(folders, recursive=TRUE)
}


print(paste0(Sys.time(), " - project finish"))
