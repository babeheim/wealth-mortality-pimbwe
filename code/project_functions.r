
save_temp <- FALSE

dir_init <- function(path, verbose=FALSE){
    if(substr(path, 1, 2)!='./') stop('path argument must be formatted
        with "./" at beginning')
    contents <- dir(path, recursive=TRUE)
    if(verbose){
        if(length(contents)==0) print(paste('folder ', path, ' created.', sep=""))
        if(length(contents)>0) print(paste('folder ', path, ' wiped of ', length(contents), ' files/folders.', sep=""))
    }
    if(dir.exists(path)) unlink(path, recursive=TRUE)
    dir.create(path)
}

module_init <- function(path, verbose=FALSE){

    code_path <- paste(path, '/code', sep='')
    inputs_path <- paste(path, '/inputs', sep='')
    dir_init(path, verbose=verbose)
    dir_init(code_path, verbose=FALSE)
    dir_init(inputs_path, verbose=FALSE)

}