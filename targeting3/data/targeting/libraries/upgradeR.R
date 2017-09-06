
# script to upgrade R to lastest version using installer
upgradeR <- function()
{
    ## load up required library loader routines (in projectLibraries)
    if(!exists("UsePackage", mode="function")) 
        source("LoadLib.R")
    
    ## source global packages second
    ## required libraries (initially growing list....)
    libraries <- c("installr")       # required package to update R
    for(library in libraries) 
    { 
        if(!UsePackage(library))
        {
            stop("LoadLibrary Error!", library)
        }
    }
    
    # Finally, update R...
    updateR()
}