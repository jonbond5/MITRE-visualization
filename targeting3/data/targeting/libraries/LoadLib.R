InstalledPackage <- function(package) 
{
    available <- suppressMessages(suppressWarnings(sapply(package, require, quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE)))
    missing <- package[!available]
    if (length(missing) > 0) 
        return(FALSE)
    return(TRUE)
}

# test code
# if ( CRANChoosen() ) {
#   # validate the cran; make sure if defined in environment
#   # it actually existibg in evironment
#   cranRepo = getOption("repos")["CRAN"]
#   bEnvSpecified = grepl("\\$\\{", cranRepo)
#   if(bEnvSpecified) {
#     # ok, set in environment, check to make sure set
#     cran = gsub("\\{|\\}|\\$", "", cranRepo)
#     cranENV = Sys.getenv(cran)
#     if(cranENV == '')
#       options(repos = c(CRAN = cranMirror))
#   }
# }
# cranChosen = TRUE
# b = getOption("repos")["CRAN"] != "@CRAN@"
# if(getOption("repos")["CRAN"] != "@CRAN@") {
#   bEnvSpecified = grepl("\\$\\{", cranRepo)
#   if(bEnvSpecified) {
#     # ok, set in environment, check to make sure set
#     cran = gsub("\\{|\\}|\\$", "", cranRepo)
#     cranENV = Sys.getenv(cran)
#     if(cranENV == '')
#       cranChosen = FALSE
#   }
# }
CRANChoosen <- function()
{
  #return(getOption("repos")["CRAN"] != "@CRAN@")
  # validate the cran; make sure if defined in environment
  # it actually exists in evironment
  cranChosen = TRUE
  cranRepo = getOption("repos")["CRAN"]
  if(cranRepo != "@CRAN@") {
    bEnvSpecified = grepl("\\$\\{", cranRepo)
    if(bEnvSpecified) {
      # ok, set in environment, check to make sure set
      cran = gsub("\\{|\\}|\\$", "", cranRepo)
      cranENV = Sys.getenv(cran)
      if(cranENV == '')
        cranChosen = FALSE
    }
  }
  return(cranChosen)
}

UsePackage <- function(package, defaultCRANmirror = "https://cran.cnr.berkeley.edu/") 
{
    if(!InstalledPackage(package))
    {
        if(!CRANChoosen())
        {   
            # chooser requires a human in loop...  
            # chooseCRANmirror is from utils package
            #chooseCRANmirror()
            #if(!CRANChoosen())
            #{
                options(repos = c(CRAN = defaultCRANmirror))
            #}
        }
        # always load dependencies...  otherwise will silently die (on occasion)
        suppressMessages(suppressWarnings(install.packages(package, dependencies = T)))
        if(!InstalledPackage(package)) 
            return(FALSE)
    }
    return(TRUE)
}
