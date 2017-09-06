######################################################################
### setup the environment
######################################################################
scriptName = "targeting.r";
projectPath = "c:/MITRE/AF-Targetting/targeting/"   # note spelling error
projectCode = paste0(projectPath, "code/");
## care with what is stored locally!!!  onlu anonimized data can be stored locally
projectDataSet = paste0(projectPath, "datasets/")
projectResults = paste0(projectPath, "results/")
projectLibraries = paste0(projectPath, "libraries/")

## if projectCode is not setup, just set to current working directory
if ( !exists("projectCode") ) {
    projectPath = getwd();
}

# Set working directory to where data is located
setwd(projectCode);
message( sprintf("Running %s from path: %s", scriptName, getwd() ) );

## MITRE has a proxy serving as a man in the micdle snooping the traffic
## we need to point to this proxy server to load any global packages
Sys.setenv(http_proxy="http://gatekeeper.mitre.org:80")
Sys.setenv(https_proxy="http://gatekeeper.mitre.org:80")
Sys.getenv("http_proxy")

######################################################################
## common defines...
######################################################################
loadSourceEvaluation = FALSE   ## load library to evaluate a source file
colClasses = c( "integer",         # StrikeNumber
                "factor",          # Classification
                "character",       # Country
                "character",       # DTG
                "character",       # date
                "character",       # time
                "character",       # ATO
                "character",       # EngageUnit
                "factor",          # Del/Dyn
                "character",       # EntityID
                "factor",          # O-suffix
                "numeric",         # CatCode
                "character",       # EntityName
                "character",       # City
                "character",      # Province
                "character",       # ROE
                "character",       # TargetFunction
                "integer",         # TF#1
                "character",       # Target_TF#1
                "numeric",         # lat
                "numeric",         # long
                "character",       # MGRS
                "character",       # Observer
                "character",       # Aircraft
                "character",       # callsign
                "character",       # ACBase
                "character",       # nationality
                "integer",         # ORD#1
                "character",       # Ordinance_ORD#1
                "character",       # target comments
                "character",       # recoupe
                "character",       # physical
                "character",       # confidence
                "character",       # functional
                "character",       # MEA
                "character",       # CDA
                "character")       # MISREP

######################################################################
# Load the requisite libraries
######################################################################
##  source local project libraries first
if(loadSourceEvaluation)
    ## internal debugging helper, generally not used
    # call via evaluteSourceExpressions("my-file.R")
    source(paste0(projectLibraries,"EvaluateSourceExpressions.R"))

# load our postgres pdmp helper routines
## load up required library loader routines (in projectLibraries)
if(!exists("UsePackage", mode="function")) 
    source(paste0(projectLibraries,"LoadLib.R"))

## source global packages second
## required libraries (initially growing list....)
libraries <- c("ggplot2", 
               "stringr",       # nice string package
               "dplyr",         # nice package to work with "table" data
               "pryr",          # pryer helps to search namespace (conflicts happen)
               "data.table",    # has a nice %like% option, largely overlaps with dplyr
               "RCurl",         # a little more power for reading web content
               "lubridate",     # date package
               "readr",         # another csv reader package
               "pastecs")       # love stat.desc - to get descriptive stats on a data frame
for(library in libraries) 
{ 
    if(!UsePackage(library))
    {
        stop("LoadLibrary Error!", library)
    }
}
######################################################################
# local support routines
######################################################################
missingValues <- function(df) {
    # are there any missing data
    bMissing = any(is.na(df))
    lMissing = list()
    index = 1
    if(bMissing) {
        for (Var in names(df)) {
            missing <- sum(is.na(df[,Var]))
            if (missing > 0) {
                print(c(Var,missing))
                lMissing[[index]] = c(Var, missing)
                index = index + 1
            }
        }
    }
    str(lMissing)
    return(lMissing)
}
categoricalAnalysis <- function(cats.df) {
    str(cats.df)
    names(cats.df)
    lcat = list()
    i = 1
    for (col in names(cats.df)) {
        col
        ftab = table(cats.df[,col], useNA="ifany")
        ptab = prop.table(ftab)
        lcat[[i]] = c(col, ftab, ptab)
        i = i + 1
    }
    return(lcat)
}
# I like this approach
printf <- function(...)print(sprintf(...))

loadData <- function (bInternet, dataFile, baseURL) {
    if (bInternet) {
        # read up the raw content (it is a string)
        rawContent = getURL(baseURL, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
        class(rawContent)
        
        #make any necessary changes (regex in case) - i.e., url_content <- gsub('<br />', '', url_content)
        # none required
        
        ## convert to our dataframe format (via textConnection)
        # findings: header, tab seperated
        raw.df = read.csv(textConnection(rawContent), header=T, sep='\t')
    } else {
        # Read data in from file, comma deliminated, header is true
        raw.df = read.csv(dataFile, header=T, sep=',', colClasses = colClasses);
    }
    return(raw.df)
}
# Constants to our mainline
#
# ftp://ftp.broadinstitute.org/pub/ExAC_release/release0.3.1/functional_gene_constraint/fordist_cleaned_exac_r03_march16_z_pli_rec_null_data.txt
# grab the Broad data set from the above URL
baseURL = ''
rawDataFile = "Targeting Data Analytics v4a.csv";
dataFile = paste0( projectDataSet, rawDataFile ); 

######################################################################
# Begin
######################################################################
# load the data (web or local)
raw.df = NULL
bInternet = FALSE   # toggle with internet access
raw.df = loadData(bInternet, dataFile, baseURL)
dim(raw.df)
str(raw.df)
names(raw.df)
summary(raw.df)
head(raw.df)

# have a useable start to importing the data\
# 1) missing data... (actually looks pretty good)
missing = missingValues(raw.df)
missing

# 2) work to correct improperly parsed feature types
# might be easier to turn off strings as factors, then change the factor features?
raw.df$Country <- as.character(raw.df$Country)
raw.df$DTG <- str_replace_all(as.character(raw.df$DTG), "Z", "")
raw.df$DTG <- parse_date_time(raw.df$DTG, orders="dHMSby")
raw.df$Date <- paste(day(raw.df$DTG), month(raw.df$DTG, label=T), year(raw.df$DTG), sep="-")
raw.df$Time <- paste(hour(raw.df$DTG), minute(raw.df$DTG), sep = ":")
raw.df$ATO <- as.character(raw.df$ATO)
raw.df$EngageUnit <- as.character(raw.df$EngageUnit)
raw.df$EntityID <- as.character(raw.df$EntityID)
raw.df$EntityName <- as.character(raw.df$EntityName)
raw.df$City <- as.character(raw.df$City)
raw.df$Province <- as.character(raw.df$Province)
raw.df$Target.Function <- as.character(raw.df$Target.Function)
raw.df$Target_T1 <- as.character(raw.df$Target_T1)
raw.df$Observer <- as.character(raw.df$Observer)
raw.df$Aircraft <- as.character(raw.df$Aircraft)
raw.df$Callsign <- as.character(raw.df$Callsign)
raw.df$ACBase <- as.character(raw.df$ACBase)
raw.df$Nationality <- as.character(raw.df$Nationality)
raw.df$Ordinance_1 <- as.character(raw.df$Ordinance_1)
raw.df$TargetsRemark <- as.character(raw.df$TargetsRemark)
raw.df$Physical <- as.character(raw.df$Physical)
raw.df$Confidence <- as.character(raw.df$Confidence)
raw.df$Functional <- as.character(raw.df$Functional)
raw.df$MEA <- as.character(raw.df$MEA)
raw.df$CDA <- as.character(raw.df$CDA)

# some elements are repeating, split them off into there own list 
# (reference strike id)

# have a loom at the categorical variable
cats_pos = c(2, 9)

#### look at the categorical variables
cats.df = raw.df[, cats_pos]
lcat = categoricalAnalysis(cats.df)
lcat

#numerical analysis - summary statistics
#stat.desc(raw.df)

# keep a local copy just in case the internet is down
#write.csv(raw.df, file = paste0(projectDataSet,"raw.csv"))


