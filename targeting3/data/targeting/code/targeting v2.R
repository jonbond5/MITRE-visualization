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
# from the data dictionary
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
    if (missing(baseURL))
        bInternet = FALSE
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
        raw.df = read.csv(dataFile, 
                          header=T, 
                          sep=',', 
                          colClasses = colClasses, 
                          na.strings=c("", "NA", "?", " "));
    }
    return(raw.df)
}

# features is a comma seperated list to query
# featureSet = c("StrikeID", "T1", "Target_T1")
loadData <- function(df, features) {
    
    if(missing(df) | missing(features))
        return(NULL)
    raw.df = df %>% select(features)
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
count(raw.df)

# have a useable start to importing the data\
# 1) missing data... (actually looks pretty good)
missing = missingValues(raw.df)
missing

# 2) work to correct improperly parsed feature types
# might be easier to turn off strings as factors, then change the factor features?
raw.df$DTG <- str_replace_all(as.character(raw.df$DTG), "Z", "")
raw.df$DTG <- parse_date_time(raw.df$DTG, orders="dHMSby")
raw.df$Date <- paste(day(raw.df$DTG), month(raw.df$DTG, label=T), year(raw.df$DTG), sep="-")
raw.df$Time <- paste(hour(raw.df$DTG), minute(raw.df$DTG), sep = ":")

# have a loom at the categorical variable
cats_pos = c(2, 9)

#### look at the categorical variables
cats.df = raw.df[, cats_pos]
lcat = categoricalAnalysis(cats.df)
lcat

#numerical analysis - summary statistics
#stat.desc(raw.df)

# some elements are repeating, split them off into there own list 
# (reference strike id).  Focus areas:
# 1) critical elements (detailed target)
targetDetailSet = c("StrikeID", "T1", "Target_T1")
td.df = na.omit(loadData(raw.df, targetDetailSet))

# get total tagets by strikeid
td.df %>% group_by(StrikeID, Target_T1) %>% summarise(totTargets = sum(T1)) %>% head

# 2) Aircraft
aircraftSet = c("StrikeID", "Aircraft", "Callsign", "ACBase", "Nationality")
rawAC.df = loadData(raw.df, aircraftSet)
missingAC = missingValues(rawAC.df)
missingAC
ac.df = na.omit(loadData(rawAC.df, aircraftSet))
head(ac.df)

# 3) Ordinance
# 4) Physical, Confidence, Functional
# 5) MEA, CDA
# Will use dplyr for the most part...

# keep a local copy just in case the internet is down
#write.csv(raw.df, file = paste0(projectDataSet,"raw.csv"))


