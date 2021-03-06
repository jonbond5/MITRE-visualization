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
               "tidyverse",     # works well with stringr and dplyr
               "stringr",       # nice string package
               "quanteda",      # document matrix package
               "purrr",         # 
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

loadData <- function (dataFile) {
    
    if (missing(dataFile))
        return(NULL)
    
    # Read data in from file, comma deliminated, header is true
    raw.df = read.csv(file=dataFile, 
                      header=T, 
                      sep=',', 
                      strip.white = T,
                      colClasses = colClasses,
                      na.strings=c("", "NA", "?", " "))
    return(raw.df)
}

# features is a comma seperated list to query
# featureSet = c("StrikeID", "T1", "Target_T1")
loadFeatures <- function(df, features) {
    
    if(missing(df) | missing(features))
        return(NULL)
    raw.df = df %>% select(features)
    return(raw.df)
}

## see - http://www.mjdenny.com/Text_Processing_In_R.html
## see - https://github.com/matthewjdenny
## still tweaking
Clean_String <- function(string){
    # Lowercase
    temp <- tolower(string)
    
    #' Remove everything that is not a number or letter (may want to keep more 
    #' stuff in your actual analyses). 
    temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
    
    # Shrink down to just one white space
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    
    # Split it
    temp <- stringr::str_split(temp, " ")[[1]]
    
    # Get rid of trailing "" if necessary
    indexes <- which(temp == "")
    if(length(indexes) > 0){
        temp <- temp[-indexes]
    } 
    return(temp)
}
#' function to clean text
#' typical calling sequence:
#       con <- file("Obama_Speech_2-24-09.txt", "r", blocking = FALSE)
#       text <- readLines(con)
#       close(con) 
Clean_Text_Block <- function(text){
    if(length(text) <= 1){
        # Check to see if there is any text at all with another conditional
        if(length(text) == 0){
            cat("There was no text in this document! \n")
            to_return <- list(num_tokens = 0, unique_tokens = 0, text = "")
        }
        else {
            # If there is , and only only one line of text then tokenize it
            clean_text <- Clean_String(text)
            num_tok <- length(clean_text)
            num_uniq <- length(unique(clean_text))
            to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
        }
    }
    else {
        # Get rid of blank lines
        indexes <- which(text == "")
        if(length(indexes) > 0){
            text <- text[-indexes]
        }  
        # Loop through the lines in the text and use the append() function to 
        clean_text <- Clean_String(text[1])
        for(i in 2:length(text)){
            # add them to a vector 
            clean_text <- append(clean_text,Clean_String(text[i]))
        }
        # Calculate the number of tokens and unique tokens and return them in a 
        # named list object.
        num_tok <- length(clean_text)
        num_uniq <- length(unique(clean_text))
        to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
    }
    return(to_return)
}
parseMetricTokens <- function(field)
{
    # works very well...
    split.pos <- gregexpr('\\(.*?)', field)[[1]]
    split.length <- attr(split.pos, "match.length")
    split.start <- sort(c(1, split.pos, split.pos+split.length))
    split.end <- c(split.start[-1]-1, nchar(field))
    split.results = substring(field,split.start,split.end)
    split.results
    index = 1;
    tokenList = list()
    # not quite: all <- unlist(strsplit(x, "\\s+"))
    # not quite: y = unlist(strsplit(x, ')'))
    # closer: regmatches(x, gregexpr("(?=\\().*?(?<=\\))", x, perl=T))[[1]]
    pat <- "(?<=\\()([^()]*)(?=\\))"
    for (i in split.results) {
        if (nchar(i) > 1) {
            element = i
            pos <- gregexpr('\\(.*?)', element)[[1]]
            if (pos > 0)
                element = regmatches(element, gregexpr(pat, element, perl=TRUE))
            element2 = str_trim(element)
            #printf("element: %s, element2: %s, length: %d", element, element2, nchar(element))
            tokenList[index] = element2
            index = index + 1
        }
    }
    printf("index: %d", index)
    if(index == 2)
        # if there  is no numeric value (x), assume 1.
        tokenList[index] = "1"
    tokenList
    df = as.data.frame(tokenList)
    names(df) = c("level", "count")
    return(df)
}

# DOES NOT WORK - NOT Supported DPLYR
#  need to investyigate purr or base::aggregate
# sets - critical notes
#   1) order is important...
#       a) StrikeID MUST BE FIRST (this is the FK back to target table)
#       b) METRIC must be second - this is because we reference columns by index
#       c) example: meaSet = c("StrikeID", "MEA")
#   2) the dataframe to query against
parseMetric <- function(metricSet, df)
{
    # no attrbute specification, no deal.
    if(missing(metricSet) | missing(df))
        return(NULL) 
    
    # easy change
    groupBy = 1
    metric = 2
    
    # ok, have at it
    metric.df = na.omit(loadFeatures(df, metricSet))

    # parse out the single line, multiple token input
    metric2.df = metric.df %>% rowwise() %>% mutate(level=parseMetricTokens(.[[2]])$level)
    metric3.df = metric2.df %>% rowwise() %>% mutate(count=parseMetricTokens(.[[2]])$count)
    metric4.df = metric3.df %>% mutate_at(vars(matches("count")),funs(as.numeric))
    rm(metric.df, metric2.df, metric3.df)
    return(metric3.df)
}
# Constants to our mainline
#
rawDataFile = "Targeting Data Analytics v4a.csv";
dataFile = paste0( projectDataSet, rawDataFile ); 

######################################################################
# Begin
######################################################################
# load the data (web or local)
raw.df = loadData(dataFile)
dim(raw.df)
str(raw.df)
names(raw.df)
head(raw.df)

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
head(raw.df)

# have a loom at the categorical variable
cats_pos = c(2, 9)
cats.df = raw.df[, cats_pos]
lcat = categoricalAnalysis(cats.df)
lcat

#numerical analysis - summary statistics (none)
#stat.desc(raw.df)

# some elements are repeating, split them off into there own list 
# (reference strike id).  Focus areas:
# 1) critical elements (detailed target)
targetDetailSet = c("StrikeID", "T1", "Target_T1")
td.df = na.omit(loadFeatures(raw.df, targetDetailSet))

# get total tagets by strikeid
td.df %>% group_by(StrikeID, Target_T1) %>% summarise(totTargets = sum(T1)) %>% head

# 2) Aircraft
aircraftSet = c("StrikeID", "Aircraft", "Callsign", "ACBase", "Nationality")
ac.df = na.omit(loadFeatures(raw.df, aircraftSet))
ac.df %>% group_by(StrikeID, Aircraft) %>% head
ac.df %>% group_by(StrikeID) %>% summarize(AC = paste(Aircraft, collapse=","), total=n()) %>% head

# 3) Ordinance
ordSet = c("StrikeID", "ORD1", "Ordinance_1")
ord.df = na.omit(loadFeatures(raw.df, ordSet))
ord.df %>% group_by(StrikeID) %>% head
ord.df %>% group_by(StrikeID) %>% summarize(Ordinance = paste(Ordinance_1, collapse=",")) %>% head
ord.df %>% group_by(StrikeID) %>% summarize(totalOrd=sum(ORD1), Ordinance = paste(Ordinance_1, collapse=",")) %>% head

# 4) Physical, Confidence, Functional
# Physical
# these are a tad bit trickier..  
Set = c("StrikeID", "Physical")
phy.df = na.omit(loadFeatures(raw.df, Set))

phy2.df = phy.df %>% rowwise() %>% mutate(damage=parseMetricTokens(Physical)$level)
phy3.df = phy2.df %>% rowwise() %>% mutate(count=parseMetricTokens(Physical)$count)
phy4.df = phy3.df %>% mutate_at(vars(matches("count")),funs(as.numeric))
str(phy4.df)
phy4.df %>% group_by(StrikeID) %>% head
rm(phy.df, phy2.df, phy3.df)

# have a loom at the categorical variable
phyCats = c(3)
phyCats.df = phy4.df[, phyCats]
phyCatsTab = categoricalAnalysis(phyCats.df)
phyCatsTab

# possible spelling matches on destroyed - there are a lot of inconsistencies.
# eventually will need a better mechanism in parseMetricTokens (see strsplit with regex?)
destroyedRegExKeyWords = c("des", "troy")
destroyedRegEx = paste(destroyedRegExKeyWords, collapse = "|")
phy4.df %>% group_by(StrikeID) %>% filter(grepl(destroyedRegEx, damage)) %>% summarize(total=sum(count)) %>% head
phy4.df %>% group_by(StrikeID) %>% filter(str_detect(damage, destroyedRegEx)) %>% summarize(total=sum(count)) %>% head

# Confidence
confSet = c("StrikeID", "Confidence")
conf.df = na.omit(loadFeatures(raw.df, confSet))
conf.df %>% group_by(StrikeID) %>% head

# parse out the single line, multiple token input
conf2.df = conf.df %>% rowwise() %>% mutate(confidence=parseMetricTokens(Confidence)$level)
conf3.df = conf2.df %>% rowwise() %>% mutate(count=parseMetricTokens(Confidence)$count)
conf4.df = conf3.df %>% mutate_at(vars(matches("count")),funs(as.numeric))
str(conf4.df)
conf4.df %>% group_by(StrikeID) %>% head
rm(conf.df, conf2.df, conf3.df)

# have a loom at the categorical variable
Cats = c(3)
confCats.df = conf4.df[, Cats]
confCatsTab = categoricalAnalysis(confCats.df)
confCatsTab

# Functional - join of physical and confidence...
# just display for now...  will require a new parse algorithm
funSet = c("StrikeID", "Functional")
fun.df = na.omit(loadFeatures(raw.df, funSet))
fun.df %>% group_by(StrikeID) %>% head

# 5) MEA, CDA
# MEA
# sets - critical notes
#   1) order is important...
#       a) StrikeID MUST BE FIRST (this is the FK back to target table)
#       b) METRIC must be second - this is because we reference columns by index
meaSet = c("StrikeID", "MEA")
mea.df = na.omit(loadFeatures(raw.df, meaSet))

## see https://stackoverflow.com/questions/26003574/r-dplyr-mutate-use-dynamic-variable-names
varLev <- paste(meaSet[2], "level", sep=".")
varCnt <- paste(meaSet[2], "count", sep=".")

# parse out the single line, multiple token input
# note parameter to parseMetricTokens is the column name,  When called this is actually the value
mea2.df = mea.df %>% rowwise() %>% mutate(!!varLev := parseMetricTokens(MEA)$level)
mea3.df = mea2.df %>% rowwise() %>% mutate(!!varCnt := parseMetricTokens(MEA)$count)
mea4.df = mea3.df %>% mutate_at(vars(matches("count")),funs(as.numeric))
mea4.df %>% group_by(StrikeID) %>% head
rm(mea.df, mea2.df, mea3.df)

#### info - trying to use dplky by  col index (to parameteriz) is not working
####  mutate thinks varname is a literal variable name (not a variable)... 
### tricky to build named variable
## using purrr to get some stats
#mea4.df %>% split(.[[3]]) %>% map_dbl(function(x) sum(x[ ,4], na.rm = TRUE))
##  using aggregate (does not work with column index)
#aggregate(mea4.df[ ,4] ~ level, data = mea4.df, sum)
# does work by name
#aggregate(count ~ level, data = mea4.df, sum)

# CDA
cdaSet = c("StrikeID", "CDA")
cda.df = na.omit(loadFeatures(raw.df, cdaSet))

## see https://stackoverflow.com/questions/26003574/r-dplyr-mutate-use-dynamic-variable-names
varLev <- paste(cdaSet[2], "level", sep=".")
varCnt <- paste(cdaSet[2], "count", sep=".")

# parse out the single line, multiple token input
# note parameter to parseMetricTokens is the column name,  When called this is actually the value
cda2.df = cda.df %>% rowwise() %>% mutate(!!varLev := parseMetricTokens(CDA)$level)
cda3.df = cda2.df %>% rowwise() %>% mutate(!!varCnt := parseMetricTokens(CDA)$count)
cda4.df = cda3.df %>% mutate_at(vars(matches("count")),funs(as.numeric))
cda4.df %>% group_by(StrikeID) %>% head
rm(cda.df, cda2.df, cda3.df)

# keep a local copy just in case the internet is down
#write.csv(raw.df, file = paste0(projectDataSet,"raw.csv"))


