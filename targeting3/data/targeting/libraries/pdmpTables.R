##################################################################
# setup the environment...  
##################################################################
scriptName = "pdmpTables.R";

# event table dataframe generation
eventWindow = 7

# the derived table
derPDMPEvents = "SELECT name_dob_hash15, payment_code, product_ndc, non_proprietary_name, prescription_number_hash, refill_code, date_filled 
                 FROM public.der_pdmp_indiana"
derPDMP = "SELECT * FROM public.der_pdmp_indiana"
derPDMP_limit = 100000   # ~ 10% of the 12M rows in table
eventTableDropCols = c("payment_code", "date_filled")


# this will run on database, but is not modifiable
doiQuery2 = "SELECT productndc, nonproprietaryname 
            FROM public.fda_product p
            WHERE 
            (
                p.nonproprietaryname ilike  '%Bupren%' 
                or p.proprietaryname    ilike  '%Bupren%' 
                or p.nonproprietaryname ilike  '%Buprenex%' 
                or p.proprietaryname    ilike  '%Buprenex%' 
                or p.nonproprietaryname ilike  '%Butrans%'  
                or p.proprietaryname    ilike  '%Butrans%'  
                or p.nonproprietaryname ilike  '%Cizdol%'
                or p.proprietaryname    ilike  '%Cizdol%'
                or p.nonproprietaryname ilike  '%Subutex%'
                or p.proprietaryname    ilike  '%Subutex%'
                or p.nonproprietaryname ilike  '%Suboxone%' 
                or p.proprietaryname    ilike  '%Suboxone%' 
                or p.nonproprietaryname ilike  '%Belbuca%'  
                or p.proprietaryname    ilike  '%Belbuca%'  
                or p.nonproprietaryname ilike  '%Zubsolv%' 
                or p.proprietaryname    ilike  '%Zubsolv%' 
                or p.nonproprietaryname ilike  '%Bunavail%'
                or p.proprietaryname    ilike  '%Bunavail%'
            )"

# added flexibility to modify the key word matches for drugs of interest
doiQuery = "SELECT productndc, nonproprietaryname, dosageformname, routename, activenumeratorstrength, activeingredunit
            FROM public.fda_product"
doiRegExKeyWordsALL = c("Bupren", "Cizdol", "Subutex", "Suboxone", "Belbuca", "Zubsolv", "Bunavail")
doiRegExKeyWords = c("Bupren", "Buprenex", "Butrans", "Cizdol", "Subutex", "Suboxone", "Belbuca", "Zubsolv", "Bunavail")
doiRegEx = paste(doiRegExKeyWords, collapse = "|")

#################################################################
# local libraries
#################################################################
# install.packages("RPostgreSQL")
require("dplyr")
#library(tidyverse)
library(stringr)
library(data.table)


##################################################################
#  Build the drugs_of_interest data frame
##################################################################
loadDrugsOfInterest <- function(dbConn) {
    # Run an SQL statement by creating first a resultSet object
    rs <- dbSendQuery(dbConn, statement = doiQuery);

    # we now fetch records from the resultSet into
    raw.df <- fetch(rs, n = -1) # -1 extract all rows
    head(raw.df)
    count(raw.df)
    
    ## flip over to dplyer and filter data set down to opiods of interest
    doi = filter(raw.df, grepl(doiRegEx, nonproprietaryname, ignore.case = TRUE))
    
    # doi = raw.df %>% 
    #     filter(str_detect(proprietaryname,doiRegEx)) %>%
    #     filter(str_detect(nonproprietaryname,doiRegEx))
    #detach(raw.df, unload = TRUE)
    # unload the result set
    dbClearResult(rs)
    
    return(doi)
}

isDrugOfInterest <- function(doi.df, product_ndc) {
    
    result = FALSE
    if(missing(product_ndc) | missing(doi.df))
        return(result)
    
    drugs.df = doi.df %>% 
               filter(product_ndc %in% doi.df$productndc)
    if(count(drugs.df) > 0)
        result=TRUE
    
    return(result)
}
getDrugOfInterest <- function(doi.df, product_ndc) {
    
    if(missing(product_ndc) | missing(doi.df))
        return(NULL)
    
    drugs.df = doi.df %>% 
        filter(doi.df$productndc == product_ndc)
    return(drugs.df)
}

loadDerivedPDMPEvents <- function(dbConn, maxRows) {
    loadDerivedPDMP(dbConn, derPDMPEvents, maxRows )
}
loadDerivedPDMPRaw <- function(dbConn, maxRows) {
    loadDerivedPDMP(dbConn, derPDMP, maxRows )
}
loadDerivedPDMP <- function(dbConn, query, maxRows) {

    if(missing(maxRows))
        maxRows = derPDMP_limit
    
    ptime_start <- proc.time()
    # Run an SQL statement by creating first a resultSet object
    rs <- dbSendQuery(dbConn, 
                      statement = query);
    ptime_stop <- proc.time()
    ptime_stop - ptime_start
    
    # we now fetch records from the resultSet into
    ptime_start <- proc.time()
    raw.df <- fetch(rs, n = maxRows) # -1 extract all rows
    ptime_stop <- proc.time()
    ptime_stop - ptime_start
    
    # clean up the result set...  
    dbClearResult(rs)
    return(raw.df)
}

# create a data frame of "drug events"
# returns a dataframe of:
#   name_dob_hash15 (Key, not unique)
#   date (derived from a window arg)
#   prescription
#   "label" is define below
#
# Algorithm:
#
# For each person p, do the following:
# Take each of his prescriptions and determine if the drug was 
#   (a.) paid by Medicaid which is payment_code = 2 
#   (b.) was a subset of DOI
# Consider ONLY the FIRST date, subtract 7 days and take that as the event (not the rest).
#
# resulting data frame
#   name_dob_hash15, product_ndc, non_proprietary_name, prescription_number_hash, refill_code, eventLabel, eventDate
buildEvents <- function(pdmp.df, doi.df, eventWindow) {
    if(missing(pdmp.df) | missing(doi.df))
        # no data, no go
        return(NULL)
    if(missing(eventWindow))
        eventWindow = eventWindow
    # # positive events is the basis of the analysis set (labels are generated)
    # eventSet.df = derPDMP.df %>%
    #     filter(payment_code == '2') #%>%
    #     #filter(product_ndc %in% doi.df$productndc)
    # dim(eventSet.df)
    # eventSet2.df = derPDMP.df %>%
    #     #filter(payment_code == '2') #%>%
    #     filter(product_ndc %in% doi.df$productndc)
    # dim(eventSet2.df)
    # eventSet2.df[, "payment_code" == '2']
    # eventSet3.df = derPDMP.df %>%
    #     filter(payment_code == '2') %>%
    #     filter(product_ndc %in% doi.df$productndc)
    # dim(eventSet3.df)
    
    ## filter the two tables down to the correct event set
    eventSet.df = pdmp.df %>%
                    filter(payment_code == '2') %>%
                    filter(product_ndc %in% doi.df$productndc)
    ## with the correct eventSet, reformat the data into correct form
    # need eventLabel and eventDate (derived from eventWindow)
    events.df = eventSet.df %>%
                mutate(eventLabel = 1) %>%
                mutate(eventDate = date_filled - eventWindow) %>%
                select(-one_of(eventTableDropCols))
    str(events.df)
    # this is the event table keyed off name_dob_hash15
    return(events.df)
}

