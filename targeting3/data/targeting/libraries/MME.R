##################################################################
# morphine milligram quivalent (MME)
##################################################################
scriptName = "MME.R"

# algorithm 
# (adopted from CDC [https://www.cdc.gov/drugoverdose/pdf/calculating_total_daily_dose-a.pdf])
# (buprenorphine is derived from https://www.cms.gov/medicare/prescription-drug-coverage/prescriptiondrugcovcontra/downloads/opioid-morphine-eq-conversion-factors-march-2015.pdf)
# For each patient, for each opioid (mg/day) prescription
#   1) determine the total daily amount of each opioid patient takez
#   2) Convert each to MME: multiple dose for each opioid by conversion factor (table lookup)
#   3) Sum sub MME's together

# conversion table
MME_xls = ".xlsx"
MME_csv = ".csv"
MME_ext = MME_csv
MME_name = "MME"
MME_File = paste0(MME_name, MME_ext)

# loaded from excel
MME_daily_dosage_threshold = 90 # MME/Day units

## need a way to derive from the factors table directly
factorsRegExKeyWords = c("Bupren", "Codeine", "Fantanyl", "Hydrocodone", "Hydromorphone", "Methadone", "Morphine", "Oxycodone", "Oxymorphone")
factorsRegEx = paste(factorsRegExKeyWords, collapse = "|")

# need to group the positive.df for easy access...
groupByPatientPrescription = c("name_dob_hash15", "prescription_number_hash", "refill_code")

##################################################################
#  Load MME conversion table 
##################################################################
loadMMETableFromPath <- function(path) {
    if(missing(path))
        # no data, no go
        return(NULL)
    MME_fullPath = paste0(path, MME_File)
    MME.df = loadMMETable(MME_fullPath)
    return(MME.df)
}

loadMMETable <- function(fullFilePath) {
    if(missing(fullFilePath))
        # no data, no go
        return(NULL)
    MME.df = read.csv(fullFilePath, sep=',', header=TRUE)
    return(MME.df)
}
getDrugFactor <- function(doi.df, product_ndc) {
    
    if(missing(product_ndc) | missing(doi.df))
        return(NULL)
    
    drugs.df = doi.df %>% 
        filter(doi.df$productndc == product_ndc)
    return(drugs.df)
}

# groupByPatientPrescription = c("name_dob_hash15", "prescription_number_hash", "refill_code")
# MME.df = positive.df %>%
#     group_by_(.dots = groupByPatientPrescription) %>%
#     mutate(MME = calculatePerPrescriptionMME(quantity, product_ndc, doi.df, factorsMME.df))

calculatePrescriptionMME <- function(quantity, product_ndc, positive.df, doi.df, factorsMME.df) {

    if(missing(quantity) | missing(product_ndc) | missing(positive.df) | missing(doi.df) | missing(factorsMME.df))
        return(NULL)
    
    # mutate the table adding the MME
    MME.df = positive.df %>%
        group_by_(.dots = groupByPatientPrescription) %>%
        mutate(MME = perPrescriptionMME(quantity, product_ndc, doi.df, factorsMME.df))
    
    return(MME.df)
}

## internal API call, use via calculatePrescriptionMME
# update indiana_range r
# set opioid_mg_per_day = 
#     (select sum(calc_opioid(p.substancename, p.activenumeratorstrength * r.quantity / r.days_supply)) 
#      from opioid_pills p where p.productndc = r.product_ndc)
# where r.product_ndc in (select productndc from opioid_pills);

perPrescriptionMME <- function(quantity, product_ndc, doi.df, factorsMME.df) {
    MME = 0
    if(isDrugOfInterest(doi.df, product_ndc)) {
        MME = calculateMME(quantity, product_ndc, doi.df, factorsMME.df)
    }
    return(MME)
    
}
# internal routine, should be called from calculatePerPrescriptionMME
calculateMME <- function(quantity, product_ndc, doi.df, factorsMME.df) {
    MME = 0
    product.df = getDrugOfInterest(doi.df, product_ndc)
    if(is.null(product.df))
        return(MME)
    likeName = substr(product.df$nonproprietaryname, 1, 6)
    ## flip over to dplyer and filter data set down to opiods of interest
    opioidFactor.df = filter(factorsMME.df, grepl(likeName, OPIOID, ignore.case = TRUE))
    MME = quantity * opioidFactor.df$ConversionFactor
    return(MME)
    
}
