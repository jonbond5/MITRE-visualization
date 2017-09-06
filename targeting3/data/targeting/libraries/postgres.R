##################################################################
# setup the environment...  
##################################################################
scriptName = "postgres.r";
pdmp_pw = "read2me"
pdmp_host = "roxy.mitre.org" 
pdmp_port = 5433
pdmp_dbname = "pdmp"
pdmp_user = "pdmp_reader"
pdmp_schema = "public"
pdmp_test_table = "der_pdmp_indiana"

#################################################################
# local libraries
#################################################################
# install.packages("RPostgreSQL")
require("RPostgreSQL")

### helper routine to clear up some globals
CleanEnvir <- function(pattern = "_pw") {
    objs <- ls(pos = ".GlobalEnv")
    rm(list = objs[grep(pattern, objs)], pos = ".GlobalEnv")
}
#The column names of this data frame are problematic for databases 
#(and especially PostgreSQL) for a few reasons: 
#   the “.”s in the names can be an issue, and 
#   PostgreSQL expects column names to be all lowercase. 

# make names db safe: no '.' or other illegal characters,
# all lower case and unique
#
# calling:
#   colnames(<dataframe>) = dbSafeNames(colnames(<dataframe>))
dbSafeNames = function(names) {
    names = gsub('[^a-z0-9]+','_',tolower(names))
    names = make.names(names, unique=TRUE, allow_=TRUE)
    names = gsub('.','_',names, fixed=TRUE)
    names
}

dbInit = function(host, port, dbname, user, pword) {

    # loads the PostgreSQL driver
    dbDriv = dbDriver("PostgreSQL")
    
    # creates a connection to the postgres database
    # note that "con" will be used later in each connection to the database
    dbConn = dbConnect(dbDriv, 
                       host = host, 
                       port = port,
                       dbname = dbname,
                       user = user, 
                       password = pword)
    
    ## return the key elements
    return( list(drv=dbDriv, con=dbConn) )
}
dbInitPDMP = function() {
    ## returns list driver, connection
    dbObj = dbInit(pdmp_host, pdmp_port, pdmp_dbname, pdmp_user, pdmp_pw)
    #rm(pdmp_pw, pos = ".GlobalEnv") # removes the password
    CleanEnvir()
    
    # check connection, we require the schema notation
    connected = dbTestConn(dbObj[[2]], pdmp_schema, pdmp_test_table)
    message( sprintf("Connection: host %s, database: %s, status: %s", pdmp_host, pdmp_dbname, connected ) );
    return(dbObj)
}

dbTestConn = function(dbConn, schema, table) {
    # check connection, we require the schema notation
    return(dbExistsTable(dbConn, c(schema, table)))
}
dbTermPDMP = function (dbObj) {
    # close the connection
    dbDisconnect(dbObj[[2]])
    #dbUnloadDriver(dbObj[[1]])
}

