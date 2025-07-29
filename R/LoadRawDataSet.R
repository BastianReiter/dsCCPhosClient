
#' LoadRawDataSet
#'
#' Load raw data set from Opal data base into R session on servers.
#'
#' @param CCPSiteSpecifications \code{data.frame} - Same data.frame used for login. Used here only for akquisition of site-specific project names (in case they are differing) - Default: NULL for virtual project
#' @param DataSources \code{list} of \code{DSConnection} objects
#'
#' @return A \code{list} of messages
#' @export
#'
#' @author Bastian Reiter
LoadRawDataSet <- function(CCPSiteSpecifications = NULL,
                           DataSources)
{
    require(dplyr)
    require(dsBaseClient)
    require(DSI)
    require(purrr)
    require(tidyr)

    #--- For testing purposes ---
    # CCPSiteSpecifications <- NULL
    # DataSources <- CCPConnections


    # Initiate output messaging objects
    Messages <- list()
    Messages$Assignment <- c(Topic = "Object assignment on servers")

    # Get server names
    ServerNames <- names(DataSources)

    # Get table names from meta data
    CCPTableNames_Raw <- dsCCPhosClient::Meta_Tables$TableName_Raw
    CCPTableNames_Curated <- dsCCPhosClient::Meta_Tables$TableName_Curated

    # Check Opal table availability before assignment
    OpalTableAvailability <- GetServerOpalInfo(CCPSiteSpecifications = CCPSiteSpecifications,
                                               DataSources = DataSources)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Assignment in R server sessions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Loop through all participating sites / servers
    for (i in 1:length(ServerNames))
    {
        # In case CCPSiteSpecifications are NULL, server Opal table names are just raw CCP table names
        ServerTableNames <- CCPTableNames_Raw
        ServerProjectName <- NULL

        # If CCPSiteSpecifications are assigned, there can be server-specific project names and therefore server-specific Opal table names
        if (!is.null(CCPSiteSpecifications))
        {
            # Get server-specific project name
            ServerProjectName <- CCPSiteSpecifications %>%
                                      filter(SiteName == ServerNames[i]) %>%
                                      select(ProjectName) %>%
                                      pull()

            # If ServerProjectName is "Virtual" (as it is the case when using virtual infrastructure in CCPhosApp) make the variable empty so that server Opal table names are just raw CCP table names
            if (ServerProjectName == "Virtual") { ServerProjectName <- "" }

            # Else add a dot ('.') to ServerProjectName according to Opal table name nomenclature
            else { ServerProjectName <- paste0(ServerProjectName, ".") }

            # Create vector with server-specific table names (raw CCP table names concatenated with server-specific project name)
            ServerTableNames <- paste0(ServerProjectName, CCPTableNames_Raw)
        }

        # Get vector of all Opal table names that are available on the current server
        AvailableOpalTables <- OpalTableAvailability %>%
                                    rename(IsAvailable = ServerNames[i]) %>%
                                    filter(IsAvailable == TRUE) %>%
                                    pull(TableName) %>%
                                    paste0(ServerProjectName, .)

        # Loop through all tables from Opal DB and assign their content to objects (data.frames) in R session
        for (j in 1:length(ServerTableNames))
        {
            if (ServerTableNames[j] %in% AvailableOpalTables)
            {
                datashield.assign(conns = DataSources[[i]],
                                  symbol = paste0("RDS_", CCPTableNames_Curated[j]),
                                  value = ServerTableNames[j],
                                  id.name = "_id")
            }
        }
    }



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if assignment on servers succeeded
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    BundledMessages <- list()

    # Loop through all CCP tables to get info about assignment on servers
    for(i in 1:length(CCPTableNames_Curated))
    {
        # Make sure assignment was successful on all servers
        ObjectStatus_Table <- ds.GetObjectStatus(ObjectName = paste0("RDS_", CCPTableNames_Curated[i]),
                                                 DataSources = DataSources)

        # Add info about table assignment to Messages
        BundledMessages <- c(BundledMessages,
                             ObjectStatus_Table["ObjectValidity"])   # Must select list element 'ObjectValidity' this way to keep naming of vector and thus class 'Success', 'Warning' and so forth
    }

    # Turn list into (named) vector and add it to Messages
    Messages$Assignment <- c(Messages$Assignment,
                             purrr::list_c(BundledMessages))



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Assign list 'RawDataSet' on all servers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Create list of vectors (one for each site) containing names of actually existing data.frames
    ExistingRDSTables <- paste0("RDS_", CCPTableNames_Curated) %>%
                              map(\(tablename) unlist(ds.exists(x = tablename, datasources = DataSources))) %>%
                              set_names(paste0("RDS_", CCPTableNames_Curated)) %>%
                              list_transpose() %>%
                              map(\(TableNames) names(TableNames[TableNames == TRUE]))

    # For every site, consolidate all existing Raw Data Set tables in one list object called "RawDataSet"
    ExistingRDSTables %>%
        purrr::iwalk(function(RDSTableNames, servername)
                     {
                        dsBaseClient::ds.list(x = RDSTableNames,
                                              newobj = "RawDataSet",
                                              datasources = DataSources[servername])
                     })


    # Make sure assignment of RawDataSet was successful on all servers
    ObjectStatus_RawDataSet <- ds.GetObjectStatus(ObjectName = "RawDataSet",
                                                  DataSources = DataSources)

    # Add info about RawDataSet assignment to Messages
    Messages$Assignment <- c(Messages$Assignment,
                             ObjectStatus_RawDataSet$ObjectValidity)



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print and return Messages object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Print messages on console
    PrintMessages(Messages)

    # Return Messages
    return(Messages)
}
