
#' LoadRawDataSet
#'
#' Load raw data set from Opal data base into R session on servers.
#'
#' @param CCPSiteSpecifications \code{data.frame} | Same data frame used for login. Used here only for akquisition of site-specific project names (in case they are differing). | Default: NULL for virtual project
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages
#' @export
#'
#' @examples
#' @author Bastian Reiter
LoadRawDataSet <- function(CCPSiteSpecifications = NULL,
                           DataSources)
{
    require(dplyr)
    require(dsBaseClient)
    require(DSI)
    require(tidyr)

    # For testing purposes
    #DataSources <- CCPConnections
    #ProjectName <- "Virtual"


    # Initiate output messaging objects
    Messages <- list()
    Messages$Assignment <- c(Topic = "Object assignment on servers")

    # Get server names
    ServerNames <- names(DataSources)

    # Get table names from meta data
    CCPTableNames_Raw <- dsCCPhosClient::Meta_TableNames$TableName_Raw
    CCPTableNames_Curated <- dsCCPhosClient::Meta_TableNames$TableName_Curated


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Assignment in R server sessions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Loop through all participating sites / servers
    for (i in 1:length(ServerNames))
    {
        # In case project is virtual, server Opal table names are just raw CCP table names
        ServerTableNames <- CCPTableNames_Raw

        # If project is not virtual, there can be server-specific project names and therefore server-specific Opal table names
        if (!is.null(CCPSiteSpecifications))
        {
            # Get server-specific project name
            ServerProjectName <- CCPSiteSpecifications %>%
                                      filter(SiteName == ServerNames[i]) %>%
                                      select(ProjectName) %>%
                                      pull()

            # Create vector with server-specific table names (raw CCP table names concatenated with server-specific project name)
            ServerTableNames <- paste0(ServerProjectName, ".", CCPTableNames_Raw)
        }

        # Loop through all tables from Opal DB and assign their content to objects (data.frames) in R session
        for(i in 1:length(ServerTableNames))
        {
            datashield.assign(conns = DataSources,
                              symbol = paste0("RDS_", CCPTableNames_Curated[i]),
                              value = ServerTableNames[i],
                              id.name = "_id")
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
        ObjectInfo_Table <- ds.GetObjectInfo(ObjectName = paste0("RDS_", CCPTableNames_Curated[i]),
                                             DataSources = DataSources)

        # Add info about table assignment to Messages
        BundledMessages <- c(BundledMessages,
                             ObjectInfo_Table)
    }


    # Turn list into (named) vector and add it to Messages
    Messages$Assignment <- c(Messages$Assignment,
                             purrr::list_c(BundledMessages))



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Assign list 'RawDataSet'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Consolidate all raw data set tables in one list object called "RawDataSet"
    dsBaseClient::ds.list(x = paste0("RDS_", CCPTableNames_Curated),
                          newobj = "RawDataSet",
                          datasources = DataSources)

    # Make sure assignment of RawDataSet was successful on all servers
    ObjectInfo_RawDataSet <- ds.GetObjectInfo(ObjectName = "RawDataSet",
                                              DataSources = DataSources)

    # Add info about RawDataSet assignment to Messages
    Messages$Assignment <- c(Messages$Assignment,
                             purrr::list_c(ObjectInfo_RawDataSet))



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print and return Messages object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Print messages on console
    PrintMessages(Messages)

    # Return Messages
    return(Messages)
}
