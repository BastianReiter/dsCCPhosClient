
#' LoadRawDataSet
#'
#' Load raw data set from Opal data base into R session on servers.
#'
#' @param DataSources List of DSConnection objects
#' @param ProjectName Name of project as stated in Opal. Use Default "Virtual" for virtual CCP connection.
#'
#' @return A list of messages
#' @export
#'
#' @examples
#' @author Bastian Reiter
LoadRawDataSet <- function(DataSources,
                           ProjectName = "Virtual")
{
    require(dplyr)
    require(DSI)
    require(tidyr)

    # For testing purposes
    # DataSources <- CCPConnections
    # ProjectName <- "Virtual"


    # Initiate output messaging objects
    Messages <- list()
    Messages$TableAvailability <- character()
    Messages$Assignment <- character()

    # Get server names
    ServerNames <- names(DataSources)


    # Assess availability of raw data set tables on servers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get overview of available tables on servers
    TableAvailability <- DSI::datashield.tables(conns = CCPConnections)

    RequiredTableAvailability <- tibble(TableName = dsCCPhosClient::Meta_TableNames$TableName_Raw)

    for (i in 1:length(ServerNames))
    {
        RequiredTableAvailability <- RequiredTableAvailability %>%
                                          mutate(!!ServerNames[i] := TableName %in% TableAvailability[[ServerNames[i]]])
    }

    RequiredTableAvailability <- RequiredTableAvailability %>%
                                      rowwise() %>%
                                      mutate(IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                                             NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                                     paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                                     NA)) %>%
                                      ungroup()

    # Compile output message concerning one table each and add it to Messages
    for (i in 1:nrow(RequiredTableAvailability))
    {
        Row <- RequiredTableAvailability[i, ]
        Message <- paste0("Table '",
                          Row$TableName,
                          ifelse(Row$IsAvailableEverywhere == TRUE,
                                 "' is available on all servers!",
                                 paste0("' is not available on ", Row$NotAvailableAt)))
        Messages$TableAvailability <- c(Messages$TableAvailability,
                                        Message)
    }


    # Assignment in R server sessions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get table names
    CCPTableNames_Raw <- dsCCPhosClient::Meta_TableNames$TableName_Raw
    CCPTableNames_Curated <- dsCCPhosClient::Meta_TableNames$TableName_Curated

    # Assign tables from Opal DB to object symbols in R session
    for(i in 1:length(CCPTableNames_Raw))
    {
        datashield.assign(conns = CCPConnections,
                          symbol = paste0("RDS_", CCPTableNames_Curated[i]),
                          value = ifelse(ProjectName == "Virtual",
                                         CCPTableNames_Raw[i],
                                         paste0(ProjectName, ".", CCPTableNames_Raw[i])),
                          id.name = "_id")

        # Make sure assignment was successful on all servers
        ObjectInfo_Table <- ds.GetObjectInfo(ObjectName = paste0("RDS_", CCPTableNames_Curated[i]),
                                             DataSources = CCPConnections)

        # Add info about table assignment to Messages
        Messages$Assignment <- c(Messages$Assignment,
                                 paste0(unlist(ObjectInfo_Table), collapse = "\n"))
    }


    # Consolidate all raw data set tables in one list object called "RawDataSet"
    ds.list(x = paste0("RDS_", CCPTableNames_Curated),
            newobj = "RawDataSet",
            datasources = CCPConnections)

    # Make sure assignment of RawDataSet was successful on all servers
    ObjectInfo_RawDataSet <- ds.GetObjectInfo(ObjectName = "RawDataSet",
                                              DataSources = CCPConnections)

    # Add info about RawDataSet assignment to Messages
    Messages$Assignment <- c(Messages$Assignment,
                             paste0(unlist(ObjectInfo_RawDataSet), collapse = "\n"))


    # Return statement
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Additionally to return, print messages on console
    cat(paste0(unlist(Messages), collapse = "\n"))

    return(Messages)
}
