
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
    require(dsBaseClient)
    require(DSI)
    require(tidyr)

    # For testing purposes
    #DataSources <- CCPConnections
    #ProjectName <- "Virtual"


    # Initiate output messaging objects
    Messages <- list()
    Messages$TableAvailability <- c(Topic = "Opal table availability")
    Messages$Assignment <- c(Topic = "Object assignment on servers")

    # Get server names
    ServerNames <- names(DataSources)


    # Assess availability of raw data set tables in Opal data base on servers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get overview of available tables on servers
    TableAvailability <- DSI::datashield.tables(conns = DataSources)

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

        # Note: It's important to use 'dplyr::if_else()' instead of 'ifelse' here, otherwise the return won't be a named vector
        Message <- if_else(Row$IsAvailableEverywhere == TRUE,
                           MakeFunctionMessage(Text = paste0("Opal data base table '",
                                                             Row$TableName,
                                                             "' is available on all servers!"),
                                               IsClassSuccess = TRUE),
                           MakeFunctionMessage(Text = paste0("Opal data base table '",
                                                             Row$TableName,
                                                             "' is not available at ",
                                                             Row$NotAvailableAt),
                                               IsClassWarning = TRUE))

        Messages$TableAvailability <- c(Messages$TableAvailability,
                                        Message)
    }


    # Assignment in R server sessions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get table names
    CCPTableNames_Raw <- dsCCPhosClient::Meta_TableNames$TableName_Raw
    CCPTableNames_Curated <- dsCCPhosClient::Meta_TableNames$TableName_Curated

    BundledMessages <- list()

    # Assign tables from Opal DB to object symbols in R session
    for(i in 1:length(CCPTableNames_Raw))
    {
        datashield.assign(conns = DataSources,
                          symbol = paste0("RDS_", CCPTableNames_Curated[i]),
                          value = ifelse(ProjectName == "Virtual",
                                         CCPTableNames_Raw[i],
                                         paste0(ProjectName, ".", CCPTableNames_Raw[i])),
                          id.name = "_id")

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


    # Print and return Messages object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Print messages on console
    PrintMessages(Messages)

    # Return Messages
    return(Messages)
}
