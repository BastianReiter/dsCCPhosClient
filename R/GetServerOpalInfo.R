
#' GetServerOpalInfo
#'
#' Check if tables are available in server Opal data bases
#'
#' @param CCPSiteSpecifications \code{data.frame} | Same data frame used for login. Used here only for akquisition of site-specific project names (in case they are differing). | Default: NULL for virtual project
#' @param DataSources List of DSConnection objects
#'
#' @return A tibble
#' @export
#'
#' @author Bastian Reiter
GetServerOpalInfo <- function(CCPSiteSpecifications = NULL,
                              DataSources = NULL)
{
    require(dplyr)
    require(DSI)

    # For testing purposes
    # CCPSiteSpecifications = NULL
    # DataSources = CCPConnections


    # Get server names (sorted alphabetically)
    ServerNames <- sort(names(DataSources))

    # Get table names from meta data
    CCPTableNames_Raw <- dsCCPhosClient::Meta_TableNames$TableName_Raw
    CCPTableNames_Curated <- dsCCPhosClient::Meta_TableNames$TableName_Curated

    # Get overview of available tables on servers
    TableAvailability <- DSI::datashield.tables(conns = DataSources)

    # Initiate data frame containing info about table availability
    RequiredTableAvailability <- tibble(TableName = CCPTableNames_Raw)

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

        # For every server, check if CCP raw data tables with site-specific correspondent names are existent in 'TableAvailability'
        RequiredTableAvailability <- RequiredTableAvailability %>%
                                          mutate(!!ServerNames[i] := ServerTableNames %in% TableAvailability[[ServerNames[i]]])
    }

    RequiredTableAvailability <- RequiredTableAvailability %>%
                                      rowwise() %>%
                                      mutate(IsAvailableEverywhere = all(c_across(all_of(ServerNames)) == TRUE),
                                             NotAvailableAt = ifelse(IsAvailableEverywhere == FALSE,
                                                                     paste0(ServerNames[c_across(all_of(ServerNames)) == FALSE], collapse = ", "),
                                                                     NA)) %>%
                                      ungroup()

    return(RequiredTableAvailability)
}
