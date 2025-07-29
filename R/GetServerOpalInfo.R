
#' GetServerOpalInfo
#'
#' Check if tables are available in server Opal data bases
#'
#' @param CCPSiteSpecifications \code{data.frame} - Same data frame used for login. Used here only for akquisition of site-specific project names (in case they are differing). - Default: NULL for virtual project
#' @param DataSources \code{list} of \code{DSConnection} objects
#'
#' @return A \code{tibble}
#' @export
#'
#' @author Bastian Reiter
GetServerOpalInfo <- function(CCPSiteSpecifications = NULL,
                              DataSources = NULL)
{
    require(dplyr)
    require(DSI)

    #--- For testing purposes ---
    # CCPSiteSpecifications = NULL
    # DataSources = CCPConnections


    # Get server names (sorted alphabetically)
    ServerNames <- sort(names(DataSources))

    # Get table names from meta data
    CCPTableNames_Raw <- dsCCPhosClient::Meta_Tables$TableName_Raw
    CCPTableNames_Curated <- dsCCPhosClient::Meta_Tables$TableName_Curated

    # Get overview of available tables on servers
    TableAvailability <- DSI::datashield.tables(conns = DataSources)

    # Initiate data frame containing info about table availability
    RequiredTableAvailability <- tibble(TableName = CCPTableNames_Raw)

    for (i in 1:length(ServerNames))
    {
        # When connecting to virtual servers 'CCPSiteSpecifications' can be NULL or project name can be 'Virtual' (e.g. in CCPhosApp)
        if (is.null(CCPSiteSpecifications))
        {
            ServerProjectName <- "Virtual"
        }
        else
        {
            # Get server-specific project name
            ServerProjectName <- CCPSiteSpecifications %>%
                                      filter(SiteName == ServerNames[i]) %>%
                                      select(ProjectName) %>%
                                      pull()
        }

        # In case project is virtual, server Opal table names are just raw CCP table names
        ServerTableNames <- CCPTableNames_Raw

        if (ServerProjectName != "Virtual")
        {
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
                                                                     NA),
                                             .after = TableName) %>%
                                      ungroup()

    return(RequiredTableAvailability)
}
