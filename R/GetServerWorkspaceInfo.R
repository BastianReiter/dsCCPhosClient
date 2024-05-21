
#' GetServerWorkspaceInfo
#'
#' Check which objects live in server-side R sessions and collect meta data about them.
#'
#' @param DataSources List of DSConnection objects
#'
#' @return A list
#' @export
#'
#' @author Bastian Reiter
GetServerWorkspaceInfo <- function(DataSources = NULL)
{
    require(dplyr)
    require(dsBaseClient)
    require(DSI)
    require(purrr)
    require(tidyr)

    # For testing purposes
    #DataSources <- CCPConnections

    # Get server names (sorted alphabetically)
    ServerNames <- sort(names(DataSources))


    # 1) Get the names of all objects living in the server-side R sessions and check whether they occur on every server
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get names of symbols (objects) in all server workspaces
    ServerObjectNames <- DSI::datashield.symbols(conns = DataSources)

    # Get all uniquely occurring object names across servers (although usually the set of symbol names should be the same on all servers)
    UniqueObjectNames <- sort(unique(unlist(ServerObjectNames)))

    # Initiate 'ObjectInfo' tibble
    ObjectInfo <- tibble(Object = UniqueObjectNames)

    # Create server-specific columns that give feedback on existence of objects (TRUE / FALSE)
    ServerColumns <- NULL

    for (i in 1:length(ServerNames))
    {
        Column <- ObjectInfo$Object %in% ServerObjectNames[[ServerNames[i]]]

        ServerColumns <- cbind(ServerColumns,      # Using cbind() instead of bind_cols() because it's quiet
                               Column)
    }

    # Name columns according to server names
    colnames(ServerColumns) <- ServerNames

    # Bind columns to Output data frame
    ObjectInfo <- bind_cols(ObjectInfo,
                            ServerColumns)


    #2) Collect meta data about existing objects and attach some of it to 'ObjectInfo'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    MetaData <- ObjectInfo$Object %>%
                    map(function(object)
                         {
                              ObjectMetaData <- ds.GetObjectMetaData(ObjectName = object,
                                                                     DataSources = DataSources)
                              return(ObjectMetaData$FirstEligible)      # The meta data for an object is collected from the first server where the object actually exists (in case it does not exist everywhere)
                         }) %>%
                    setNames(ObjectInfo$Object)


    # Add some meta data to 'ObjectInfo'
    ObjectInfo <- ObjectInfo %>%
                      rowwise() %>%
                          mutate(Class = ifelse(!is.null(MetaData[[Object]]$Class), MetaData[[Object]]$Class, NA),
                                 Length = ifelse(!is.null(MetaData[[Object]]$Length), MetaData[[Object]]$Length, NA),
                                 RowCount = ifelse(!is.null(MetaData[[Object]]$RowCount), MetaData[[Object]]$RowCount, NA),
                                 .after = Object) %>%
                      ungroup()


    # Return list
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(list(Overview = ObjectInfo,
                Details = MetaData))
}
