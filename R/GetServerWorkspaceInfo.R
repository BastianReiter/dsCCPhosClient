
#' GetServerWorkspaceInfo
#'
#' Check which objects live in server-side R sessions
#'
#' @param DataSources List of DSConnection objects
#'
#' @return A tibble
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


    # Get names of symbols (objects) in all server workspaces
    ServerObjectNames <- DSI::datashield.symbols(conns = DataSources)

    # Get all uniquely occurring object names across servers (although usually the set of symbol names should be the same on all servers)
    UniqueObjectNames <- sort(unique(unlist(ServerObjectNames)))


    # Create tibble with object names and types (can't use mutate() because ds.class() does not work with vectors)
    Output <- bind_cols(Object = UniqueObjectNames,
                        Type = purrr::modify(UniqueObjectNames,
                                             function(symbol) { as.character(dsBaseClient::ds.class(x = symbol,
                                                                                                    datasources = DataSources)[[1]]) }))      # Note: The type definition is based on the object found on the first server (therefore '[[1]]')

    # Create server-specific columns that give feedback on existence of objects (TRUE / FALSE)
    ServerColumns <- NULL

    for (i in 1:length(ServerNames))
    {
        Column <- Output$Object %in% ServerObjectNames[[ServerNames[i]]]

        ServerColumns <- cbind(ServerColumns,      # Using cbind() instead of bind_cols() because it's quiet
                               Column)
    }

    colnames(ServerColumns) <- ServerNames

    # Bind columns to Output data frame
    Output <- bind_cols(Output,
                        ServerColumns)

    # Return Output object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(Output)
}
