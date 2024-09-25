
#' ModifyConnections
#'
#' Modifies current list of DSConnection-objects created by DataSHIELD functionality or ConnectToCCP() respectively.
#'
#' @param DataSources List of DSConnection objects
#' @param ServersToBeRemoved \code{character vector} of server names to be removed from connections
#'
#' @return A (modified) list of DSConnection-objects
#' @export
#'
#' @author Bastian Reiter
ModifyConnections <- function(DataSources,
                              ServersToBeRemoved = NULL)
{
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check argument eligibility
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }
    if (!(is.character(ServersToBeRemoved)))
    {
        stop("Error: Argument 'ServersToBeRemoved' must be a character vector.", call. = FALSE)
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Modify DataSources (DSConnections)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Remove DSConnection list elements according to 'ServersToBeRemoved' vector
    ModifiedCCPConnections <- DataSources[names(DataSources) %in% ServersToBeRemoved == FALSE]


    # Return DSConnection objects
    return(ModifiedCCPConnections)
}
