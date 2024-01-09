
#' ds.GetObjectInfo
#'
#' What it does
#'
#' Linked to server-side AGGREGATE method GetObjectInfoDS()
#'
#' @param ObjectName String | Name of object on server
#' @param DataSources List of DSConnection objects
#'
#' @return
#' @export
#'
#' @examples
ds.GetObjectInfo <- function(ObjectName,
                             DataSources)
{
    # Look for DS connections
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    # Ensure DataSources is a list of DSConnection-class
    if (!(is.list(DataSources) && all(unlist(lapply(DataSources, function(d) {methods::is(d,"DSConnection")})))))
    {
        stop("'DataSources' were expected to be a list of DSConnection-class objects", call. = FALSE)
    }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Call GetObjectInfoDS() on every server
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Construct server-side function call
    ServerCall <- call("GetObjectInfoDS", ObjectName)

    # Get object info from every server
    ObjectInfo <- DSI::datashield.aggregate(conns = DataSources,
                                            expr = ServerCall)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inspect object info and return message
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    CountSources <- length(ObjectInfo)

    ObjectExistsEverywhere <- TRUE
    ObjectNotNullEverywhere <- TRUE

    for (i in 1:CountSources)
    {
    	  if (!ObjectInfo[[i]]$ObjectExists)
    	  {
    		    ObjectExistsEverywhere <- FALSE
    		}
    	  if (is.null(ObjectInfo[[i]]$ObjectClass) || ("ABSENT" %in% ObjectInfo[[i]]$ObjectClass))
    	  {
    		    ObjectNotNullEverywhere <- FALSE
    		}
    }

    # Return message in case non-null object has been created on all servers
    ReturnMessage <- paste0("A data object <", ObjectName, "> has been created in all specified data sources.")

    # ...and in case object creation did not succeed on all servers
    if (!(ObjectExistsEverywhere && ObjectNotNullEverywhere))
    {
        ReturnMessage <- list(paste0("Error: A valid data object <", ObjectName, "> does NOT exist in ALL specified data sources."),
                              paste0("It is either ABSENT and/or has no valid content/class, see return.info above."),
                              paste0("Please use ds.ls() to identify where missing."))
    }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look for messages linked to object on server to check for possible errors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Call dsBase::messageDS() to get possible server-side message about object
    ServerCall <- call("messageDS", ObjectName)

    ServerMessage <- DSI::datashield.aggregate(conns = DataSources,
                                               expr = ServerCall)

    NoErrors <- TRUE

    for (i in 1:CountSources)
    {
        if (ServerMessage[[i]] != "ALL OK: there are no studysideMessage(s) on this datasource")      # This is a string defined in dsBase::messageDS()
        {
    		    NoErrors <- FALSE
    		}
    }


    if (NoErrors == TRUE)
    {
        ValidityCheck <- paste0("<", ObjectName, "> appears valid in all sources")

        return(list(ObjectCreated = ReturnMessage,
    	              ObjectValidity = ValidityCheck))
    }

    if (NoErrors == FALSE)
    {
    	  ValidityCheck <- paste0("<",ObjectName,"> invalid in at least one source.")

    	  return(list(ObjectCreated = ReturnMessage,
    	              ObjectValidity = ValidityCheck,
    	              ObjectMessage = ServerMessage))
    }
}
