
#' ds.GetObjectInfo
#'
#' Checks if an object exists on every server in a valid form and returns appropriate messages.
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
#' @author Bastian Reiter
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
    ServerCall <- call("GetObjectInfoDS",
                       ObjectName.S = ObjectName)

    # Get object info from every server
    ObjectInfo <- DSI::datashield.aggregate(conns = DataSources,
                                            expr = ServerCall)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Inspect object info and return message
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Initiate output messaging objects
    Messages <- list()
    Messages$ObjectValidity <- character()


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
    MessageExistence <- MakeFunctionMessage(Text = paste0("The object '", ObjectName, "' has been created on all specified servers."),
                                            IsClassSuccess = TRUE)

    # ...and in case object creation did not succeed on all servers
    if (!(ObjectExistsEverywhere && ObjectNotNullEverywhere))
    {
        MessageExistence <- MakeFunctionMessage(Text = paste0("Error: A valid data object '", ObjectName, "' does NOT exist on ALL specified servers.",
                                                              "\n",
                                                              "It is either ABSENT and/or has no valid content/class, see return.info above.",
                                                              "\n",
                                                              "Please use ds.ls() to identify servers where the object is missing."),
                                                IsClassWarning = TRUE)
    }

    # Add message to list
    Messages$ObjectValidity <- c(Messages$ObjectValidity,
                                 MessageExistence)


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
        ValidityMessage <- MakeFunctionMessage(Text = paste0("'", ObjectName, "' appears valid on all servers."),
                                               IsClassSuccess = TRUE)

        Messages$ObjectValidity <- c(Messages$ObjectValidity,
                                     ValidityMessage)

        return(Messages)
    }

    if (NoErrors == FALSE)
    {
    	  ValidityMessage <- MakeFunctionMessage(Text = paste0("'",ObjectName,"' seems to be invalid in at least one source."),
    	                                         IsClassWarning = TRUE)

    	  Messages$ObjectValidity <- c(Messages$ObjectValidity,
                                     ValidityMessage)

    	  Messages$ServerMessage <- ServerMessage

    	  return(Messages)
    }
}
