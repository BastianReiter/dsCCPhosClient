
#' ds.GetObjectMetaData
#'
#' Gathers meta data about an R object.
#'
#' Linked to server-side AGGREGATE method GetObjectMetaDataDS()
#'
#' @param ObjectName String | Name of object on server
#' @param DataSources List of DSConnection objects
#'
#' @return A list of server returns
#' @export
#'
#' @author Bastian Reiter
ds.GetObjectMetaData <- function(ObjectName,
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
# Call GetObjectMetaDataDS() on every server
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For testing purposes
# ObjectName <- "ADS_Patients"
# DataSources <- CCPConnections


require(purrr)

# Construct server-side function call
ServerCall <- call("GetObjectMetaDataDS",
                   ObjectName.S = ObjectName)

# Get object meta data from every server
ObjectMetaData <- DSI::datashield.aggregate(conns = DataSources,
                                            expr = ServerCall)

# Get logical vector indicating existence of object on servers
ObjectExistence <- ObjectMetaData %>%
                        map_lgl(\(metadatalist) metadatalist$ObjectExists)

# Get names of all servers that host the object (so everywhere it exists)
EligibleServers <- names(DataSources)[ObjectExistence]

# Add to output list: Meta data from any (first eligible) server that hosts the object in question
ObjectMetaData$FirstEligible <- if(!is.null(EligibleServers)) { ObjectMetaData[[first(EligibleServers)]] } else { NULL }

return(ObjectMetaData)
}
