#' ds.ADS_Postprocess
#'
#' Postprocesses the augmented data set (ADS)
#'
#' Linked to server-side ASSIGN method ADS_PostprocessDS()
#'
#' @param AugmentedDataSetName String | Name of the Augmented Data Set object on server | Default: 'AugmentedDataSet'
#' @param OutputName String | Name of output object to be assigned on server | Default: 'AugmentationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of variables containing messages about object assignment for monitoring purposes.
#' @export
#'
#' @examples
#' @author Daniel Maier
#'

ds.ADS_Postprocess <- function(AugmentedDataSetName = "AugmentedDataSet",
                           OutputName = "ADS_Postprocessed",
                           DataSources = NULL)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


  ServerCall <- call("ADS_PostprocessDS",
                     data = AugmentedDataSetName)

  # Execute the server-side function call
  DSI::datashield.assign(conns = DataSources,
                         symbol = OutputName,
                         value = ServerCall)

}



