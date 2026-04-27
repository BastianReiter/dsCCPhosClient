
#' ds.GetRDSValidationReport
#'
#' Triggers validation of Raw Data Set on server and requests a report.
#'
#' Linked to server-side \code{AGGREGATE} method \code{GetRDSValidationReportDS()}
#'
#' @param RawDataSetName \code{string} - Name of 'RawDataSet' object on server - Default: 'RawDataSet'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#'
#' @return A \code{list} of \code{tibbles} containing output of validation
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetRDSValidationReport <- function(RawDataSetName = "CCP.RawDataSet",
                                      DSConnections = NULL,
                                      DS.async = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  ValidationReport <- DSI::datashield.aggregate(conns = DSConnections,
                                                expr = call("GetRDSValidationReportDS",
                                                            RawDataSetName.S = RawDataSetName),
                                                async = DS.async)

#-------------------------------------------------------------------------------
  return(ValidationReport)
}
