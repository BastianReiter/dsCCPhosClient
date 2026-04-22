
#' QuickProcessingRun
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Perform a complete run of CCPhos funtionality with default settings.
#'
#' @param ServerSpecifications \code{data.frame} - Same \code{data.frame} used for login. Used here only for acquisition of server-specific project names (in case they are differing) - Default: \code{NULL} for virtual project
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#'
#' @return A \code{list} of messages
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
QuickProcessingRun <- function(ServerSpecifications = NULL,
                               RunAssignmentChecks = FALSE,
                               DSConnections = NULL,
                               DS.async = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  assert_that(is.flag(RunAssignmentChecks),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Load RDS
  LoadRawDataSet(ServerSpecifications = ServerSpecifications,
                 RunAssignmentChecks = RunAssignmentChecks,
                 DSConnections = DSConnections,
                 DS.async = DS.async)

  # Perform Curation Step
  ds.CurateData(RunAssignmentChecks = RunAssignmentChecks,
                DSConnections = DSConnections,
                DS.async = DS.async)

  # Perform Augmentation Step
  ds.AugmentData(RunAssignmentChecks = RunAssignmentChecks,
                 DSConnections = DSConnections,
                 DS.async = DS.async)

}
