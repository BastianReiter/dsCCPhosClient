
#' ds.UnpackCuratedDataSet
#'
#' Make tables within Curated Data Set (\code{list} object) directly addressable in R server sessions
#'
#' Linked to server-side \code{ASSIGN} function \code{ExtractFromListDS()}
#'
#' @param CuratedDataSetName \code{string} - Name of Curated Data Set object (list) on server - Default: 'CCP.CuratedDataSet'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.UnpackCuratedDataSet <- function(CuratedDataSetName = "CCP.CuratedDataSet",
                                    DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(CuratedDataSetName))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  Messages$Assignment <- list()

  # Get curated CCP table names
  CCPTableNames.CDS <- dsCCPhosClient::Meta.Tables$TableName.Curated

  for(i in 1:length(CCPTableNames.CDS))
  {
      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = paste0("CCP.CDS.", CCPTableNames.CDS[i]),      # E.g. 'CCP.CDS.Metastasis'
                             value = call("ExtractFromListDS",
                                          ListName.S = CuratedDataSetName,
                                          ObjectName.S = CCPTableNames.CDS[i]))

      # Call helper function to check if object assignment succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = paste0("CCP.CDS.", CCPTableNames.CDS[i]),
                                                  DSConnections = DSConnections))
  }

  # Turn list into (named) vector
  Messages$Assignment <- purrr::list_c(Messages$Assignment)

  # Add topic element to start of vector
  Messages$Assignment <- c(Topic = "Object assignment on servers",
                           Messages$Assignment)


#--- Print messages and invisibly return Messages object -----------------------

  # Print messages on console
  PrintMessages(Messages)

  # Return Messages invisibly
  invisible(Messages)
}
