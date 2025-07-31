
#' ds.AugmentData
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Transforms curated CCP core data set (CDM) into augmented data set (ADM)
#'
#' Linked to server-side ASSIGN method \code{AugmentDataDS()}
#'
#' @param CuratedDataSetName \code{string} - Name of the Curated Data Set object on server | Default: 'CuratedDataSet'
#' @param OutputName \code{string} - Name of output object to be assigned on server | Default: 'AugmentationOutput'
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of variables containing messages about object assignment for monitoring purposes.
#' @export
#'
#' @author Bastian Reiter
ds.AugmentData <- function(CuratedDataSetName = "CuratedDataSet",
                           OutputName = "AugmentationOutput",
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)

  #--- For testing purposes ---
  # CuratedDataSetName <- "CuratedDataSet"
  # OutputName <- "AugmentationOutput"
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Initiate output messaging objects
  Messages <- list()
  Messages$Assignment <- list()
  Messages$AugmentationCompletion <- list()


  # 1) Trigger dsCCPhos::AugmentDataDS()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Construct the server-side function call
  ServerCall <- call("AugmentDataDS",
                     CuratedDataSetName.S = CuratedDataSetName)

  # Execute the server-side function call
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = ServerCall)

  # Call helper function to check if assignment of AugmentationOutput succeeded
  Messages$Assignment <- c(Messages$Assignment,
                           ds.GetObjectStatus(ObjectName = OutputName,
                                              DSConnections = DSConnections))



  # 2) Extract objects from list returned by AugmentDataDS() and assign them to R server sessions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  AugmentationOutputObjects <- c("AugmentedDataSet",
                                 "AugmentationReport",
                                 "AugmentationMessages")

  for(i in 1:length(AugmentationOutputObjects))
  {
      # Construct the server-side function call
      ServerCall <- call("ExtractFromListDS",
                         ListName.S = OutputName,
                         ObjectName.S = AugmentationOutputObjects[i])

      # Execute server-side assign function
      DSI::datashield.assign(conns = DSConnections,
                             symbol = AugmentationOutputObjects[i],
                             value = ServerCall)

      # Call helper function to check if object assignment succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = AugmentationOutputObjects[i],
                                                  DSConnections = DSConnections))
  }

  # Turn list into (named) vector
  Messages$Assignment <- purrr::list_c(Messages$Assignment)

  # Add topic element to start of vector
  Messages$Assignment <- c(Topic = "Object assignment on servers",
                           Messages$Assignment)



  # 3) Get AugmentationMessages objects from servers (as a list of lists) and create completion check object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ServerCall <- call("GetReportingObjectDS",
                     ObjectName.S = "AugmentationMessages")

  AugmentationMessages <- DSI::datashield.aggregate(conns = DSConnections,
                                                    expr = ServerCall)

  # Create table object for output
  AugmentationCompletionCheck <- AugmentationMessages %>%
                                      map(\(SiteMessages) tibble(CheckAugmentationCompletion = SiteMessages$CheckAugmentationCompletion) ) %>%
                                      list_rbind(names_to = "SiteName")

  # Create vector of messages informing about Augmentation completion
  Messages$AugmentationCompletion <- AugmentationMessages %>%
                                          imap(function(SiteMessages, sitename)
                                               {
                                                  case_when(SiteMessages$CheckAugmentationCompletion == "green" ~ MakeFunctionMessage(Text = paste0("Augmentation on server '", sitename, "' performed successfully!"),
                                                                                                                                 IsClassSuccess = TRUE),
                                                            SiteMessages$CheckAugmentationCompletion == "yellow" ~ MakeFunctionMessage(Text = paste0("Augmentation on server '", sitename, "' performed with warnings!"),
                                                                                                                                  IsClassWarning = TRUE),
                                                            SiteMessages$CheckAugmentationCompletion == "red" ~ MakeFunctionMessage(Text = paste0("Augmentation on server '", sitename, "' could not be performed!"),
                                                                                                                               IsClassFailure = TRUE),
                                                            TRUE ~ MakeFunctionMessage(Text = paste0("Augmentation on server '", sitename, "' could not be assessed."),
                                                                                       IsClassFailure = TRUE))
                                               }) %>%
                                          list_c()

  # Add topic element to start of vector
  Messages$AugmentationCompletion <- c(Topic = "Augmentation process completion",
                                       Messages$AugmentationCompletion)



  # Print messages and return Messages object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Print messages on console
  PrintMessages(Messages)

  # Return Messages and Augmentation completion check object
  return(list(Messages = Messages,
              AugmentationCompletionCheck = AugmentationCompletionCheck))
}
