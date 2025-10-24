
#' ds.AugmentData
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Transforms curated data set (CDS) into augmented data set (ADS)
#'
#' Linked to server-side ASSIGN method \code{AugmentDataDS()}
#'
#' @param CuratedDataSetName \code{string} - Name of the Curated Data Set object on server - Default: 'CCP.CuratedDataSet'
#' @param OutputName \code{string} - Name of output object to be assigned on server - Default: 'CCP.AugmentationOutput'
#' @param RunAssignmentChecks \code{logical} Indicating whether assignment checks should be performed or omitted for reduced execution time - Default: \code{TRUE}
#' @param UnpackAugmentedDataSet \code{logical} indicating whether the Augmented Data Set \code{list} should be unpacked so that tables \code{data.frames} are directly accessible - Default: \code{TRUE}
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of variables containing messages about object assignment for monitoring purposes.
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.AugmentData <- function(CuratedDataSetName = "CCP.CuratedDataSet",
                           OutputName = "CCP.AugmentationOutput",
                           RunAssignmentChecks = TRUE,
                           UnpackAugmentedDataSet = TRUE,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  #--- For Testing Purposes ---
  # CuratedDataSetName <- "CuratedDataSet"
  # OutputName <- "AugmentationOutput"
  # RunAssignmentChecks <- TRUE
  # UnpackAugmentedDataSet <- TRUE
  # DSConnections <- CCPConnections

  # --- Argument Validation ---

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Initiate output messaging objects
  Messages <- list()
  if (RunAssignmentChecks == TRUE) { Messages$Assignment <- list() }
  Messages$AugmentationCompletion <- list()


  # 1) Trigger dsCCPhos::AugmentDataDS()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Execute the server-side function call
  DSI::datashield.assign(conns = DSConnections,
                         symbol = OutputName,
                         value = call("AugmentDataDS",
                                      CuratedDataSetName.S = CuratedDataSetName))

  if (RunAssignmentChecks == TRUE)
  {
      # Call helper function to check if assignment of AugmentationOutput succeeded
      Messages$Assignment <- c(Messages$Assignment,
                               ds.GetObjectStatus(ObjectName = OutputName,
                                                  DSConnections = DSConnections))
  }


# Extract objects from list returned by AugmentDataDS() and assign them to R server sessions
#-------------------------------------------------------------------------------

  # Temporary (Generalize function later on)
  Module <- "CCP"

  # Named vector determining how objects inside AugmentationOutput list created on servers should be extracted
  ObjectNames <- setNames(c(paste0(Module, ".AugmentedDataSet"),
                            paste0(Module, ".AugmentationReport"),
                            "Messages"),
                          nm = c("AugmentedDataSet",
                                 "AugmentationReport",
                                 "Messages"))

  # Extract objects from AugmentationOutput list
  for (i in 1:length(ObjectNames))
  {
      # Execute server-side list extraction
      DSI::datashield.assign(conns = DSConnections,
                             symbol = unname(ObjectNames[i]),
                             value = call("ExtractFromListDS",
                                           ListName.S = OutputName,
                                           ObjectName.S = names(ObjectNames)[i]))

      if (RunAssignmentChecks == TRUE)
      {
          # Call helper function to check if object assignment succeeded
          Messages$Assignment <- c(Messages$Assignment,
                                   ds.GetObjectStatus(ObjectName = unname(ObjectNames[i]),
                                                      DSConnections = DSConnections))
      }
  }



# Optionally unpack (unlist) CCP.AugmentedDataSet
#-------------------------------------------------------------------------------
  if (UnpackAugmentedDataSet == TRUE)
  {
      # Define ADS table names
      CCPTableNames.ADS <- c("Patient", "Diagnosis", "Therapy", "DiseaseCourse", "Events")

      for(i in 1:length(CCPTableNames.ADS))
      {
          # Execute server-side assign function
          DSI::datashield.assign(conns = DSConnections,
                                 symbol = paste0("CCP.ADS.", CCPTableNames.ADS[i]),      # E.g. 'CCP.ADS.Events'
                                 value = call("ExtractFromListDS",
                                              ListName.S = "CCP.AugmentedDataSet",
                                              ObjectName.S = CCPTableNames.ADS[i]))

          if (RunAssignmentChecks == TRUE)
          {
              # Call helper function to check if object assignment succeeded
              Messages$Assignment <- c(Messages$Assignment,
                                       ds.GetObjectStatus(ObjectName = paste0("CCP.ADS.", CCPTableNames.ADS[i]),
                                                          DSConnections = DSConnections))
          }
      }
  }

  if (RunAssignmentChecks == TRUE)
  {
      # Turn list into (named) vector
      Messages$Assignment <- purrr::list_c(Messages$Assignment)

      # Add topic element to start of vector
      Messages$Assignment <- c(Topic = "Object assignment on servers",
                               Messages$Assignment)
  }


  # 3) Get Messages object from servers (as a list of lists) and create completion check object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  AugmentationMessages <- DSI::datashield.aggregate(conns = DSConnections,
                                                    expr = call("GetReportingObjectDS",
                                                                ObjectName.S = "Messages"))

  # Create table object for output
  AugmentationCompletionCheck <- AugmentationMessages %>%
                                      map(\(ServerMessages) tibble(CheckAugmentationCompletion = ServerMessages$CheckAugmentationCompletion) ) %>%
                                      list_rbind(names_to = "ServerName")

  # Create vector of messages informing about Augmentation completion
  Messages$AugmentationCompletion <- AugmentationMessages %>%
                                          imap(function(ServerMessages, servername)
                                               {
                                                  case_when(ServerMessages$CheckAugmentationCompletion == "green" ~ dsFredaClient::MakeFunctionMessage(Text = paste0("Augmentation on server '", servername, "' performed successfully!"),
                                                                                                                                                       IsClassSuccess = TRUE),
                                                            ServerMessages$CheckAugmentationCompletion == "yellow" ~ dsFredaClient::MakeFunctionMessage(Text = paste0("Augmentation on server '", servername, "' performed with warnings!"),
                                                                                                                                                        IsClassWarning = TRUE),
                                                            ServerMessages$CheckAugmentationCompletion == "red" ~ dsFredaClient::MakeFunctionMessage(Text = paste0("Augmentation on server '", servername, "' could not be performed!"),
                                                                                                                                                     IsClassFailure = TRUE),
                                                            TRUE ~ dsFredaClient::MakeFunctionMessage(Text = paste0("Augmentation on server '", servername, "' could not be assessed."),
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

  # Invisibly return Messages and Augmentation completion check object
  invisible(list(Messages = Messages,
                 AugmentationCompletionCheck = AugmentationCompletionCheck))
}
