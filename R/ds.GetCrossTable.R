
#' ds.GetCrossTab
#'
#' Get cross tab
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetCrossTabDS()}
#'
#' @param TableName \code{string} - Name of \code{data.frame} on server
#' @param FilterExpression \code{string} - \code{dplyr::filter} expression as string
#' @param GroupBy \code{string} - Optional \code{dplyr::group_by} expression as string
#' @param OutputName \code{string} - Name of resulting \code{data.frame} on server
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.GetCrossTab <- function(TableName,
                           FeatureNames,
                           DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(assertthat)
  require(DSI)
  require(purrr)
  require(tidyr)

  # --- For Testing Purposes ---
  TableName <- "ADS_Patient"
  FeatureNames <- c("Sex", "CountDiagnoses")
  DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

  # Argument assertions
  assert_that(is.string(TableName),
              is.character(FeatureNames))

#-------------------------------------------------------------------------------

  # Paste elements of 'FeatureNames' together and encode the resulting string to make it passable through DSI
  FeatureNamesString <- paste0(FeatureNames, collapse = ", ") %>%
                            .encode_tidy_eval(.get_encode_dictionary())

  # Get (maximum) number of unique values for selected features and multiply them to calculate the projected number of value combinations
  ProjectedCombinations <- sapply(FeatureNames, function(featurename)
                                                { ds.GetFeatureInfo(TableName = TableName,
                                                                    FeatureName = featurename) %>%
                                                      pull(CountUniqueValues) %>%
                                                      max(na.rm = TRUE)
                                                }) %>%
                              prod(na.rm = TRUE)

  # Prompt user input when projected number of value combinations is high
  if (ProjectedCombinations > 20)
  {
      UserResponse <- readline(prompt = paste0("The features you selected will result in a high number of different value combinations (",
                                               ProjectedCombinations,
                                               "). This can result in low absolute frequencies for certain combinations, which could increase disclosure potential.\n",
                                               "Do you want to continue? (y/n) "))

      if (!(UserResponse %in% c("Y", "y"))) { stop(call. = FALSE) }
  }


  # Call server-side function
  ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                             expr = call("GetCrossTabDS",
                                                         TableName.S = TableName,
                                                         FeatureNames.S = FeatureNamesString))


  CrossTab.Separate <- ServerReturns %>%
                            list_transpose(simplify = FALSE) %>%
                            pluck("CrossTab") %>%
                            list_rbind(names_to = "Server")


  CrossTab.Cumulated <- CrossTab.Separate %>%
                            group_by(across(all_of(FeatureNames))) %>%
                            summarize(Server = "All",
                                      Count = sum(Count, na.rm = TRUE),
                                      ServersBelowThreshold = sum(NBelowThreshold == TRUE)) %>%
                            as.data.frame()

  CrossTab <- CrossTab.Cumulated %>%
                    bind_rows(CrossTab.Separate)


#
#   MarginalCounts.Separate <- ServerReturns %>%
#                                   list_transpose(simplify = FALSE) %>%
#                                   pluck("MarginalCounts")
#
#
#
#   if (length(FeatureNames) == 2)
#   {
#       PrepareTable <- CrossTab.Cumulated %>%
#                           select(-Server,
#                                  -ServersBelowThreshold) %>%
#                           filter(if_all(all_of(FeatureNames), ~ !is.na(.)))
#
#
#       TableObject <- xtabs(formula = reformulate(termlabels = FeatureNames,
#                                                  response = "Count"),
#                            data = PrepareTable)
#
#       ChiSq <- chisq.test(x = TableObject)
#
#   }
#
#   # loglin()
#   if (length(FeatureNames) > 2)
#   {
#
#   }


  return(CrossTab)


}
