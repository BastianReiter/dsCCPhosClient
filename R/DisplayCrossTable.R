
#' DisplayCrossTable
#'
#' Transform CrossTab data output by \code{dsFredaClient::ds.GetCrossTab()} into display-friendly format.
#'
#' @param CrossTabData \code{data.frame} - Containing data output by \code{dsFredaClient::ds.GetCrossTab()}
#' @param FeatureX \code{symbol} or \code{string} - Feature in 'TimeSeriesData' that contains time points (e.g. years)
#' @param FeatureY \code{symbol} or \code{string} - Feature in 'TimeSeriesData' that contains values
#' @param IncludeMissingTimePoints \code{logical} - Indicates whether discrete time points (e.g. years) with no data should be displayed in the time series - Default: \code{FALSE}
#'
#' @return A \code{data.frame} displaying time series data in horizontal form
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DisplayCrossTable <- function(CrossTabData,
                              FeatureX,
                              FeatureY,
                              IncludeMissingTimePoints = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- For Testing Purposes ---
  CrossTabData <- TestCrossTab$CrossTab$All
  FeatureX <- "Sex"
  FeatureY <- "LastVitalStatus"

  # --- Argument Validation ---
  assert_that(is.data.frame(CrossTabData),
              is.string(FeatureX) | is.symbol(FeatureX),
              is.string(FeatureY) | is.symbol(FeatureY),
              is.flag(IncludeMissingTimePoints))

#-------------------------------------------------------------------------------

  CrossTabData <- CrossTabData %>%
                        select({{ FeatureX }},
                               {{ FeatureY }},
                               JointCount,
                               JointRelFreq,
                               starts_with("MargCount"),
                               starts_with("MargRelFreq"))

  RoundSensibly <- function(value)
  {
      if (value > 0 & value < 0.005) { return("<1%") }
      else if (value < 1 & value >= 0.995) { return(">99%") }
      else { return(paste0(round(value * 100, digits = 0), "%")) }
  }

  Test1 <- CrossTabData %>%
                rowwise() %>%
                mutate(Joint = paste0(JointCount, " (", RoundSensibly(JointRelFreq), ")"),
                       !!sym(paste0("Marg.", FeatureX)) := paste0(!!sym(paste0("MargCount.", FeatureX)), " (", RoundSensibly(!!sym(paste0("MargRelFreq.", FeatureX))), ")"),
                       !!sym(paste0("Marg.", FeatureY)) := paste0(!!sym(paste0("MargCount.", FeatureY)), " (", RoundSensibly(!!sym(paste0("MargRelFreq.", FeatureY))), ")")) %>%
                select({{ FeatureX }},
                       {{ FeatureY }},
                       Joint,
                       starts_with("Marg."))


  Test2 <- Test1 %>%
              pivot_longer(cols = c(- {{ FeatureX }},
                                    - {{ FeatureY }}),
                           names_to = "ValueType",
                           values_to = "Value")







  # TimeSeriesTable <- TimeSeriesData %>%
  #                       pivot_wider(names_from = {{ TimePointFeature }},
  #                                   values_from = {{ ValueFeature }})
  #
  # if (IncludeMissingTimePoints == TRUE)
  # {
  #     AvailableYears <- as.integer(names(TimeSeriesTable)[-1])
  #     TimeSpan <- min(AvailableYears):max(AvailableYears)
  #     MissingYears <- TimeSpan[!(TimeSpan %in% AvailableYears)]
  #
  #     TimeSeriesTable <- TimeSeriesTable %>%
  #                             mutate(!!! setNames(rep(list(NA), length(MissingYears)), MissingYears)) %>%      # Add empty columns with missing years as column names
  #                             select({{ GroupingFeature }},
  #                                    all_of(as.character(TimeSpan)))
  # }

#-------------------------------------------------------------------------------
  return(Test2)
}
