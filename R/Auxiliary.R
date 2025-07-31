
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   dsCCPhosClient internal auxiliary functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' CheckArguments
#'
CheckArguments <- function()
{
    if (!(is.character(TableName) & is.character(MetricFeatureName)))
    {
        stop("Error: Arguments 'TableName' and 'MetricFeatureName' must be character strings.", call. = FALSE)
    }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' CheckDSConnections
#'
#' This function checks if 'DSConnections' that are passed to a DataSHIELD client function are valid. If this object is not passed, the function will try to find an existing \code{list} programmatically (using \code{DSI} functionality) and return it.
#'
#' @param DSConnections Usually a \code{list} of \code{DSConnection} class objects. If this is set to \code{NULL} this function will try to find an existing \code{list} programmatically and return it.
#' @keywords internal
#' @export
CheckDSConnections <- function(DSConnections)
{
    require(DSI)

    # If no DataSHIELD connections are specified, assign them programmatically
    if (is.null(DSConnections))
    {
        DSConnections <- datashield.connections_find()
    }

    # If 'DSConnections' is not valid, throw an error message
    if (!(is.list(DSConnections) && all(unlist(lapply(DSConnections, function(d) { methods::is(d,"DSConnection") })))))
    {
        stop("'DSConnections' must be a list of DSConnection-class objects", call. = FALSE)
    }

    return(DSConnections)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' MakeFunctionMessage
#'
#' Turn function message into named vector to enable classification of feedback
#'
#' @param Text \code{character}
#' @param IsClassInfo \code{logical}
#' @param IsClassSuccess \code{logical}
#' @param IsClassWarning \code{logical}
#' @param IsClassFailure \code{logical}
#' @keywords internal
#' @export
MakeFunctionMessage <- function(Text,
                                IsClassInfo = FALSE,
                                IsClassSuccess = FALSE,
                                IsClassWarning = FALSE,
                                IsClassFailure = FALSE)
{
    Name <- dplyr::case_when(IsClassFailure ~ "Failure",
                             IsClassWarning ~ "Warning",
                             IsClassSuccess ~ "Success",
                             IsClassInfo ~ "Info",
                             TRUE ~ "None")

    Message <- stats::setNames(object = Text,
                               nm = Name)

    return(Message)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' PrintMessages
#'
#' Take list of messages, add symbols and print them on console
#'
#' @param Text \code{list} List of of named vectors
#' @param IsClassInfo \code{logical}
#' @param IsClassSuccess \code{logical}
#' @param IsClassWarning \code{logical}
#' @param IsClassFailure \code{logical}
#' @keywords internal
#' @export
PrintMessages <- function(Messages)
{
    require(dsCCPhosClient)
    require(stringr)

    purrr::walk(.x = Messages,
                .f = function(Subvector)      # List of messages contains named vectors that serve as 'topic-specific messages'
                     {
                          cat("\n")

                          for (i in 1:length(Subvector))      # for-loop instead of nested purrr::walk because items in list are vectors
                          {
                              message <- Subvector[i]

                              if (names(message) == "Topic")
                              {
                                  # Print topic string in bold letters (formatted with ANSI code \033...) and with horizontal line underneath
                                  cat("\033[1m", as.character(message), "\n", paste0(rep("~", times = stringr::str_length(as.character(message))), collapse = ""), "\033[0m", "\n", sep = "")
                              }
                              else
                              {
                                  cli::cat_bullet(as.character(message),
                                                  bullet = dplyr::case_when(names(message) == "Info" ~ "info",
                                                                            names(message) == "Success" ~ "tick",
                                                                            names(message) == "Warning" ~ "warning",
                                                                            names(message) == "Failure" ~ "cross",
                                                                            TRUE ~ "none"),
                                                  bullet_col = dplyr::case_when(names(message) == "Success" ~ CCPhosColors$Green,
                                                                                names(message) == "Warning" ~ CCPhosColors$Orange,
                                                                                names(message) == "Failure" ~ CCPhosColors$Red,
                                                                                TRUE ~ "black"))
                              }
                          }

                          cat("\n")
                     })
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

