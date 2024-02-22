
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   CCPhosApp package internal functions
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



#' PrintMessages
#'
#' Take list of messages, add symbols and print them on console
#'
#' @param Text \code{list} List of Sublists of named vectors
#' @param IsClassInfo \code{logical}
#' @param IsClassSuccess \code{logical}
#' @param IsClassWarning \code{logical}
#' @param IsClassFailure \code{logical}
#' @keywords internal
#' @export
PrintMessages <- function(Messages)
{
    purrr::walk(.x = Messages,
                .f = function(Sublist)
                     {
                          for (i in 1:length(Sublist))      # for-loop instead of nested purrr::walk because of the latter seems to address Sublist[[i]] instead of Sublist[i]
                          {
                              message <- Sublist[i]
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

                          cat("\n")
                     })
}

