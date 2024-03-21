
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

