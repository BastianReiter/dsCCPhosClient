
#' ds.ConnectToCCP
#'
#' Takes credentials of CCP sites and returns a list of DSConnection-objects. Has to be executed from within a CCP bridgehead R server session.
#'
#' @param CCPSiteCredentials data.frame or tibble | Data on CCP site credentials
#'
#' @return A list of DSConnection-objects
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.ConnectToCCP <- function(CCPSiteCredentials)
{
    require(DSI)
    require(DSOpal)

    # Beam settings
    pkgconfig::set_config_in(Options = httr::use_proxy(url = "http://beam-connect", port = 8062), .in = parent.frame())
    pkgconfig::set_config_in(Options = httr::config(ssl_verifyhost = 0L, ssl_verifypeer = 0L), .in = parent.frame())

    # Create an environment
    LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

    # Append credentials of participating Sites
    for (i in 1:nrow(CCPSiteCredentials))
    {
        LoginBuilder$append(server = CCPSiteCredentials$SiteName[i],
                            url = CCPSiteCredentials$URL[i],
                            token = CCPSiteCredentials$Token[i])
    }

    # Returns a data frame of login data to different Sites
    LoginData <- LoginBuilder$build()

    # Get list of DSConnection objects of all servers
    CCPConnections <- DSI::datashield.login(logins = LoginData,
                                            assign = TRUE)

    return(CCPConnections)
}
