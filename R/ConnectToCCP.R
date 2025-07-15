
#' ConnectToCCP
#'
#' Takes credentials of CCP sites and returns a list of DSConnection-objects. Has to be executed from within a CCP bridgehead R server session.
#'
#' @param CCPSiteSpecifications \code{data.frame} - Login data of CCP sites
#'
#' @return A list of \code{DSConnection}-objects
#' @export
#'
#' @author Bastian Reiter
ConnectToCCP <- function(CCPSiteSpecifications,
                         proxyurl = "http://localhost")
{
    require(DSI)
    require(DSOpal)
    require(httr)

    # Beam settings
    httr::set_config(httr::use_proxy(url = proxyurl, port = 8062))
    httr::set_config(httr::config(ssl_verifyhost = 0L, ssl_verifypeer = 0L))

    # Create an environment
    LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

    # Append credentials of participating Sites
    for (i in 1:nrow(CCPSiteSpecifications))
    {
        LoginBuilder$append(server = CCPSiteSpecifications$SiteName[i],
                            url = CCPSiteSpecifications$URL[i],
                            token = CCPSiteSpecifications$Token[i])
    }

    # Returns a data frame of login data to CCP Sites
    LoginData <- LoginBuilder$build()

    # Perform login process and get list of DSConnection objects of all servers
    CCPConnections <- DSI::datashield.login(logins = LoginData,
                                            assign = TRUE,
                                            failSafe = TRUE)

    # Return DSConnection objects
    return(CCPConnections)
}
