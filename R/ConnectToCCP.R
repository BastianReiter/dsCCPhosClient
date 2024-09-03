
#' ConnectToCCP
#'
#' Takes credentials of CCP sites and returns a list of DSConnection-objects. Has to be executed from within a CCP bridgehead R server session.
#'
#' @param CCPSiteSpecifications \code{data.frame} | Login data of CCP sites
#'
#' @return A list of DSConnection-objects
#' @export
#'
#' @examples
#' @author Bastian Reiter
ConnectToCCP <- function(CCPSiteSpecifications)
{
    require(DSI)
    require(DSOpal)

    # Beam settings
    set_config(use_proxy(url = "http://beam-connect", port = 8062))
    set_config(config(ssl_verifyhost = 0L, ssl_verifypeer = 0L))

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

    # Trigger creation of vector holding info on processing checkpoints
    ds.CreateCheckpoints(DataSources = CCPConnections)

    # Return DSConnection objects
    return(CCPConnections)
}
