
#-------------------------------------------------------------------------------
LocalPackageUpdate <- function(PackageNames = c("dsCCPhosClient",
                                                "dsFredaClient",
                                                "dsCCPhos",
                                                "dsFreda"),
                               Mode = "load")
#-------------------------------------------------------------------------------
{
  require(devtools)

  # Unload all packages first
  for (packagename in PackageNames)
  {
      if (packagename %in% loaded_packages()$package)
      {
          devtools::unload(package = packagename)
      }
  }

  # If Mode is 'load' only use load_all() to (re-)load all functions from chosen local packages
  if (Mode == "load")
  {
      for (packagename in rev(PackageNames))
      {
          devtools::load_all(path = paste0("../", packagename))
      }
  }

  # If Mode is 'install' (re-)install all chosen local packages
  if (Mode == "install")
  {
      for (packagename in rev(PackageNames))
      {
          devtools::install(pkg = paste0("../", packagename),
                            upgrade = "never")
      }
  }
}
#-------------------------------------------------------------------------------
