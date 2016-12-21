# install.packages('rsconnect')
library(rsconnect)
rsconnect::setAccountInfo(name='datadigressions', 
  token='CEF577B25A8011BD1E31C4A8888136B2', 
  secret='O8PyHBUFOBXBRUqUpbACHtpbXb0uqkWOogFXijAx')


#To deploy shiny app
rsconnect::deployApp('Z:\\Personal\\ImpactRadius App\\ImpactRadius R Files\\shiny app')


# runApp("shiny app")





