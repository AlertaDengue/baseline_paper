# install.packages('rsconnect')
library(rsconnect)

# exporta todas as raízes confiáveis do System keychain
# system("security find-certificate -a -p /Library/Keychains/System.keychain > ~/certs.pem")

# aponta o curl do R para esse bundle
# Sys.setenv(CURL_CA_BUNDLE = "~/certs.pem")

rsconnect::setAccountInfo(name='danielcamara',
                          token='2DEB182DB5BE6165F9DE7B917A1164B6',
                          secret='geR64/B1YJbifH4KVSKNQaZcpXJ7a/2mXbZNe0yp')

rsconnect::deployApp(
  appDir = here::here(), 
  appName = "dashboard_teste",
  account = "danielcamara"
)
