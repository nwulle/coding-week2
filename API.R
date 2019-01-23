install.packages("httr")
library("httr")
library(sf)

url<-"https://api-adresse.data.gouv.fr/search/"

query<-GET(url,query=list(q="13, rue de l'Université, Paris"))
query
geojson<-content(query, as = "text")
adresse<-read_sf(geojson)
mapview(adresse)

#static map

tm_shape(adresse)+
  tm_dots()

#zapier for contecting things. Send me an email, when you receive a mail. Autmating things and communicating together

