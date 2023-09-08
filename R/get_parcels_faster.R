library(httr)
library(dplyr)

#
#not set up to return geoms yet
query_parcels_faster <- function(returnGeom=F, fields=c()) {


  fields_string <- paste0(fields, collapse=",")

  returnGeom <- tolower(as.character(returnGeom))

  url <- "https://services2.arcgis.com/qvkbeam7Wirps6zC/arcgis/rest/services/Parcels_Current/FeatureServer/0/query"

  ids <- GET(url,
             query=list(
               where='1=1',
               returnIdsOnly='true',
               f='pjson'
             )) %>%
    content()

  ids <- ids[[2]]

  splits <- split(ids, ceiling(seq_along(ids)/2000))



  a <- lapply(splits,function(x){
    response <- POST(url, encode="form",                      # this will set the header for you
            #body=list(file=upload_file("example.txt")),   # this is how to upload files
            body=list(
              objectIds=paste0(unlist(x),collapse=','),
              returnGeometry=returnGeom,
              outFields=fields_string,
              f='pjson',
              returnExceededLimitFeatures="true"
              ))  %>%
    content()%>%
    .$features %>%
      unlist(recursive=F)

    names(response) <- NULL
    response <- tidyr::unnest(as.data.frame(do.call(rbind, response)),cols=fields)
    }

  )

  a <- do.call(rbind, a)

}

