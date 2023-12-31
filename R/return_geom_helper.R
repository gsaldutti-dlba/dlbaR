
return_geoms_helper <- function(query, out_fields) {

  #get resposne as list
  features_json <- content(query)$features

  #initiate empty list
  geometries <- list()

  #loop
  for(i in 1:length(features_json)) {
    #for geometry in feature
    x <- features_json[[i]]$geometry$rings

    #for polygon in geometry
    for(j in 1:length(x)) {

      #get points
      points <- lapply(seq_along(x[[j]]),
                       function(k) {
                         #convert poitn to sf object
                         sf::st_point(unlist(x[[j]][k]))
                       })

      #if multipolyogn
      if(length(x) > 1) {

      } else {

        #create polygon out of points
        poly <-  points%>% sf::st_as_sfc(crs=4326) %>% sf::st_combine() %>%sf::st_cast('POLYGON')
      }
    }
    #append to list
    geometries[[i]] <- poly
  }

  #bind geometries
  geometries <- do.call(rbind, geometries)
  #convert to sf column
  geometries <- geometries%>%sf::st_sfc()

  #create df out of features
  df <- lapply(features_json, function(x) (as.vector(unlist(x$attributes))))
  #create df
  df <- as.data.frame(do.call(rbind,df), col.names=out_fields)

  colnames(df) <- out_fields

  #bind to geometries
  polys <- cbind(df, geometries)
  #convert to spatial object
  df <- polys %>% sf::st_as_sf(sf_column_name="geometry")

  return(df)
}
