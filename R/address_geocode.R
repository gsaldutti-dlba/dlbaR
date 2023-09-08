# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

address_geocode <- function(addresses) {
  #column_vector: data frame column as vector with addresses
  #set base url
  base_url <- 'https://opengis.detroitmi.gov/opengis/rest/services/BaseUnits/BaseUnitGeocoderDev/GeocodeServer/geocodeAddresses/query?f=json'

  #set response variables
  format_out <- 'json'
  outSR <-'4326'
  outFields <-'*'

  #addresses <- data.frame(addresses)
  addresses <- as.list(add$address[1:3])
  #name lists
  names <- rep('SingleLine', length(addresses))
  names(addresses) <- names


  #create objectid list
  objectID <-1:length(addresses)
  ids <- rep('OBJECTID', length(objectID))
  names(objectID) <- ids
  #
  #address_df <- cbind(addresses,objectID)

  #names(address_df) <- c('SingleLine','OBJECTID'

  address_list <- list()
  for(i in 1:length(addresses)) {
    new_add <-c(addresses[[i]], objectID[[i]])
    new_add <- as.list(new_add)
    names(new_add) <- c('SingleLine','OBJECTID')
    add_list <- list(attributes=new_add)
    address_list<-rlist::list.append(address_list, add_list)
  }
  address_json <- jsonlite::toJSON(address_list,auto_unbox=TRUE)

  #create list for formatting
  jsonlist <- list(
    records = address_list
  )

  #format to json
  return_json <- jsonlite::toJSON(jsonlist, auto_unbox = TRUE)

  url_bytes=""
  url_bytes$query <- list(addresses=return_json,f='json',outFields='parcel_id')
  url_bytes <-gsub("?","",url_build(url_bytes))

  t<-httr::POST(base_url, body = return_json)

  httr::content(t)

  req <- request(base_url) %>%
    req_url_query(outFields="parcel_id",addresses=return_json,f='json') %>%
    req_headers(`Content-Length`=url_length) %>%
    req_method("POST") %>%
    req_perform()

  resp_body_json(req)




  f <- httr2::request(base_url) %>%
    req_url_query(f='json',
                  addresses=return_json)
  c_length <- nchar(f$url)


  g <- httr2::request(base_url) %>%
    req_method('POST') %>%
    req_url_query(f='json',
                  addresses=return_json) %>%
    req_headers(`Content-Length`=as.character(c_length),
                `Content-Type`=
                'application/x-www-form-urlencoded') %>%
    #req_dry_run()
    #req_body_json(return_json) %>%
    req_perform() #%>%
    resp_body_json()

  g <- httr2::GET()
  #
  # query=list(
  #   f=format_out,
  #   outSR=outSR,
  #   outFields=outFields,
  #   addresses=return_json
  # ),
  # # encode='json')
  # httr::add_headers(
  #   "Accept"="*/*",
  #   "Accept-Encoding"=
  #     "gzip, deflate, br",
  #   'Content-Type'='application/json'))

  response <- content(g)$locations

  results <- lapply(response, function(x){
    add <- x$attributes$ShortLabel
    match <- x$attributes$Score
    parcel_id <- x$attributes$parcel_id
    vec <- c(add,match,parcel_id)
  }
  )
  return(do.call(rbind,results))


}
