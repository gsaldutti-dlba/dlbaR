arcgis_post_query <- function(df,
                              out_fields,
                              returnGeometry=F,
                              query_field,
                              url) {

  #used for large IN Queries (greater than 50)


  if (nrow(df<2001)) {

    url$query <- list(f='pjson',
                      outFields=paste0(out_fields),
                      returnGeometry=tolower(as.character(returnGeometry)))
    url = build_url(url)
    headers = c(
      'Content-Type' = 'application/x-www-form-urlencoded',
    )

    body = list(where=
                  sprintf("%s IN ('%s')"),
                query_field,
                paste0(unlist(df$field),collapse="','"))


    res <- POST(url,add_headers(headers),
                encode='form',
                body = body)

    df_return <- jsonlite::fromJSON(content(res))$features$attributes

    return(df_return)


  } else {

    splits <- split(df, ceiling(seq(nrow(df)/2000)))

    return <- list()
    j <- 0
    for(i in 1:length(splits)) {

      body <-list(where=
                      sprintf("%s IN ('%s')"),
                    query_field,
                    paste0(unlist(splits[[i]]$query_field),collapse="','"))

      print(i, j)
      query <- POST(url, encode="form",                      # this will set the header for you
                    #body=list(file=upload_file("example.txt")),   # this is how to upload files
                    body=body)


      df_return <- content(res)$features$attributes

      # names(response) <- NULL
      #
      # response <- tidyr::unnest(as.data.frame(do.call(rbind, response)),cols=all_of(fields))
      #
      # return[[i]] <- response


      j <- j+2000


    }
  }
}
