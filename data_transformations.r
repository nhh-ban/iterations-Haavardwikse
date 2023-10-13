# functions/data_transformations.r

transform_metadata_to_df <- function(data) {
  # Assuming data is a list with nested structure
  df <- tibble(
    id = sapply(data, function(x) x$id),
    name = sapply(data, function(x) x$name),
    latestData = as.POSIXct(sapply(data, function(x) x$latestData), origin="1970-01-01", tz="UTC"),
    lat = sapply(data, function(x) x$location$lat),
    lon = sapply(data, function(x) x$location$lon)
  )
  
  return(df)
}
