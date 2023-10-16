# Check if column names match the expected names.
test_stations_metadata_colnames <- function(df) {
  expected_colnames <- c("id", "name", "latestData", "lat", "lon")
  if (all(colnames(df) == expected_colnames) == TRUE) {
    print("PASS: Data has the correct columns")
  } else {
    print("FAIL: Columns do not match the correct specification")
  }
}

# Validate row count is between 5,000 and 10,000.
test_stations_metadata_nrows <- function(df) {
  min_expected_rows <- 5000
  max_expected_rows <- 10000
  if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
    print("PASS: Data has a reasonable number of rows")
  } else if (nrow(df) <= min_expected_rows) {
    print("FAIL: Data has suspiciously few rows")
  } else {
    print("FAIL: Data has suspiciously many rows")
  }
}

# Ensure columns have the expected data types.
test_stations_metadata_coltypes <- function(df) {
  expected_coltypes <- c("character", "character", "double", "double", "double")
  if (all(sapply(df, typeof) == expected_coltypes) == TRUE) {
    print("PASS: All cols have the correct specifications")
  } else {
    print("FAIL: Columns do not have the correct specification")
  }
}

# Check if data has more than 200 missing values.
test_stations_metadata_nmissing <- function(df) {
  max_miss_vals <- 200
  if (sum(sapply(df, function(col) sum(is.na(col)))) < max_miss_vals) {
    print("PASS: Amount of missing values is reasonable")
  } else {
    print("FAIL: Too many missing values in data set")
  }
}

# Verify time column is in UTC time zone.
test_stations_metadata_latestdata_timezone <- function(df) {
  if (attr(df$latestData,"tzone") == "UTC") {
    print("PASS: latestData has UTC-time zone")
  } else {
    print("FAIL: latestData does not have expected UTC-time zone")
  }
}

# Combine and run all the above tests.
test_stations_metadata <- function(df) {
  test_stations_metadata_colnames(df)
  test_stations_metadata_coltypes(df)
  test_stations_metadata_nmissing(df)
  test_stations_metadata_nrows(df)
  test_stations_metadata_latestdata_timezone(df)
}
