# Vol_qry function

vol_qry <- function(id, from, to) {
  query <- glue::glue(
    '{
      trafficData(trafficRegistrationPointId: "[id]") {
        volume {
          byHour(from: "[from]", to: "[to]") {
            edges {
              node {
                from
                to
                total {
                  volumeNumbers {
                    volume
                  }
                }
              }
            }
          }
        }
      }
    }',
    .open = "[",
    .close = "]"
  )
  return(query)
}
