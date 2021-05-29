movie_info <- function(query = '', start_date = '', end_date = '', key) {
  url <- paste(
    'https://api.nytimes.com/svc/movies/v2/reviews/search.json?query=', query,
    '&opening-date=', start_date,
    ';', end_date,
    '&api-key=', key, 
    '&order=by-opening-date',
    '&offset=', sep = ''
  )
  
  i = 0
  out <- data.table(matrix(ncol = 4, nrow = 0))
  names(out) <- c('name', 'opening', 'summary', 'url')
  
  repeat {
    url_offset <- paste(url, 20 * i, sep = '')
    query <- GET(url_offset)
    dat <- query$content %>% rawToChar %>% fromJSON
    
    if(query$status_code %in% c(429, 400)) break # Query limit exceeded
    if(dat$num_results == 0) break # No results returned
    
    out <- rbind(out, data.table(
      name = dat$results$display_title,
      opening = dat$results$opening_date,
      summary = dat$results$summary_short,
      url = dat$results$link$url
    ))
    
    i = i + 1
  }
  
  return(out)
}

show_review <- function(mi, i) 
  system(paste(
    '"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"',
    mi$url[i]
  ))
    
say_summary <- function(mi, i) {
  system(paste(
    'say', mi$summary[i]
  ))
}