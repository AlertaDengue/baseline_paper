# Auxiliar functions 
# Leo Bastos

get_cases <- function(  
    start_date=today()-11*7, 
    end_date = today(), 
    disease = 'dengue',
    uf=NULL, # RJ
    muncode=NULL, # 3304557
    per_page = 100
){
  
  infodengue_api <- "https://api.mosqlimate.org/api/datastore/infodengue/"
  page <- "1"
  pagination <- paste0("?page=", page, "&per_page=", per_page, "&")
  
  
  filters <- paste0("disease=", disease, "&start=", start_date, 
                    "&end=", end_date)
  
  filters.extra <- case_when(
    !is.null(muncode) ~  paste0("&geocode=", muncode),
    (is.null(muncode) & !is.null(uf)) ~ paste0("&uf=", uf),
    TRUE ~ NA
  )
  
  if(!is.na(filters.extra)){
    filters <- paste0(filters, filters.extra)
  }
  
  url <- paste0(infodengue_api, pagination, filters)
  
  resp <- GET(url)
  
  content <- content(resp, "text")
  json_content <- fromJSON(content)
  items <- json_content$items
  
  pagination_data <- json_content$pagination
  
  PAGES <- pagination_data$total_pages
  
  if(PAGES>1){
    
    for(k in 2:PAGES){
      
      pagination <- paste0("?page=", k, "&per_page=", per_page, "&")
      
      json_content <- paste0(
        infodengue_api, 
        pagination, 
        filters) |> 
        GET() |> 
        content("text") |> 
        fromJSON() 
      
      items.k <- json_content$items
      items <- bind_rows(items, items.k)
    }
    
  }
  
  items
}
