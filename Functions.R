
get_user_data <- function(token, call_type="contacts.json", rapidpro_site = "https://rapidpro.idems.international/api/v2/"){
  source_1 <- paste(rapidpro_site, call_type, sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  user_result <- results$results
  return(user_result)
}

get_flow_data <- function(uuid_data,token, flow_name, result, rapidpro_site = "https://rapidpro.idems.international/api/v2/", call_type="runs.json?flow=" ){
  uuid_flow <- uuid_data[which(uuid_data == flow_name),]  
  source_1 <- paste(rapidpro_site, call_type, uuid_flow[2], sep = "")
  response <- httr::GET(source_1, config = httr::add_headers(Authorization = paste("Token", token)))
  raw <- httr::content(response, as = "text")
  results <- jsonlite::fromJSON(raw)
  result_flow<-results$results
  uuid <- result_flow$contact$uuid
  response <- result_flow$responded
  category <- result_flow$values$result$category            
  flow_interaction <- tibble::tibble(uuid, response, category)
  flow_interaction <- flow_interaction %>% mutate(flow_type = uuid_flow[1])
  return(flow_interaction)
}

response_rate_graphs<-function(flow_interaction, flow_name){
 print(flow_interaction %>% group_by(response) %>% summarise(n())) 
 print(flow_interaction %>% group_by(category) %>% summarise(n())) 
  
  ggplot(flow_interaction, aes(x = response)) +
    geom_bar() +
    labs(x = "Response", y = "Frequency", title = paste(flow_name ," - Response"))
  
  flow_interaction_response <- flow_interaction %>% filter(response == TRUE)
  ggplot(flow_interaction_response, aes(x = category)) +
    geom_bar() +
    labs(x = "Response", y = "Frequency", title = paste(flow_name ," - Response"))
  
}

create_user_dataframe <- function(flow_interaction){
  temp<-flow_interaction %>% group_by(uuid,response) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
  temp2<-flow_interaction %>% filter(response == TRUE) %>% group_by(uuid,category) %>% summarise(n=n()) %>% mutate(freq=100*n/sum(n))
}