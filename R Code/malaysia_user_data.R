# Script to download user data in Malaysia

library(rapidpror)

# put your token under "key"
key <- read.table("tokens/PT_malaysia_key.txt", quote="\"", comment.char="")

site <- "https://app.rapidpro.io/api/v2/"

set_rapidpro_site(site = site)
set_rapidpro_key(key = key[[1]])

contacts_unflat <- get_user_data(flatten = FALSE, date_from = NULL, call_type = "contacts.json")
