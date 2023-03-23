# Malaysia, Philippines - 3months
set_rapidpro_site(site = site)
set_rapidpro_key(key = key[[1]])
set_rapidpro_uuid_names()

# Get archived data initially
malaysia_archived <- get_archived_data(date_from = default_date_from,
                                       date_to = default_date_to, period = "none")
saveRDS(malaysia_archived, file = "malaysia_archived.RDS")

philippines_archived <- get_archived_data(date_from = default_date_from,
                                       date_to = default_date_to, period = "none")
saveRDS(philippines_archived, file = "philippines_archived.RDS")

# Update archived data

