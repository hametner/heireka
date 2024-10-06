

getPatientsMap <- function(demog = demog){
# install.packages("ggmap")
require(ggmap)
api_key_google <- "AIzaSyALQnjPYcggAqDusskhvhjEpZkezmQghEE"
ggmap::register_google(api_key_google, account_type = "standard")

demog <- demog %>% 
        mutate(adr_full = paste0(`Straße/Nr.`,", ",PLZ," ",Ort,", ",Land)) %>% 
        mutate(case_id = parse_number(as.character(Fall)))

df <- case_id %>% drop_na() %>% 
        left_join(demog, by = "case_id") %>% group_by(case_id) %>% slice(1L) %>% ungroup %>% unique() %>% 
        drop_na(adr_full) %>% 
        mutate(coordinates = geocode(adr_full))

dump <- df %>% select(-contains("Prüfz"))
write.csv2(dump, "output/demog_incl_coordinates.csv")

# patients coordinates
map_data <- df %>% select(case_id, coordinates) %>% unnest(cols = c(coordinates))
write.csv(map_data, "output/shiny_map/data.csv")

# hospital coordinates
hosp_adr <- "Im Neuenheimer Feld 400, 69120 Heidelberg"
hosp_coord <- geocode(hosp_adr)
write.csv(hosp_coord, "output/shiny_map/hosp.csv")
        

#test
# adr <- demog$adr_full[30]
# coordinates <- geocode(adr)



}