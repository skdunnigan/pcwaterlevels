# load data

# USGS
# downloaded in two chunks. load each and then bind_rows() 

pc_usgs_1 <- readxl::read_xlsx(here::here('data', 'bings_010119_070119.xlsx')) %>% 
  janitor::clean_names()
pc_usgs_2 <- readxl::read_xlsx(here::here('data', 'bings_070119_010120.xlsx')) %>% 
  janitor::clean_names()

tail(pc_usgs_1)
head(pc_usgs_2)

pc_usgs <- bind_rows(pc_usgs_1, pc_usgs_2)

rm(pc_usgs_1, pc_usgs_2) # remove chunks
