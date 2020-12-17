# 01 load data ----

# 01.1 Bings ----
# downloaded in two chunks. load each and then bind_rows() 

bings_1 <- readxl::read_xlsx(here::here('data', 'bings_010119_070119.xlsx')) %>% 
  janitor::clean_names()
bings_2 <- readxl::read_xlsx(here::here('data', 'bings_070119_010120.xlsx')) %>% 
  janitor::clean_names()

# tail(pc_usgs_1)
# head(pc_usgs_2)

bings_big <- bind_rows(bings_1, bings_2) 
rm(bings_1, bings_2) # remove chunks

bings <- bings_big %>% 
  dplyr::select(date_time_edt, navd88_water_level_m) %>% 
  dplyr::mutate(level = as.numeric(navd88_water_level_m)) %>% 
  dplyr::rename(datetime = date_time_edt) %>% 
  dplyr::select(-navd88_water_level_m) %>% 
  tibble::add_column(site = "bings") %>% 
  dplyr::filter(level < 3.676) # something happened on 2019-01-04 and so filtering this out

# qaqc check for strange data
# this graphing effort resulted in the filtering in previous code chunk

a <- bings %>% 
  ggplot(aes(x = datetime, y = level)) +
  geom_point() +
  geom_line()

ggplotly(a)
rm(a, bings_big)

# 01.2 SWMP ----
# !!! path will need to be edited, this is an artifact of the SWMPr package

path <- "C:/Users/Dunnigan_S/Desktop/OPEN_PROJECTS/pcwaterlevels/data"

swmp <- SWMPr::import_local(path, 'gtmpcwq') %>% 
  SWMPr::qaqc(qaqc_keep = c('0', '1', '3')) %>% 
  dplyr::select(datetimestamp, cdepth) %>% 
  dplyr::rename(datetime = datetimestamp) %>% 
  tibble::add_column(site = "swmp")

# qaqc check for strange data


b <- swmp %>% 
  ggplot(aes(x = datetime, y = cdepth)) +
  geom_point() +
  geom_line()

ggplotly(b)
rm(b, path)

# 01.3 USGS ----

usgs <- readxl::read_xlsx(here::here('data', 'pc_usgs_010119_010120.xlsx')) 

usgs <- usgs %>% 
  dplyr::select(datetime, height) %>%
  tibble::add_column(site = "usgs") %>% 
  dplyr::mutate(height_m = height * 0.3048) %>% # gage height is in ft, need to convert to meters
  dplyr::select(-height)

# qaqc check for strange data

c <- usgs %>% 
  ggplot(aes(x = datetime, y = height_m)) +
  geom_point() +
  geom_line()

ggplotly(c)

rm(a,b,c)

# 01.4 tides ----

tides <- readxl::read_xlsx(here::here('data', 'fm_noaa_tides_2019.xlsx')) %>% 
  janitor::clean_names()

tides %>% 
  dplyr::select(-day, -time) %>% 
  dplyr::mutate(pred_m = pred_cm /100)

# 04 combine all data ----

dat <- swmp %>%
  dplyr::rename(height_m = cdepth) %>%
  dplyr::bind_rows(usgs) %>% 
  dplyr::rename(level = height_m) %>% 
  dplyr::bind_rows(bings) %>% 
  dplyr::mutate(month = lubridate::month(datetime),
                day = lubridate::day(datetime),
                date = lubridate::date(datetime))

# 05 summary statistics ----

daily <- dat %>% 
  dplyr::group_by(site, month, day) %>% 
  dplyr::summarise(min = min(level, na.rm = TRUE),
                   max = max(level, na.rm = TRUE),
                   range = max(level, na.rm = TRUE) - min(level, na.rm = TRUE),
                   mean = mean(level, na.rm = TRUE),
                   sd = sd(level, na.rm = TRUE),
                   IQR = IQR(level, na.rm = TRUE),
                   mad = mad(level, na.rm = TRUE)) %>% 
  ungroup()


monthly <- dat %>% 
  dplyr::group_by(site, month) %>% 
  dplyr::summarise(min = min(level, na.rm = TRUE),
                   max = max(level, na.rm = TRUE),
                   range = max(level, na.rm = TRUE) - min(level, na.rm = TRUE),
                   mean = mean(level, na.rm = TRUE),
                   sd = sd(level, na.rm = TRUE),
                   IQR = IQR(level, na.rm = TRUE),
                   mad = mad(level, na.rm = TRUE)) %>% 
  ungroup()

yearly_combo <- dat %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarise(min = min(level, na.rm = TRUE),
                   max = max(level, na.rm = TRUE),
                   range = max(level, na.rm = TRUE) - min(level, na.rm = TRUE),
                   mean = mean(level, na.rm = TRUE),
                   sd = sd(level, na.rm = TRUE),
                   IQR = IQR(level, na.rm = TRUE),
                   mad = mad(level, na.rm = TRUE)) %>% 
  ungroup()

yearly_daily <- daily %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarise() %>% 
  ungroup()

yearly %>% 
  ggplot(aes(x = site, y = range)) +
  geom_point(size = 3) 