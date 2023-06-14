library(leaflet)
library(tidyverse)
library(tidygeocoder)
library(sf)
library(mapview)


map <- leaflet()
map


filename <- "fdi_provinces_vi.csv"
df <- read.csv(filename)

columns <- colnames(df)
for (i in 3:9) {
  df <- df %>% mutate(!!columns[i] := as.numeric(!!sym(columns[i])))
}
df[is.na(df)] <- 0
head(df)

df_2015 = filter(df, df$Năm == 2015)

map_2015 = data.frame(Đối.tác = df_2015$Đối.tác, Số.dự.án = df_2015$Số.dự.án.cấp.mới)
map_2015_tbl <- map_2015 %>%
  tidygeocoder::geocode(
    address = Đối.tác,
    method = "osm"
  )
map_2015_tbl
 
map <- leaflet() %>% addTiles() %>% setView(108.219955, 16.047199, zoom = 6) %>%
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad") %>%
  addCircleMarkers(
    data = map_2015_tbl,
    lng = ~long,
    lat= ~lat,
    color = "red",
    radius = ~sqrt(Số.dự.án)*2,
    label = ~Số.dự.án
    )
map