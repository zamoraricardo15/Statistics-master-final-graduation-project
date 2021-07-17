library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)

#Cargar hojas del archivo
diputados <- read.csv("01_08_CSV_POSITIVOS.csv", sep = ";",header = TRUE) 
head(diputados)

# #leer hojas del archivo
# read_excel_allsheets <- function(diputados) {
#   sheets <- readxl::excel_sheets(diputados)
#   x <-    lapply(sheets, function(X) readxl::read_excel(diputados, sheet = X))
#   names(x) <- sheets
#   x
# }
# 
# #crear lista con archivos
# mysheets <- read_excel_allsheets("diputados.xlsx")
# 
# #Unir bases
# evangelicos <- join_all(mysheets, by = "codigod", type = "left", match="all")
# 
# #reemplazar blancos con cero
# evangelicos[is.na(evangelicos)] <- 0
#diputados$evangelicos <- rowSums( diputados[,5:304] )
diputados$evangelicos <- diputados[,304]
evangelicos <- diputados

evangelicos <- evangelicos %>%
  mutate(tot14=X08.01.2021) %>%
  select(cod_provincia, provincia, cod_canton, canton)
# 
cantones <- diputados %>%
  group_by(evangelicos) %>%
  # summarise(validos14=sum(validos14),
  #           tot14=sum(tot14),
  #           validos10=sum(validos10),
  #           tot10=sum(tot10),
  #           validos06=sum(validos06),
  #           tot06=sum(tot06),
  #           validos02=sum(validos02),
  #           tot02=sum(tot02),
  #           validos98=sum(validos98),
  #           tot98=sum(tot98))  %>%
  # mutate(por14=tot14/validos14*100,
  #        por10=tot10/validos10*100,
  #        por06=tot06/validos06*100,
  #        por02=tot02/validos02*100,
  #        por98=tot98/validos98*100) %>%
  select(cod_provincia, provincia, cod_canton, canton, X08.01.2021)

cantones %>%
  select(canton)%>%
  arrange(desc(evangelicos))


cantones %>%
  select(canton, por10)%>%
  filter(por10>=10) %>%
  arrange(desc(por10))

cantones %>%
  select(canton, por06)%>%
  filter(por06>=10)%>%
  arrange(desc(por06))

cantones %>%
  select(canton, por02)%>%
  filter(por02>=10)%>%
  arrange(desc(por02))

cantones %>%
  select(canton, por98)%>%
  filter(por98>=10)%>%
  arrange(desc(por98))

provincia <- evangelicos %>%
  group_by(provincia) %>%
  summarise(validos14=sum(validos14),
            tot14=sum(tot14),
            validos10=sum(validos10),
            tot10=sum(tot10),
            validos06=sum(validos06),
            tot06=sum(tot06),
            validos02=sum(validos02),
            tot02=sum(tot02),
            validos98=sum(validos98),
            tot98=sum(tot98))  %>%
  mutate(Y2014=tot14/validos14*100,
         Y2010=tot10/validos10*100,
         Y2006=tot06/validos06*100,
         Y2002=tot02/validos02*100,
         Y1998=tot98/validos98*100) %>%
  select(provincia, starts_with("Y"))
provincia


provincia2 <- provincia%>%
  gather(anio, porcentaje,-provincia) %>%
  arrange(anio)
head(provincia2, 10)

####################

graficoprovincias<- ggplot(provincia2, aes(x=anio, y=porcentaje, group=provincia, colour=provincia)) + 
  geom_line(size=1) +
  ggtitle("Evolucion del voto evangelico") +
  theme_fivethirtyeight() +
  ylab("Porcentaje de votos a partidos evangelicos") +
  theme(axis.title = element_text()) + 
  theme(axis.title.x = element_blank())

graficoprovincias


total<- evangelicos %>%
  summarise(validos14=sum(validos14),
            tot14=sum(tot14),
            validos10=sum(validos10),
            tot10=sum(tot10),
            validos06=sum(validos06),
            tot06=sum(tot06),
            validos02=sum(validos02),
            tot02=sum(tot02),
            validos98=sum(validos98),
            tot98=sum(tot98))  %>%
  mutate(Y2014=tot14/validos14*100,
         Y2010=tot10/validos10*100,
         Y2006=tot06/validos06*100,
         Y2002=tot02/validos02*100,
         Y1998=tot98/validos98*100) %>%
  select(starts_with("Y"))
total


total <- total%>%
  gather(anio, porcentaje) %>%
  arrange(desc(anio))

graficoevolucion <- ggplot(total, aes(x=anio, y=porcentaje, group=1)) + 
  geom_line(size=1.5, color="#00cef6") +
  ggtitle("Evolucion del voto evangelico") +
  theme_fivethirtyeight() +
  ylab("Porcentaje de votos a partidos evangélicos") +
  theme(axis.title = element_text()) + 
  theme(axis.title.x = element_blank())
graficoevolucion


############################
library("gpclib")
library("raster")
library("maptools")
library("broom")
library("mapproj")
gpclibPermit()

install.packages('rgeos', type='source')
install.packages('rgdal', type='source')
install.packages("gpclib", type="source")

require(c("gpclib", "maptools"))
unioned <- unionSpatialPolygons(cp, invert(polys))

library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
#Importar geodatos
cr <- getData("GADM", country = "CRI", level = 2)
#transformar geodatos
cr2<- fortify(cr, region = "HASC_2")
#Unir bases
cr_mapa <- merge(cr2, cantones, by.x= "id", by.y="HASC", all.x = TRUE)
#ordenar pol?gonos
ord2<- order(cr_mapa$order)
cr_mapa <- cr_mapa[ord2, ]


m_1998 <-ggplot() +
  geom_polygon(data = cr_mapa, aes(x = long, y = lat, group = group, fill = por98)) +
  coord_map() + ylim(8, NA) + 
  scale_fill_gradient(low = "#c6c6c6", high = "#1f4263", limits = c(0, 25)) +
  labs(x = NULL, 
       y = NULL, 
       title = "Voto a partidos cristianos", 
       subtitle = "Elecciones 1998") +
  theme_void()

m_2002 <-ggplot() +
  geom_polygon(data = cr_mapa, aes(x = long, y = lat, group = group, fill = por02)) +
  coord_map() + ylim(8, NA) + 
  scale_fill_gradient(low = "#c6c6c6", high = "#1f4263", limits = c(0, 25)) +
  labs(x = NULL, 
       y = NULL, 
       title = "Voto a partidos cristianos", 
       subtitle = "Elecciones 2002") +
  theme_void()

m_2006 <-ggplot() +
  geom_polygon(data = cr_mapa, aes(x = long, y = lat, group = group, fill = por06)) +
  coord_map() + ylim(8, NA) + 
  scale_fill_gradient(low = "#c6c6c6", high = "#1f4263", limits = c(0, 25)) +
  labs(x = NULL, 
       y = NULL, 
       title = "Voto a partidos cristianos", 
       subtitle = "Elecciones 2006") +
  theme_void()
m_2010 <-ggplot() +
  geom_polygon(data = cr_mapa, aes(x = long, y = lat, group = group, fill = por10)) +
  coord_map() + ylim(8, NA) + 
  scale_fill_gradient(low = "#c6c6c6", high = "#1f4263", limits = c(0, 25)) +
  labs(x = NULL, 
       y = NULL, 
       title = "Voto a partidos cristianos", 
       subtitle = "Elecciones 2010")+
  theme_void()
m_2014 <-ggplot() +
  geom_polygon(data = cr_mapa, aes(x = long, y = lat, group = group, fill = por14)) +
  coord_map() + ylim(8, NA) + 
  scale_fill_gradient(low = "#c6c6c6", high = "#1f4263", limits = c(0, 25)) +
  labs(x = NULL, 
       y = NULL, 
       title = "Voto a partidos cristianos", 
       subtitle = "Elecciones 2014")+
  theme_void()



#############################
confirmados <- read.csv("01_08_CSV_POSITIVOS.csv", sep = ";",header = TRUE) 
head(confirmados)

confirmados$confirmed <- confirmados[,304]



library(flexdashboard)
valueBox(
  value = paste(format(sum(confirmados$confirmed), big.mark = ","), "", sep = " "),
  caption = "Total confirmed cases",
  icon = "fas fa-user-md",
  color = "blue"
)

fallecidos <- read.csv("01_08_CSV_FALLECIDOS.csv", sep = ";",header = TRUE) 
head(fallecidos)

fallecidos$death <- fallecidos[,267]

valueBox(
  value = paste(format(sum(fallecidos$death, na.rm = TRUE), big.mark = ","), " (",
                round(100 * sum(fallecidos$death, na.rm = TRUE) / sum(confirmados$confirmed), 1),
                "%)",
                sep = ""
  ),
  caption = "Death cases (death rate)",
  icon = "fas fa-heart-broken",
  color = "red"
)

####
df_daily <- 

plotly::plot_ly(data = df_daily) %>%
  plotly::add_trace(
    x = ~date,
    # y = ~active_cum,
    y = ~confirmed_cum,
    type = "scatter",
    mode = "lines+markers",
    # name = "Active",
    name = "Confirmed",
    line = list(color = active_color),
    marker = list(color = active_color)
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~death_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "Death",
    line = list(color = death_color),
    marker = list(color = death_color)
  ) %>%
  plotly::layout(
    title = "",
    yaxis = list(title = "Cumulative number of cases"),
    xaxis = list(title = "Date"),
    legend = list(x = 0.1, y = 0.9),
    hovermode = "compare"
  )


# map tab added by Art Steinmetz
library(leaflet)
library(leafpop)
library(purrr)
cv_data_for_plot <- coronavirus %>%
  # dplyr::filter(Country.Region == "Costa Rica") %>%
  dplyr::filter(cases > 0) %>%
  dplyr::group_by(Country.Region, Province.State, Lat, Long, type) %>%
  dplyr::summarise(cases = sum(cases)) %>%
  dplyr::mutate(log_cases = 2 * log(cases)) %>%
  dplyr::ungroup()
cv_data_for_plot.split <- cv_data_for_plot %>% split(cv_data_for_plot$type)
pal <- colorFactor(c("orange", "red", "green"), domain = c("confirmed", "death", "recovered"))
map_object <- leaflet() %>% addProviderTiles(providers$Stamen.Toner)
names(cv_data_for_plot.split) %>%
  purrr::walk(function(df) {
    map_object <<- map_object %>%
      addCircleMarkers(
        data = cv_data_for_plot.split[[df]],
        lng = ~Long, lat = ~Lat,
        #                 label=~as.character(cases),
        color = ~ pal(type),
        stroke = FALSE,
        fillOpacity = 0.8,
        radius = ~log_cases,
        popup = leafpop::popupTable(cv_data_for_plot.split[[df]],
                                    feature.id = FALSE,
                                    row.numbers = FALSE,
                                    zcol = c("type", "cases", "Country.Region", "Province.State")
        ),
        group = df,
        #                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
        labelOptions = labelOptions(
          noHide = F,
          direction = "auto"
        )
      )
  })

map_object %>%
  addLayersControl(
    overlayGroups = names(cv_data_for_plot.split),
    options = layersControlOptions(collapsed = FALSE)
  )


################
daily_confirmed <- confirmados %>%
  dplyr::filter("confirmed") %>%
  dplyr::summarise(total = sum(cases)) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = country, values_from = total)

#----------------------------------------
# Plotting the data

daily_confirmed %>%
  plotly::plot_ly() %>%
  plotly::add_trace(
    x = ~date,
    y = ~`Costa Rica`,
    type = "scatter",
    mode = "lines+markers",
    name = "Costa Rica"
  ) %>%
  # plotly::add_trace(
  #   x = ~date,
  #   y = ~Panama,
  #   type = "scatter",
  #   mode = "lines+markers",
  #   name = "Panama"
  # ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~Colombia,
    type = "scatter",
    mode = "lines+markers",
    name = "Colombia"
  ) %>%
  plotly::add_trace(
    x = ~date,
    y = ~Uruguay,
    type = "scatter",
    mode = "lines+markers",
    name = "Uruguay"
  ) %>%
  plotly::layout(
    title = "",
    legend = list(x = 0.1, y = 0.9),
    yaxis = list(title = "Number of new confirmed cases"),
    xaxis = list(title = "Date"),
    # paper_bgcolor = "black",
    # plot_bgcolor = "black",
    # font = list(color = 'white'),
    hovermode = "compare",
    margin = list(
      # l = 60,
      # r = 40,
      b = 10,
      t = 10,
      pad = 2
    )
  )