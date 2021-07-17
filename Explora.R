library(rgeos)
library(gdal)
library(rgdal) # needs gdal > 1.11.0
library(ggplot2)
library(readOGR)
library(ogrListLayers)

ogrListLayers("division.json")

library(rgeos)
library(rgdal) # needs gdal > 1.11.0
library(ggplot2)

# ggplot map theme
devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")

map = readOGR("readme-swiss.json", "cantons")

map_df <- fortify(map)

slotNames(map)


# map theme
devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")

# grab each of the layers

limites = readOGR("division.json", "limites")
provincias = readOGR("division.json", "provincias")
cantones = readOGR("division.json", "cantones")
distritos = readOGR("division.json", "distritos")

# make the data frame

limites_df <- fortify(limites)
cantones_df <- fortify(cantones)
distritos_df <- fortify(distritos)
provincias_df <- fortify(provincias)

# make a map! style it like the D3 example

gg <- ggplot()
gg <- gg + geom_map(data=limites_df, map=limites_df,
                    aes(map_id=id, x=long, y=lat, group=group),
                    color="white", fill="#dddddd", size=0.25)
gg <- gg + geom_map(data=cantones_df, map=cantones_df,
                    aes(map_id=id, x=long, y=lat, group=group),
                    color="red", fill="#ffffff00", size=0.2)
gg <- gg + geom_map(data=distritos_df, map=distritos_df,
                    aes(map_id=id, x=long, y=lat, group=group),
                    color="#999999", fill="#ffffff00", size=0.1)
gg <- gg + geom_map(data=provincias_df, map=provincias_df,
                    aes(map_id=id, x=long, y=lat, group=group),
                    color="black", fill="#ffffff00", size=0.33)
gg <- gg + coord_map()
gg <- gg + labs(x="", y="", title="Costa Rica TopoJSON")
gg <- gg + theme_map()
gg

ggsave("costarica.svg", gg, width=11, height=9)


con <- url("http://gadm.org/data/rda/CRI_adm1.RData")
print(load(con))
close(con)
costa <- readGDAL("costaRica.tif")


temp <- Polygons(list(Polygon(gadm@polygons[[6]]@Polygons[[27]]@coords),Polygon(gadm@polygons[[6]]@Polygons[[25]]@coords)),"puntarenas")
temp <- SpatialPolygons(list(temp),proj4string=CRS(proj4string(gadm)))
punt.sp <- SpatialPolygonsDataFrame(temp, data.frame(cbind(2,2), row.names=c("puntarenas")))  # puntarenas


puntarenas <- costa
puntarenas.overlay <- overlay(costa,punt.sp)  # 1 in interior of puntarenas polygons, 0 outside
puntarenas$band1 <- costa$band1*puntarenas.overlay

levs <- sort(unique(puntarenas$band1));  # land cover units present