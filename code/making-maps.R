#imports
install.packages(sf, raster, dplyr, ggplot2, rgdal, tmap)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(rgdal)
library(tmap)

ternary_paaf <- brick("paf_gama_Ternario.tif")
#plotRGB(paaf)


coords_max <- st_bbox(ternary_paaf)
x_len <- coords_max[[3]] - coords_max[[1]]
y_len <- coords_max[[4]] - coords_max[[2]]

ternary <- read.csv("paf_gama_Ternario.csv")

#to convert table to coords utm
table <- ternary
for (c in 1:length(table[[1]])) {
  table$x[c] <- (table$x[c] * x_len) + coords_max[[1]]
  table$y[c] <- coords_max[[4]] - (table$y[c] * y_len)
  table$w[c] <- table$w[c] * x_len
  table$h[c] <- table$h[c] * y_len
}

#to convert x y w h to x y
new_table <- data.frame(x = numeric(), y = numeric())
k = 1
for (i in 1:length(table[[1]])) {
  x <- table$x[i] - (table$w[i]/2)
  y <- table$y[i] - (table$h[i]/2)
  new_table <- rbind(new_table, c(x, y))
  
  x <- table$x[i]+(table$w[i]/2)
  y <- table$y[i]+(table$h[i]/2)
  new_table <- rbind(new_table, c(x, y))
}

#creating the sf object

paaf_crs <- crs(ternary_paaf, asText = TRUE)
points <- st_as_sf(new_table, coords = c(names(new_table)[1], names(new_table)[2]), 
                   crs = paaf_crs)


counter <- 1
len <- length(points[[1]])
for (p in seq(1, len, 2)) {
  ifelse(counter == 1, bboxes <- st_make_grid(points[p:p+1, ], n = 1), 
         bboxes[counter] <- st_make_grid(points[p:p+1, ], n = 1))
  counter = counter + 1
}


ternary_pred <- poly2
ternary_pred <- st_transform(bboxes, crs = crs(paaf, asText = TRUE))

ternary_pred_union <- st_union(ternary_pred)


#ternary
#ternary_paaf <- brick("paf_gama_Ternario.tif")
#ternary_pred <- st_read("ternary_union.shp")
ternary_pred_union <- st_transform(ternary_pred_union, crs = crs(ternary_paaf, asText = TRUE))

ternary_pred_union <- st_cast(ternary_pred_union, "POLYGON")


red_ternary <- subset(ternary_paaf, 1)
red_values_ternary <- extract(red_ternary, ternary_pred_union, fun = mean)
green_values_ternary <- extract(subset(ternary_paaf, 2), ternary_pred_union, fun = mean)
blue_values_ternary <- extract(subset(ternary_paaf, 3), ternary_pred_union, fun = mean)

gb_values_ternary <- (green_values_ternary + blue_values_ternary) / 2
values_ternary <- red_values_ternary - gb_values_ternary

ternary_pred_union <- mutate(ternary_pred_union, red_values = values_ternary + 255)

#
#
coords_max <- st_bbox(ternary_paaf)
x_len <- coords_max[[3]] - coords_max[[1]]
y_len <- coords_max[[4]] - coords_max[[2]]

ThK <- read.csv("paf_gama_ThK.csv")          

#to convert table to coords utm
table <- ThK
for (c in 1:length(table[[1]])) {
  table$x[c] <- (table$x[c] * x_len) + coords_max[[1]]
  table$y[c] <- coords_max[[4]] - (table$y[c] * y_len)
  table$w[c] <- table$w[c] * x_len
  table$h[c] <- table$h[c] * y_len
}

#to convert x y w h to x y
new_table <- data.frame(x = numeric(), y = numeric())
k = 1
for (i in 1:length(table[[1]])) {
  x <- table$x[i] - (table$w[i]/2)
  y <- table$y[i] - (table$h[i]/2)
  new_table <- rbind(new_table, c(x, y))
  
  x <- table$x[i]+(table$w[i]/2)
  y <- table$y[i]+(table$h[i]/2)
  new_table <- rbind(new_table, c(x, y))
}

#creating the sf object

paaf_crs <- crs(ternary_paaf, asText = TRUE)
points <- st_as_sf(new_table, coords = c(names(new_table)[1], names(new_table)[2]), 
                   crs = paaf_crs)

counter <- 1
len <- length(points[[1]])
for (p in seq(1, len, 2)) {
  ifelse(counter == 1, bboxes <- st_make_grid(points[p:p+1, ], n = 1), 
         bboxes[counter] <- st_make_grid(points[p:p+1, ], n = 1))
  counter = counter + 1
}



ternary_pred <- st_transform(bboxes, crs = crs(paaf, asText = TRUE))
ThK_union <- st_union(ternary_pred)


#ThK

thk_pred <- st_transform(ThK_union, crs = crs(ternary_paaf, asText = TRUE))

thk_pred <- st_cast(thk_pred, "POLYGON")

blue_thk <- extract(subset(ternary_paaf, 3), thk_pred, fun = mean)
red_thk <- extract(subset(ternary_paaf, 1), thk_pred, fun = mean)
green_thk <- extract(subset(ternary_paaf, 2), thk_pred, fun = mean)

rg_values_thk <- (red_thk + green_thk) / 2
values_thk <- blue_thk - rg_values_thk

thk_pred <- mutate(thk_pred, blue_values = values_thk + 255) #+255 para nao ter caso de numero negativo o que daria problema na hora da soma ponderada


#
#
coords_max <- st_bbox(ternary_paaf)
x_len <- coords_max[[3]] - coords_max[[1]]
y_len <- coords_max[[4]] - coords_max[[2]]

fator <- read.csv("PAAF-fator-f.csv")                      

#to convert table to coords utm
table <- fator
for (c in 1:length(table[[1]])) {
  table$x[c] <- (table$x[c] * x_len) + coords_max[[1]]
  table$y[c] <- coords_max[[4]] - (table$y[c] * y_len)
  table$w[c] <- table$w[c] * x_len
  table$h[c] <- table$h[c] * y_len
}

#to convert x y w h to x y
new_table <- data.frame(x = numeric(), y = numeric())
k = 1
for (i in 1:length(table[[1]])) {
  x <- table$x[i] - (table$w[i]/2)
  y <- table$y[i] - (table$h[i]/2)
  new_table <- rbind(new_table, c(x, y))
  
  x <- table$x[i]+(table$w[i]/2)
  y <- table$y[i]+(table$h[i]/2)
  new_table <- rbind(new_table, c(x, y))
}

#creating the sf object

paaf_crs <- crs(ternary_paaf, asText = TRUE)
points <- st_as_sf(new_table, coords = c(names(new_table)[1], names(new_table)[2]), 
                   crs = paaf_crs)

counter = 1
len <- length(points[[1]])
for (p in seq(1, len, 2)) {
  ifelse(counter == 1, bboxes <- st_make_grid(points[p:p+1, ], n = 1), 
         bboxes[counter] <- st_make_grid(points[p:p+1, ], n = 1))
  counter = counter + 1
}


fator_union <- st_union(bboxes)


#fator f
fator_paaf <- brick("fator_paaf_gis.tif")                    #-> verificar no pycharm
#fator_pred <- st_read("fator2-union.shp")                    #-> vira fator_union
crs(fator_paaf) <- "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
fator_pred <- st_transform(fator_union, crs = crs(ternary_paaf, asText = TRUE))

fator_pred <- st_cast(fator_pred, "POLYGON")
fator_pred_cast <- fator_pred[1:34, ]

st_write(fator_pred, "fator_pred_cast.shp")

red_fator <- extract(subset(fator_paaf, 1), fator_pred_cast, fun = mean, na.rm=TRUE)
green_fator <- extract(subset(fator_paaf, 2), fator_pred_cast, fun = mean, na.rm=TRUE)
blue_fator <- extract(subset(fator_paaf, 3), fator_pred_cast, fun = mean, na.rm=TRUE)

rg_values_fator <- (red_fator + green_fator) / 2
values_fator <- rg_values_fator - blue_fator

fator_pred_cast <- mutate(fator_pred_cast, rg_values = values_fator + 255)



#
#
#
#

#rasterizing the shapefiles
blank_raster <- subset(fator_paaf, 1)
blank_raster[is.na(blank_raster)] <- 0       #retirando NA's values
blank_raster <- reclassify(blank_raster, rcl = cbind(0, 255, 0))   #todas as cell = 0
blank_raster <- reclassify(blank_raster, rcl = cbind(NA, 0))       #tudo NA

#ternary
ternary_pred_raster <- rasterize(ternary_pred, blank_raster, 
                                 field = ternary_pred$red_values, 
                                 update = TRUE)
#writeRaster(ternary_pred_raster, "ternary_pred_raster.tif")
#thk
thk_pred_raster <- rasterize(thk_pred, blank_raster, 
                             field = thk_pred$blue_values, 
                             update = TRUE)
#writeRaster(thk_pred_raster, "thk_pred_raster.tif")
#fator
fator_pred_raster <- rasterize(fator_pred_cast, blank_raster, 
                               field = fator_pred_cast$rg_values, 
                               update = TRUE)
#writeRaster(fator_pred_raster, "fator_pred_raster.tif")
#layers calculations
predict <- ternary_pred_raster + 1.5*thk_pred_raster + 2*fator_pred_raster
predict <- reclassify(predict, rcl = cbind(0, NA))





#data

predict <- predict / 1210
writeRaster(predict, "predict_gama_n.tif")



map2 <- tm_shape(predict) + tm_raster(style = "cont", palette = "Greens", legend.reverse = TRUE,
                              title = "Predição Gama") + 
  tm_layout(main.title ="Predições Obtidas a Partir de Dados Gamaespectrométricos",
            main.title.position = "center", 
            legend.outside = TRUE, 
            legend.position = c("right", "top")) + 
  tm_grid(n.x = 5, n.y = 5, lines = FALSE) + 
  tm_compass() + 
  tm_scale_bar() + tm_layout(attr.outside = TRUE, attr.position = c("center", "bottom"))

tmap_save(map2, "pred-gama.jpg")

'''
plot(predict, main = "Predição de Regiões Alvos e Locais de Ocorrência Mineral (Ouro)")
plot(ocor_pri, add = TRUE, col = "red", pch = 3)
plot(ocor_seg, add = TRUE, col = "blue", pch = 3)

ternary_pred <- raster("ternary_pred_raster.tif")
ternary_pred <- reclassify(ternary_pred, cbind(0, NA))
plot(ternary_pred, main = "Anômalias de K")

thk_pred <- raster("thk_pred_raster.tif")
thk_pred <- reclassify(thk_pred, cbind(0, NA))
plot(thk_pred, main = "Baixos Valores Para Razão Th/K")

fator_pred <- raster("fator_pred_raster.tif")
fator_pred <- reclassify(fator_pred, cbind(0, NA))
plot(fator_pred, main = "Anomalias Fator F")
'''