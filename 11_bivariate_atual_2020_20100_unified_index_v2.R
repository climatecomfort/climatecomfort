install.packages(c("faux","raster","terra","ggplot2"))
install.packages("cowplot")
install.packages("biscale", dependencies = TRUE)
install.packages("tidyverse")
install.packages("mapproj")
install.packages("ggspatial")

library(biscale)   # bivariate mapping
library(cowplot)   # combine ggplot2 objects
library(faux)      # create simulated data
library(ggplot2)   # create maps
library(ggspatial) # add scale and north arrow
library(mapproj)   #coordinates in ggplot2 maps
library(terra)    # work with raster data
library(tidyverse)

# first raster ("population_density_2020.tif"):
population_2020<-rast("ssp2_2020.tif") # from this source: https://sedac.ciesin.columbia.edu/data/set/popdynamics-1-8th-pop-base-year-projection-ssp-2000-2100-rev01
population_2100<-rast("ssp2_2100.tif") # from this source: https://sedac.ciesin.columbia.edu/data/set/popdynamics-1-8th-pop-base-year-projection-ssp-2000-2100-rev01
population_change<-population_2100 - population_2020
#terra::plot(population_change, breaks=3, breakby="cases")
#writeRaster(population_change,"population_change.tif")

# second raster ("final_data_index")
final_index<-rast("final_data_index.tif")
final_index_reproject<-project(final_index,population_change)
final_index_reproject_resampled<-resample(final_index_reproject,population_change)
#plot(final_index_reproject_resampled,breaks=3, breakby="cases")

#This script is based on the example at https://chris-prener.github.io/biscale/articles/rasters.html
final_index_dataframe<- as.data.frame(final_index_reproject_resampled, xy=TRUE)
#View(final_index_dataframe)
final_index_dataframe$final_index <- final_index_dataframe$mean
bivariate_change_dataframe<-as.data.frame(population_change, xy=TRUE)
#View(bivariate_change_dataframe)
bivariate_change_dataframe$change<-bivariate_change_dataframe$ssp2_2100
bivariate_change<-left_join(final_index_dataframe, bivariate_change_dataframe, by = c("x"="x", "y"="y"))
#View(bivariate_change)
bivariate_change[is.na(bivariate_change)] <- 0 #change null values to zero
#hist(bivariate_change$change)
#bivariate_change$change_bin <- cut(bivariate_change$change, breaks = c(min(bivariate_change$change),0,500, max(bivariate_change$change)), include.lowest = TRUE)
bivariate_change_biscale <- bi_class(bivariate_change, x = final_index, y = change, style = "quantile")
#View(bivariate_change_biscale)
map <- ggplot() +
  geom_tile(data = bivariate_change_biscale , aes(x = x, y = y, fill = bi_class)) +
  bi_scale_fill(pal = "DkBlue") +
  coord_quickmap() +
  labs(
    title = "",
    x = "",
    y = "",
  ) +
  # bi_theme(base_size = 16) + # we take out this, in order to see the lat long coordinates
  annotation_north_arrow(which_north = "grid", height = unit(0.5, "cm"), width = unit(0.4, "cm"), pad_x = unit(8.6, "cm"), pad_y = unit(0.6, "cm"), style = north_arrow_orienteering(text_size = 0)) +
  #annotation_scale(plot_unit = "km", pad_x = unit(7, "cm")) +
  theme(legend.position="none", 
        panel.background = element_rect(fill = "snow4",colour = "snow4"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA))

labels1<-bi_class_breaks(bivariate_change_biscale, x = final_index, y = change, style = "quantile",
                             dim = 3, dig_lab = c(x = 3, y = 3), split = TRUE)
#View(labels1$bi_y)
labels1$bi_y<-c(-982632,-18,0,3246085) # changing manually the breaks of the legend


my.labels <- c("Ambystoma\nmexicanum",
               "Daubentonia madagascariensis", 
               "Psychrolutes marcidus") # first create labels, add \n where appropriate.

myplot + 
  scale_x_discrete(labels= my.labels)

legend <- bi_legend(pal = "DkBlue",
                    xlab = "Dissatisfaction rate",
                    ylab = "Population change",
                    size = 24,
                    breaks = labels1)

## construct final plot
finalPlot <-  plot_grid(
  map, legend,
  rel_widths = c(1, .4),
  nrow = 1
)

## print final plot
map
legend
finalPlot
