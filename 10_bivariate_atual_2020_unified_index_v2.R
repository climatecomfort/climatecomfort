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
# second raster ("final_data_index")
final_index<-rast("final_data_index.tif")
final_index_reproject<-project(final_index,population_2020)
final_index_reproject_resampled<-resample(final_index_reproject,population_2020)

#This script is based on the example at https://chris-prener.github.io/biscale/articles/rasters.html
final_index_dataframe<- as.data.frame(final_index_reproject_resampled, xy=TRUE)
final_index_dataframe$final_index <- final_index_dataframe$mean
bivariate_2020_dataframe<-as.data.frame(population_2020, xy=TRUE)
bivariate_2020<-left_join(final_index_dataframe, bivariate_2020_dataframe, by = c("x"="x", "y"="y"))
bivariate_2020[is.na(bivariate_2020)] <- 0
bivariate_2020_biscale <- bi_class(bivariate_2020, x = final_index, y = ssp2_2020, style = "quantile")
map <- ggplot() +
  geom_tile(data = bivariate_2020_biscale , aes(x = x, y = y, fill = bi_class)) +
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

labels_2020<-bi_class_breaks(bivariate_2020_biscale, x = final_index, y = ssp2_2020, style = "quantile",
                         dim = 3, dig_lab = c(x = 3, y = 3), split = TRUE)

legend <- bi_legend(pal = "DkBlue",
                    xlab = "Dissatisfaction rate ",
                    ylab = "Population density",
                    size = 24,
                    breaks=labels_2020)
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
