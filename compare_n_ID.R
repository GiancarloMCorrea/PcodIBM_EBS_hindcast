# Clean environment:
rm(list = ls())

# Set working directory to source file location

# Load libraries (not sure if all of them are used):
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
require(raster)
library(jcolors)
library(gapminder)
library(ggplot2)
library(wesanderson)
library(ggcorrplot)
library(magick)
require(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
require(data.table)
library(mapdata)
library(marmap)
library(tidyverse)
require(mapproj)
require(reshape2)
library(gganimate)
require(lubridate)
require(scales)
require(rnaturalearth)
require(rnaturalearthdata)
library(gridExtra)
library(stringr)
require(sf)

# Some important information:
source('aux_functions.R')
bathy1 = read.csv('main_files_hindcast/bathy1.csv')
bathy2 = read.csv('main_files_hindcast/bathy2.csv')
# Modify bathy2 for plotting
tmp_bathy2 = bathy2[bathy2$lon == 179.9, ]
tmp_bathy2$lon = 180
bathy2 = rbind(bathy2, tmp_bathy2)

# Alaska map for plotting
ak = map_data('worldHires','USA:Alaska')
world = ne_countries(scale = "medium", returnclass = "sf")

# Some important information for plotting:
sel_years = c(2002, 2007, 2015)
thLow1 = 0.025
thHigh1 = 0.975
alphaLevel = 0.5
n_releases = 11
n_grids = 206
n_id = n_grids*n_releases

# Select folders to read:
mainCols = c('#08306B', '#C6DBEF')
color_pal = 'Dark2'

# Read processed data: ----------------------------------------------------
save_folder_1 = 'output_data/hindcast'
save_folder_2 = 'output_data/hindcast_moreID'

# Compare biological variables --------------------------------------------
# -------------------------------------------------------------------------

# Hatching success
load(file = file.path(save_folder_1, 'plot_data_1d.RData'))
plot_data_1a = bind_rows(plot_data_1d)
plot_data_1a = plot_data_1a %>% dplyr::mutate(type = 'Total ID: 2266')
plot_data_1a = plot_data_1a %>% filter(year %in% sel_years)
load(file = file.path(save_folder_2, 'plot_data_1d.RData'))
plot_data_1b = bind_rows(plot_data_1d)
plot_data_1b = plot_data_1b %>% dplyr::mutate(type = 'Total ID: 6386')
plot_data_1b = plot_data_1b %>% filter(year %in% sel_years)

plot_data_1 = rbind(plot_data_1a, plot_data_1b)
plot_1 = ggplot(plot_data_1, aes(x = factor(year))) + 
  geom_boxplot(aes(y = hatsuc), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Hatch success') +
  facet_wrap(~ type)


# Survival prob
load(file = file.path(save_folder_1, 'plot_data_2d.RData'))
plot_data_2a = bind_rows(plot_data_2d)
plot_data_2a = plot_data_2a %>% dplyr::mutate(type = 'Total ID: 2266')
plot_data_2a = plot_data_2a %>% filter(year %in% sel_years)
load(file = file.path(save_folder_2, 'plot_data_2d.RData'))
plot_data_2b = bind_rows(plot_data_2d)
plot_data_2b = plot_data_2b %>% dplyr::mutate(type = 'Total ID: 6386')
plot_data_2b = plot_data_2b %>% filter(year %in% sel_years)

plot_data_2 = rbind(plot_data_2a, plot_data_2b)
plot_2 = ggplot(plot_data_2, aes(x = factor(year))) + 
  geom_boxplot(aes(y = psurv*1e-06), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Survival probability') +
  facet_wrap(~ type)

# Standard length:
load(file = file.path(save_folder_1, 'plot_data_4d.RData'))
plot_data_3a = bind_rows(plot_data_4d)
plot_data_3a = plot_data_3a %>% dplyr::mutate(type = 'Total ID: 2266')
plot_data_3a = plot_data_3a %>% filter(year %in% sel_years)
load(file = file.path(save_folder_2, 'plot_data_4d.RData'))
plot_data_3b = bind_rows(plot_data_4d)
plot_data_3b = plot_data_3b %>% dplyr::mutate(type = 'Total ID: 6386')
plot_data_3b = plot_data_3b %>% filter(year %in% sel_years)

plot_data_3 = rbind(plot_data_3a, plot_data_3b)
plot_3 = ggplot(plot_data_3, aes(x = factor(year))) + 
  geom_boxplot(aes(y = SL), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Standard length (mm)')  +
  facet_wrap(~ type)


# Growth performance
load(file = file.path(save_folder_1, 'plot_data_5d.RData'))
plot_data_4a = bind_rows(plot_data_5d)
plot_data_4a = plot_data_4a %>% dplyr::mutate(type = 'Total ID: 2266')
plot_data_4a = plot_data_4a %>% filter(year %in% sel_years)
load(file = file.path(save_folder_2, 'plot_data_5d.RData'))
plot_data_4b = bind_rows(plot_data_5d)
plot_data_4b = plot_data_4b %>% dplyr::mutate(type = 'Total ID: 6386')
plot_data_4b = plot_data_4b %>% filter(year %in% sel_years)

plot_data_4 = rbind(plot_data_4a, plot_data_4b)
plot_4 = ggplot(plot_data_4, aes(x = factor(year))) + 
  geom_boxplot(aes(y = Gperf), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Growth performance') +
  facet_wrap(~ type)

# Make biological variables plot ------------------------------------------
png(filename = 'figures/hind_biovar_compareID.png', width = 190, height = 220, 
    units = 'mm', res = 500)
grid.arrange(plot_1, plot_2, plot_3, plot_4, ncol=1)
dev.off()

