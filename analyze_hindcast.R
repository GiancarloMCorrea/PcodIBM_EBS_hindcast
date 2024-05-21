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
min_plot_year = 2002
max_plot_year = 2018
by_plot_year = 4
x_labs_years = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)
allYears = 2000:2020
x_labs = rep('', times = length(allYears))
x_labs[allYears %in% x_labs_years] = as.character(x_labs_years)
relday_vec = seq(from = 1, to = 31, by = 3)
thLow1 = 0.025
thHigh1 = 0.975
alphaLevel = 0.5
n_releases = 11
n_grids = 206
n_id = n_grids*n_releases

# Select folders to read:
mainCols = c('#08306B', '#C6DBEF')
color_pal = 'Dark2'

# Read output files -------------------------------------------------------
# DisMELS outputs not provided due to file size (too big)
main_folder = 'E:/DisMELS_save_outputs/save_hindcast' # directory where the DisMELS outputs are
save_folder = 'output_data/hindcast' # directory where to save the processed data for plotting

# Read processed data: ----------------------------------------------------
load(file = file.path(save_folder, 'plot_data_0a.RData'))
load(file = file.path(save_folder, 'plot_data_0b.RData'))
load(file = file.path(save_folder, 'plot_data_0c.RData'))
load(file = file.path(save_folder, 'plot_data_0d.RData'))
load(file = file.path(save_folder, 'plot_data_0e.RData'))
load(file = file.path(save_folder, 'plot_data_1a.RData'))
load(file = file.path(save_folder, 'plot_data_1b.RData'))
load(file = file.path(save_folder, 'plot_data_1c.RData'))
load(file = file.path(save_folder, 'plot_data_1d.RData'))
load(file = file.path(save_folder, 'plot_data_2a.RData'))
load(file = file.path(save_folder, 'plot_data_2b.RData'))
load(file = file.path(save_folder, 'plot_data_2c.RData'))
load(file = file.path(save_folder, 'plot_data_2d.RData'))
load(file = file.path(save_folder, 'plot_data_2e.RData'))
load(file = file.path(save_folder, 'plot_data_4a.RData'))
load(file = file.path(save_folder, 'plot_data_4b.RData'))
load(file = file.path(save_folder, 'plot_data_4c.RData'))
load(file = file.path(save_folder, 'plot_data_4d.RData'))
load(file = file.path(save_folder, 'plot_data_4e.RData'))
load(file = file.path(save_folder, 'plot_data_5a.RData'))
load(file = file.path(save_folder, 'plot_data_5b.RData'))
load(file = file.path(save_folder, 'plot_data_5c.RData'))
load(file = file.path(save_folder, 'plot_data_5d.RData'))
load(file = file.path(save_folder, 'plot_data_5e.RData'))
load(file = file.path(save_folder, 'plot_data_6.RData'))
load(file = file.path(save_folder, 'plot_data_6b.RData'))
load(file = file.path(save_folder, 'plot_data_6_5.RData'))
load(file = file.path(save_folder, 'plot_data_8a.RData'))
load(file = file.path(save_folder, 'plot_data_8b.RData'))
load(file = file.path(save_folder, 'plot_data_8c.RData'))
load(file = file.path(save_folder, 'plot_data_9a.RData'))
load(file = file.path(save_folder, 'plot_data_9b.RData'))
load(file = file.path(save_folder, 'plot_data_9c.RData'))
load(file = file.path(save_folder, 'plot_data_10a.RData'))
load(file = file.path(save_folder, 'plot_data_10b.RData'))
load(file = file.path(save_folder, 'plot_data_11a.RData'))
load(file = file.path(save_folder, 'plot_data_11b.RData'))
load(file = file.path(save_folder, 'plot_data_12.RData'))
load(file = file.path(save_folder, 'plot_data_13.RData'))
load(file = file.path(save_folder, 'plot_data_14.RData'))
load(file = file.path(save_folder, 'plot_data_15.RData'))
load(file = file.path(save_folder, 'plot_data_16.RData'))
load(file = file.path(save_folder, 'baseLocs.RData'))
load(file = file.path(save_folder, 'baseLocs2.RData'))
load(file = file.path(save_folder, 'base_wgt.RData'))

# Analyze results: temporal series ---------------------------------------------------------
# -------------------------------------------------------------------------

# Hatching success + surv prob (Sep 15th) + SL (Sep 15th) ------------------------------

plot_data_1 = bind_rows(plot_data_1d)
x_line1 = 0.05
plot_1 = ggplot(plot_data_1, aes(x = factor(year))) + 
  geom_boxplot(aes(y = hatsuc), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Hatch success') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs)  +
  geom_segment(aes(x = 2, y = x_line1, xend = 6, yend = x_line1), colour = "red", 
               linewidth = 2.5) +
  geom_segment(aes(x = 8, y = x_line1, xend = 14, yend = x_line1), colour = "blue", 
               linewidth = 2.5) +
  geom_segment(aes(x = 15, y = x_line1, xend = 19, yend = x_line1), colour = "red", 
               linewidth = 2.5)

# Figure by release day:
plot_data_1 = plot_data_1 %>% separate_wider_delim(relday, '-', names = c('dayrel', 'monthrel'))
plot_dat = plot_data_1 %>% group_by(year, dayrel) %>% summarise(hatsuc = median(hatsuc))
day_levels = sort(as.numeric(unique(plot_dat$dayrel)))
plot_dat = plot_dat %>% mutate(dayrel = factor(dayrel, levels = day_levels))

# color scale: 1 = blue, 31 = yellow
p1_bio = ggplot(plot_dat, aes(x = factor(year), y = hatsuc)) +
  geom_jitter(aes(color = dayrel), width = 0.1) +
  theme_bw() +
  xlab(NULL) +
  ylab('Hatch success') +
  scale_x_discrete(limits = as.character(allYears), labels = x_labs) +
  scale_color_viridis_d() +
  theme(legend.position = 'none', axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))



plot_data_2 = bind_rows(plot_data_2d)
x_line2 = -0.0025
plot_2 = ggplot(plot_data_2, aes(x = factor(year))) + 
  geom_boxplot(aes(y = psurv*1e-06), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Survival probability') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) +
  geom_segment(aes(x = 2, y = x_line2, xend = 6, yend = x_line2), colour = "red", 
               linewidth = 2.5) +
  geom_segment(aes(x = 8, y = x_line2, xend = 14, yend = x_line2), colour = "blue", 
               linewidth = 2.5) +
  geom_segment(aes(x = 15, y = x_line2, xend = 19, yend = x_line2), colour = "red", 
               linewidth = 2.5) 



plot_data_3 = bind_rows(plot_data_4d)
x_line3 = 4
plot_3 = ggplot(plot_data_3, aes(x = factor(year))) + 
  geom_boxplot(aes(y = SL), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Standard length (mm)') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) +
  geom_segment(aes(x = 2, y = x_line3, xend = 6, yend = x_line3), colour = "red", 
               linewidth = 2.5) +
  geom_segment(aes(x = 8, y = x_line3, xend = 14, yend = x_line3), colour = "blue", 
               linewidth = 2.5) +
  geom_segment(aes(x = 15, y = x_line3, xend = 19, yend = x_line3), colour = "red", 
               linewidth = 2.5) 

# Figure by release day:
plot_data_3 = plot_data_3 %>% separate_wider_delim(relday, '-', names = c('dayrel', 'monthrel'))
plot_dat = plot_data_3 %>% group_by(year, dayrel) %>% summarise(SL = median(SL))
day_levels = sort(as.numeric(unique(plot_dat$dayrel)))
plot_dat = plot_dat %>% mutate(dayrel = factor(dayrel, levels = day_levels))

# color scale: 1 = blue, 31 = yellow
p2_bio = ggplot(plot_dat, aes(x = factor(year), y = SL)) +
  geom_jitter(aes(color = dayrel), width = 0.1) +
  theme_bw() +
  xlab(NULL) +
  ylab('Standard length (mm)') +
  scale_x_discrete(limits = as.character(allYears), labels = x_labs) +
  scale_color_viridis_d()+
  theme(legend.position = 'none', axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))


plot_data_4 = bind_rows(plot_data_5d)
x_line4 = 0.75
plot_4 = ggplot(plot_data_4, aes(x = factor(year))) + 
  geom_boxplot(aes(y = Gperf), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Growth performance') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) +
  geom_segment(aes(x = 2, y = x_line4, xend = 6, yend = x_line4), colour = "red", 
               linewidth = 2.5) +
  geom_segment(aes(x = 8, y = x_line4, xend = 14, yend = x_line4), colour = "blue", 
               linewidth = 2.5) +
  geom_segment(aes(x = 15, y = x_line4, xend = 19, yend = x_line4), colour = "red", 
               linewidth = 2.5) 

# Figure by release day:
plot_data_4 = plot_data_4 %>% separate_wider_delim(relday, '-', names = c('dayrel', 'monthrel'))
plot_dat = plot_data_4 %>% group_by(year, dayrel) %>% summarise(Gperf = median(Gperf))
day_levels = sort(as.numeric(unique(plot_dat$dayrel)))
plot_dat = plot_dat %>% mutate(dayrel = factor(dayrel, levels = day_levels))

# color scale: 1 = blue, 31 = yellow
p3_bio = ggplot(plot_dat, aes(x = factor(year), y = Gperf)) +
  geom_jitter(aes(color = dayrel), width = 0.1) +
  theme_bw() +
  xlab(NULL) +
  ylab('Growth performance') +
  coord_cartesian(ylim = c(0.98, 1)) +
  scale_x_discrete(limits = as.character(allYears), labels = x_labs) +
  scale_color_viridis_d() +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        legend.position = c(0.5, 0.15), legend.direction = 'horizontal',
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(title = NULL, nrow = 2, override.aes = list(size = 1.1)))


# Make biological variables plot ------------------------------------------
lay = matrix(c(1,2,3, 4), nrow = 2)
png(filename = 'figures/hind_biovar.png', width = 190, height = 120, 
    units = 'mm', res = 500)
grid.arrange(plot_1, plot_2, plot_3, plot_4, layout_matrix = lay)
dev.off()


# -------------------------------------------------------------------------
# By release day ----------------------------------------------------------

plot_data_1 = bind_rows(plot_data_1d)
plot_data_1$relday = factor(plot_data_1$relday, levels = paste0(relday_vec, '-3'))
plot_data_1$relday = factor(plot_data_1$relday, labels = relday_vec)

plot_1a = ggplot(plot_data_1, aes(x = factor(relday))) + 
  geom_boxplot(aes(y = hatsuc), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Hatch success') 

plot_data_2 = bind_rows(plot_data_2e)
plot_data_2$relday = factor(plot_data_2$relday, levels = paste0(relday_vec, '-3'))
plot_data_2$relday = factor(plot_data_2$relday, labels = relday_vec)

plot_2a = ggplot(plot_data_2, aes(x = factor(relday))) + 
  geom_boxplot(aes(y = psurv*1e-06), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Survival probability') # should be at 100 dph?

plot_data_3 = bind_rows(plot_data_4e)
plot_data_3$relday = factor(plot_data_3$relday, levels = paste0(relday_vec, '-3'))
plot_data_3$relday = factor(plot_data_3$relday, labels = relday_vec)

plot_3a = ggplot(plot_data_3, aes(x = factor(relday))) + 
  geom_boxplot(aes(y = SL), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Standard length (mm)') 

plot_data_4 = bind_rows(plot_data_5e)
plot_data_4$relday = factor(plot_data_4$relday, levels = paste0(relday_vec, '-3'))
plot_data_4$relday = factor(plot_data_4$relday, labels = relday_vec)

plot_4a = ggplot(plot_data_4, aes(x = factor(relday))) + 
  geom_boxplot(aes(y = Gperf), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Growth performance') 

# Make biological variables plot by release day ------------------------------------------
lay = matrix(c(1,2,3,4), nrow = 2)
png(filename = 'figures/hind_biovar_relday.png', width = 190, height = 120, 
    units = 'mm', res = 500)
grid.arrange(plot_1a, plot_2a, plot_3a, plot_4a, layout_matrix = lay)
dev.off()


# Plot: Compare standard length with obs --------------------------------------------------------

# Read obs data (ML):
sl_obs = read.csv('Compare_ML_len.csv')
sl_obs$value = sl_obs$value * 10
sl_obs = sl_obs[sl_obs$year >= 2000 & sl_obs$year <= 2020, ]
sl_obs = sl_obs[sl_obs$type == 'ML_0', ]
sl_obs$type = 'SAM'

ibm_sl = bind_rows(plot_data_4d)
ibm_sl = ibm_sl %>% group_by(year) %>% summarise(value = median(SL))
ibm_sl = ibm_sl %>% mutate(type = 'DisMELS')

plot_data = rbind(sl_obs, ibm_sl)
# Plot:
slobs = ggplot(plot_data, aes(x = year)) + 
  geom_line(aes(y = value, colour = factor(type))) +
  theme_bw() +
  xlab(NULL) +
  ylab('Mean length-at-age 0.5 (cm)') +
  theme(legend.position = c(0.5,0.9), legend.background =element_blank()) +
  guides(colour = guide_legend(title = ' ')) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)) 

# Merge plots:
jpeg(filename = 'figures/hind_SL_compare.jpg', width = 85, height = 80, 
    units = 'mm', res = 500)
print(slobs)
dev.off()


# Plot 5: Mortality by category: -------------------------------------
plot_data_4 = bind_rows(plot_data_6b)
plot_data_4$type = factor(plot_data_4$type, levels = c("mortfish", "mortinv", "mortstarv"))
plot_data_4$type = factor(plot_data_4$type, labels = c('Fish predation', 'Invertebrate predation', '% empty stomach'))

plot_data_4 = ggplot(plot_data_4, aes(x = factor(year))) + 
                geom_boxplot(aes(y = value), alpha=alphaLevel, color = mainCols[1], 
                             fill = mainCols[2], outlier.size = 0.6) +
                theme_bw() +
                xlab(NULL) +
                ylab(NULL) +
                scale_x_discrete(limits = as.character(allYears), labels = x_labs) +
                theme(legend.position = 'none') +
                facet_wrap(~ type, ncol = 3, scales = 'free_y', strip.position = "left") +
                theme(strip.background = element_blank(), strip.placement = "outside")
# expression(paste('Mortality (', s^{-1}, ', ', 10^{-6}, ')'))

# Make mortality plot ------------------------------------------

png(filename = 'figures/hind_mort_types.png', width = 190, height = 60, 
    units = 'mm', res = 500)
print(plot_data_4)
dev.off()

# Plot: Stomach fullness --------------------------------------------------
plot_data = bind_rows(plot_data_6_5)

ggplot(plot_data, aes(x = factor(year))) + 
  geom_boxplot(aes(y = quant), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Stomach fullness') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs)

# Plot 7: Dead individuals annual -------------------------------------

# Starvation:
plot_data = bind_rows(plot_data_0a)
plot_data = aggregate(list(porcsurv = plot_data$id), list(year = plot_data$year),
                      FUN = function(x) length(unique(x))/n_id)
plot_data$type = '% survived starvation'
colnames(plot_data)[2] = 'var'
my_data_a = plot_data

# Num ind out of EBS
plot_data = bind_rows(plot_data_0c)
plot_data = aggregate(list(in_id = plot_data$id), list(year = plot_data$year),
                      FUN = function(x) length(unique(x))/n_id)
plot_data$type = '% remained in the EBS'
colnames(plot_data)[2] = 'var'
my_data_b = plot_data

# Prop all individuals alive
plot_data = bind_rows(plot_data_0d)
plot_data = aggregate(list(alive_id = plot_data$id), list(year = plot_data$year),
                      FUN = function(x) length(unique(x))/n_id)
plot_data$type = 'All alive'
colnames(plot_data)[2] = 'var'
my_data_c = plot_data

my_plot_data = rbind(my_data_a, my_data_b)
my_plot_data$var = my_plot_data$var*100
my_plot_data$type = factor(my_plot_data$type, levels = c("% survived starvation","% remained in the EBS"))

plot_5 = ggplot(my_plot_data, aes(x = year, y = var)) + 
          geom_line(color = mainCols[1]) +
          theme_bw() +
          xlab(NULL) +
          ylab(NULL) +
          scale_x_continuous(expand=c(0, 0), breaks = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)) +
          scale_color_manual(values = mainCols) +
          theme(legend.position = 'none', strip.background = element_blank(),
                strip.placement = "outside") +
          facet_wrap(. ~ type, ncol = 1, scales = 'free_y', strip.position = "left")

# Make plot dead ind ------------------------------------------------------

# png(filename = 'figures/hind_dead_ind.png', width = 95, height = 130, 
#     units = 'mm', res = 500)
# print(plot_5)
# dev.off()


# Dead ind by release day -------------------------------------------------

# Starvation:
plot_data = bind_rows(plot_data_0a)
plot_data = aggregate(list(porcsurv = plot_data$id), list(rel_date = plot_data$rel_date,
                                                          year = plot_data$year),
                      FUN = function(x) length(unique(x))/n_grids)
plot_data$type = '% survived starvation'
colnames(plot_data)[3] = 'var'
my_data_a = plot_data

# Num ind out of EBS:
plot_data = bind_rows(plot_data_0c)
plot_data = aggregate(list(porcsurv = plot_data$id), list(rel_date = plot_data$rel_date,
                                                          year = plot_data$year),
                      FUN = function(x) length(unique(x))/n_grids)
plot_data$type = '% remained in the EBS'
colnames(plot_data)[3] = 'var'
my_data_b = plot_data

# Prop all individuals alive
plot_data = bind_rows(plot_data_0d)
plot_data = aggregate(list(porcsurv = plot_data$id), list(rel_date = plot_data$rel_date,
                                                          year = plot_data$year),
                      FUN = function(x) length(unique(x))/n_grids)
plot_data$type = 'All alive'
colnames(plot_data)[3] = 'var'
my_data_c = plot_data

my_plot_data = rbind(my_data_a, my_data_b)
my_plot_data$var = my_plot_data$var*100
my_plot_data$type = factor(my_plot_data$type, levels = c("% survived starvation","% remained in the EBS"))
my_plot_data$rel_date = factor(my_plot_data$rel_date, levels = paste0(relday_vec, '-3'))
my_plot_data$rel_date = factor(my_plot_data$rel_date, labels = relday_vec)

plot_5b = ggplot(my_plot_data, aes(x = factor(rel_date))) + 
  geom_boxplot(aes(y = var), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(legend.position = 'none', strip.background = element_blank(),
        strip.placement = "outside") +
  facet_wrap(type ~ ., ncol = 1, scales = 'free_y', strip.position = "left")

# Make plot dead ind by release date ------------------------------------------------------

png(filename = 'figures/hind_dead_ind_reldate.png', width = 95, height = 130, 
    units = 'mm', res = 500)
print(plot_5b)
dev.off()


# Plot 8: Index of recruitment ---------------------------------------------
plot_data = NULL
hatch_data = bind_rows(plot_data_1d)
psurv100_data = bind_rows(plot_data_2d)

# Index 1: hatch
tmp_data = hatch_data
temp = tmp_data %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(p_index = sum(hatsuc))
temp$type = 'hatch'
plot_data = rbind(plot_data, temp)

# Index 2: psurv100
tmp_data = psurv100_data
temp = tmp_data %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(p_index = sum(psurv), .groups = 'drop')
temp$type = 'psurv100'
plot_data = rbind(plot_data, temp)

# Index 3: hatch * psurv100
tmp_data = left_join(hatch_data, psurv100_data, by = c('year', 'id'))
temp = tmp_data %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(p_index = sum(hatsuc*psurv), .groups = 'drop')
temp$type = 'hatch_psurv100'
plot_data = rbind(plot_data, temp)

# Make plot:
plot_data$type = factor(plot_data$type, levels = c("hatch", 'psurv100', 'hatch_psurv100'))
plot_data$type = factor(plot_data$type, labels = c("Sigma~HS", 'Sigma~P[s]', 'Sigma~P[s]~HS'))

recin1 = ggplot(plot_data, aes(x = year, y = p_index)) + 
  geom_line(color = mainCols[1]) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)) +
  scale_color_manual(values = mainCols) +
  theme(legend.position = 'none', strip.background = element_blank()) +
  facet_wrap(type ~ ., ncol = 3, scales = 'free_y', 
             labeller = my_label_parsed)


# Make plot recruitment index ------------------------------------------------------

png(filename = 'figures/hind_rec_index.png', width = 190, height = 60, 
    units = 'mm', res = 500)
print(recin1)
dev.off()


# Plot 9: Compare ind rec with rec estimates ------------------------------
# Keep plot_data from previous plot:
rec_estimates = read.csv('Compare_Recs_Surv.csv')
rec_estimates$type = rec_estimates$scenario
rec_estimates = rec_estimates[rec_estimates$scenario == 'sage0_mod', ]
rec_estimates$type = factor(rec_estimates$type, levels = c("sage0_mod"))
rec_estimates$type = factor(rec_estimates$type, 
                        labels = c("Age-0~abundance~(billion~ind)"))

new_plot_data = rbind(plot_data, rec_estimates[,c('year', 'p_index', 'type')])
new_plot_data = new_plot_data[new_plot_data$year <= 2020 & new_plot_data$year >= 2000, ]

recin2 = ggplot(new_plot_data, aes(x = year, y = p_index)) + 
  geom_line(color = mainCols[1]) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)) +
  scale_color_manual(values = rep(mainCols, 6)) +
  theme(legend.position = 'none') +
  facet_wrap(. ~ type, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed, ncol = 2) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

# Make plot:
png(filename = 'figures/hind_rec_index_compare.png', width = 190, height = 120, 
    units = 'mm', res = 500)
print(recin2)
dev.off()


# Plot 11: Environmental variables -------------------------------------------------

# Temperature 
plot_dataT = bind_rows(plot_data_8a)
plot_dataT$state = ifelse(test = plot_dataT$state == 'alive', yes = 'Surviving', no = 'Dead')

x_line = -1.2
plot_temp_1 = ggplot(plot_dataT, aes(x = factor(year), color = factor(state), 
                               fill = factor(state))) + 
  geom_boxplot(aes(y = value), alpha=0.3, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Temperature (°C)') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) +
  scale_color_brewer(palette = color_pal) +
  scale_fill_brewer(palette = color_pal) +
  theme(legend.position = c(0.5, 0.88), legend.background =element_blank()) +
  guides(fill=guide_legend(title=NULL), color=guide_legend(title=NULL)) +
  geom_segment(aes(x = 2, y = x_line, xend = 6, yend = x_line), colour = "red", 
               linewidth = 2.5) +
  geom_segment(aes(x = 8, y = x_line, xend = 14, yend = x_line), colour = "blue", 
               linewidth = 2.5) +
  geom_segment(aes(x = 15, y = x_line, xend = 19, yend = x_line), colour = "red", 
               linewidth = 2.5) 

# By release date (differenciating dead and surviving):
plot_dataT$relday = factor(plot_dataT$relday, levels = paste0(relday_vec, '-3'))
plot_dataT$relday = factor(plot_dataT$relday, labels = relday_vec)

plot_env_1b = ggplot(plot_dataT, aes(x = relday, color = factor(state), 
                                     fill = factor(state))) + 
  geom_boxplot(aes(y = value), alpha=0.3,  outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Temperature (°C)') +
  theme(legend.position = c(0.1, 0.88), legend.background =element_blank()) +
  guides(fill=guide_legend(title=NULL),
         color=guide_legend(title=NULL)) 

# Make plot:
png(filename = 'figures/hind_temp_relday.png', width = 95, height = 60, 
    units = 'mm', res = 500)
print(plot_env_1b)
dev.off()

# Figure by release day:
plot_dataT = plot_dataT %>% filter(state == 'Surviving')
plot_dataT = plot_dataT %>% separate_wider_delim(relday, '-', names = c('dayrel', 'monthrel'))
plot_dat = plot_dataT %>% group_by(year, dayrel) %>% summarise(temperature = median(value))
day_levels = sort(as.numeric(unique(plot_dat$dayrel)))
plot_dat = plot_dat %>% mutate(dayrel = factor(dayrel, levels = day_levels))

# color scale: 1 = blue, 31 = yellow
p4_env = ggplot(plot_dat, aes(x = factor(year), y = temperature)) +
  geom_jitter(aes(color = dayrel), width = 0.1) +
  theme_bw() +
  xlab(NULL) +
  ylab('Temperature (°C)') +
  scale_x_discrete(limits = as.character(allYears), labels = x_labs) +
  scale_color_viridis_d()+
  theme(legend.position = 'none', axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5))

# Make plot by release day:
png(filename = 'figures/hind_bioenv_relday.png', width = 190, height = 170, 
    units = 'mm', res = 300)
grid.arrange(p1_bio, p2_bio, p3_bio, p4_env, ncol = 2)
dev.off()





# Depth:
plot_dataD = bind_rows(plot_data_15)
plot_dataD$state = ifelse(test = plot_dataD$state == 'alive', yes = 'Surviving', no = 'Dead')

plot_t_1 = ggplot(plot_dataD, aes(x = factor(year), color = factor(state), 
                                     fill = factor(state))) + 
  geom_boxplot(aes(y = value), alpha=0.3, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Depth (m)') +
  ylim(c(-400, 0)) +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) +
  scale_color_brewer(palette = color_pal) +
  scale_fill_brewer(palette = color_pal) +
  theme(legend.position = c(0.8, 0.15), legend.background =element_blank()) +
  guides(fill=guide_legend(title=NULL),
         color=guide_legend(title=NULL)) 

# Light:
plot_dataL = bind_rows(plot_data_16)
plot_dataL$state = ifelse(test = plot_dataL$state == 'alive', yes = 'Surviving', no = 'Dead')

plot_t_2 = ggplot(plot_dataL, aes(x = factor(year), color = factor(state), 
                                  fill = factor(state))) + 
  geom_boxplot(aes(y = value*1E-15), alpha=0.3, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(expression(Ambient~irradiance~"("*mu*mol*"."*m^{-2}*"."*s^{-1}*")")) +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) +
  scale_color_brewer(palette = color_pal) +
  scale_fill_brewer(palette = color_pal) +
  theme(legend.position = 'none', legend.background =element_blank()) +
  guides(fill=guide_legend(title=NULL),
         color=guide_legend(title=NULL)) 


# Make plot:
png(filename = 'figures/hind_depth_light.png', width = 95, height = 140, 
    units = 'mm', res = 500)
grid.arrange(plot_t_1, plot_t_2, ncol = 1)
dev.off()

# Plot 12: prey density data -------------------------------------------------------

plot_data = bind_rows(plot_data_9a)
plot_data$variable = factor(plot_data$variable, levels = c("euphausiids",
                                                           "neocalanusShelf", 
                                                           "neocalanus", "copepods"))
plot_data$variable2 = factor(plot_data$variable, labels = c("Eup~(mg~C/m^3)",
                                                            "NCaS~(mg~C/m^3)", 
                                                            "NCaO~(mg~C/m^3)", "Cop~(mg~C/m^3)"))
plot_data$state = ifelse(test = plot_data$state == 'alive', yes = 'Surviving', no = 'Dead')

plot_env_2 = ggplot(plot_data, aes(x = factor(year), color = factor(state), 
                                   fill = factor(state))) + 
  geom_boxplot(aes(y = value), alpha=0.3, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) +
  theme(legend.position = c(0.1, 0.4), legend.background =element_blank()) +
  scale_color_brewer(palette = color_pal) +
  scale_fill_brewer(palette = color_pal) +
  guides(fill=guide_legend(title=NULL),
         color=guide_legend(title=NULL)) +
  facet_wrap(. ~ variable2, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed, ncol = 2) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")


# Make plot:
png(filename = 'figures/hind_preydensity.png', width = 190, height = 120, 
    units = 'mm', res = 500)
print(plot_env_2)
dev.off()

# By release date:
plot_data$relday = factor(plot_data$relday, levels = paste0(relday_vec, '-3'))
plot_data$relday = factor(plot_data$relday, labels = relday_vec)

plot_env_2b = ggplot(plot_data, aes(x = relday, color = factor(state), 
                                    fill = factor(state))) + 
  geom_boxplot(aes(y = value), alpha=0.3, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = c(0.1, 0.4), legend.background =element_blank()) +
  guides(fill=guide_legend(title=NULL),
         color=guide_legend(title=NULL)) +
  facet_wrap(. ~ variable2, scales = 'free_y', strip.position = "left", 
             labeller = label_parsed, ncol = 2) +
  theme(strip.background = element_blank(),
        strip.placement = "outside")

# Make plot:
png(filename = 'figures/hind_preydensity_relday.png', width = 190, height = 120, 
    units = 'mm', res = 500)
print(plot_env_2b)
dev.off()


# Relationship between env variables with recruitment estimates -----------
plot_dataT = bind_rows(plot_data_8a)
mean_dfT = plot_dataT %>% group_by(year) %>% summarise(meanValue = mean(value))
plot_data = bind_rows(plot_data_9a)
mean_dfP = plot_data %>% group_by(year, variable) %>% summarise(meanValue = mean(value))
mean_dfP = tidyr::spread(mean_dfP, variable, meanValue)

rec_estimates = read.csv('Compare_Recs_Surv.csv')
rec_estimates = rec_estimates[rec_estimates$year <= 2020 & rec_estimates$year >= 2000 & rec_estimates$scenario == 'sage0_mod', ]
rec_estimates$temperature = mean_dfT$meanValue
rec_estimates$Cop = mean_dfP$copepods
rec_estimates$NCaS = mean_dfP$neocalanusShelf
rec_estimates$NCaO = mean_dfP$neocalanus
rec_estimates$Eup = mean_dfP$euphausiids

#Standardize:
rec_estimates = rec_estimates %>% dplyr::mutate(dplyr::across(p_index:Eup, BBmisc::normalize))
cor_df = rec_estimates %>% dplyr::select(p_index:Eup)
colnames(cor_df) = c('Recruit', 'Temp', 'Cop', 'NCaS', 'NCaO', 'Eup')

# Make correlation plot:
corr <- round(cor(cor_df), 1) # Pearson correlation coefficient
p.mat <- ggcorrplot::cor_pmat(cor_df) # pvalues
# Make plot: only plot significant squares:
png(filename = 'figures/hind_corr_recenv.png', width = 190, height = 160, units = 'mm', res = 500)
print(ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
           type = "lower", insig = "blank", lab = TRUE))
dev.off()

# Make plot correlation Rec and Temp:
ggplot(cor_df, aes(x = Temp, y = Recruit)) +
  geom_point() +
  theme_bw() +
  ylab('SAM recruitment estimates') +
  xlab('Average temperature experienced by fish') +
  geom_smooth(method='lm', formula= y~x)
ggsave(filename = 'figures/hind_cor_recr_temp.png', width = 90, height = 80, dpi = 500, units = 'mm')

# Plot map anomalies ----------------------------------------------------------

# Hatching success
plot_data = bind_rows(plot_data_1b)
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[4] = 'var'

anom1 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = 'Hatch success')

# Survival 100 dph
plot_data = bind_rows(plot_data_2b)
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[4] = 'var'
plot_data$var = plot_data$var*1e-06
plot_data$var = log(plot_data$var+1)

anom2 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = 'Survival probability')

# SL
plot_data = bind_rows(plot_data_4b)
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[4] = 'var'

anom3 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = 'Standard length (mm)')

# Growth performance
plot_data = bind_rows(plot_data_5b)
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[4] = 'var'

anom4 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = 'Growth performance')


# Make plot:
png(filename = 'figures/hind_mapanom_biovar.png', width = 190, height = 130, units = 'mm', res = 500)
grid.arrange(anom1, anom3, anom2, anom4, ncol = 2)
dev.off()

# ----

# Temperature
plot_data = bind_rows(plot_data_8b)
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[5] = 'var'

anom5 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = '')

# Make plot:
png(filename = 'figures/hind_temp.png', width = 95, height = 150, units = 'mm', res = 500)
grid.arrange(plot_temp_1, anom5, ncol = 1)
dev.off()


# ----

# Cop:
plot_data = bind_rows(plot_data_9b)
plot_data = plot_data[plot_data$variable == 'copepods', ]
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[5] = 'var'

anom6 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = 'Cop')

# NCaO:
plot_data = bind_rows(plot_data_9b)
plot_data = plot_data[plot_data$variable == 'neocalanus', ]
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[5] = 'var'

anom7 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = 'NCaO')

# NCaS:
plot_data = bind_rows(plot_data_9b)
plot_data = plot_data[plot_data$variable == 'neocalanusShelf', ]
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[5] = 'var'

anom8 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = 'NCaS')

# Eup:
plot_data = bind_rows(plot_data_9b)
plot_data = plot_data[plot_data$variable == 'euphausiids', ]
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
colnames(plot_data)[5] = 'var'

anom9 = plot_map_var(plot_data = plot_data, legTitle = '', mainTitle = 'Eup')

#Make plot:
png(filename = 'figures/hind_mapanom_prey.png', width = 190, height = 130, units = 'mm', res = 500)
grid.arrange(anom9, anom8, anom7, anom6, ncol = 2)
dev.off()


# Map ind dead  -----------------------------------------------------------
n_years = length(allYears)

# Starvation:
plot_data = bind_rows(plot_data_0a)
plot_data = plot_data %>%
              count(id)
plot_data$id_grid = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(n = plot_data$n), list(id_grid = plot_data$id_grid), mean)
plot_data$var = plot_data$n/n_years
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data = left_join(baseLocs2, plot_data, by = c('id_grid', 'lon', 'lat'))
plot_data$var[which(is.na(plot_data$var))] = 0
plot_data$var = plot_data$var*100

mapdead1 = plot_map_var2(plot_data = plot_data, legTitle = '', mainTitle = '% survived starvation')

# Out of EBS:
plot_data = bind_rows(plot_data_0c)
plot_data = plot_data %>%
  count(id)
plot_data$id_grid = baseLocs$id_grid[match(plot_data$id, baseLocs$id)] # replace id column
plot_data = aggregate(list(n = plot_data$n), list(id_grid = plot_data$id_grid), mean)
plot_data$var = plot_data$n/n_years
plot_data$lon = baseLocs2$lon[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data$lat = baseLocs2$lat[match(plot_data$id_grid, baseLocs2$id_grid)]
plot_data = left_join(baseLocs2, plot_data, by = c('id_grid', 'lon', 'lat'))
plot_data$var[which(is.na(plot_data$var))] = 0
plot_data$var = plot_data$var*100

mapdead2 = plot_map_var2(plot_data = plot_data, legTitle = '', mainTitle = '% remained in the EBS')

# Make plot:
lay = matrix(c(1,1,2,3), nrow = 2)
png(filename = 'figures/hind_dead.png', width = 190, height = 130, units = 'mm', res = 500)
grid.arrange(plot_5, mapdead1, mapdead2, layout_matrix = lay)
dev.off()


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Information to map
shift_value_1 = 0
shift_value_2 = 360

map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
  dplyr::filter(region != "Antarctica")

country_shapes <-  geom_polygon(data = map_world_df, 
                                aes(x=long, y = lat, group = group),
                                fill = "gainsboro",
                                color = "gainsboro",
                                size = 0.15)

# -------------------------------------------------------------------------
# Plot trajectories:
# We need the raw DisMELS outputs to run this plot:

# List to save plots:
plotList = list()
indList = 1
cores = list.files(path = main_folder)

for(i in seq_along(cores)) {

  core_name = cores[i]

  tmpData = read_data_in(eggInclude = FALSE,
                         path = file.path(main_folder, core_name))
  tmpData$horizPos1 = ifelse(test = tmpData$horizPos1 > 0, yes = tmpData$horizPos1 - 360,
                             no = tmpData$horizPos1)
  tmpData$horizPos1 = tmpData$horizPos1 + 360
  tmpData$relDay = lubridate::day(tmpData$startTime)

  # Find initial and final points
  init_points = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]

  plotList[[i]] = plot_trajectory(tmpData)
  print(i)
  
}

png(filename = 'figures/hind_trajectories.png', width = 190, height = 110, units = 'mm', res = 500)
do.call("grid.arrange", c(plotList, ncol = 6, nrow = 4))
dev.off()


# Plot density of final locations -----------------------------------------
plot_data = bind_rows(plot_data_14)
plot_data$decade = as.factor(plot_data$year)

density_map = plot_map_2d_density2(plot_data = plot_data, nCol = 6)

# Make plot:
png(filename = 'figures/hind_2d_density.png', width = 190, height = 140, units = 'mm', res = 500)
print(density_map)
dev.off()


# -------------------------------------------------------------------------
# Plot initial locations:
plot_initial_locations(initData = baseLocs)
ggsave(filename = 'figures/initLocations.jpg', width = 85, height = 60, units = 'mm', dpi = 500)

