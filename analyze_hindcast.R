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

# Read output files -------------------------------------------------------
main_folder = 'E:/DisMELS_save_outputs/save_hindcast' # directory where the DisMELS outputs are
# DisMELS outputs not provided due to file size (too big)
save_folder = 'output_data/hindcast' # directory where to save the processed data for plotting


# Process DisMELS outputs (only run it once) -------------------------------------------------
plot_data_0a = list()
plot_data_0b = list()
plot_data_0c = list()
plot_data_0d = list()
plot_data_0e = list()
plot_data_0f = list()
plot_data_0g = list()
plot_data_1a = list()
plot_data_1b = list()
plot_data_1c = list()
plot_data_1d = list()
plot_data_2a = list()
plot_data_2b = list()
plot_data_2c = list()
plot_data_2d = list()
plot_data_2e = list()
plot_data_3a = list()
plot_data_3b = list()
plot_data_3c = list()
plot_data_4a = list()
plot_data_4b = list()
plot_data_4c = list()
plot_data_4d = list()
plot_data_4e = list()
plot_data_5a = list()
plot_data_5b = list()
plot_data_5c = list()
plot_data_5d = list()
plot_data_5e = list()
plot_data_6 = list()
plot_data_6b = list()
plot_data_6_5 = list()
plot_data_7 = list()
plot_data_8a = list()
plot_data_8b = list()
plot_data_8c = list()
plot_data_9a = list()
plot_data_9b = list()
plot_data_9c = list()
plot_data_10a = list()
plot_data_10b = list()
plot_data_11a = list()
plot_data_11b = list()
plot_data_12 = list()
plot_data_13 = list()
plot_data_14 = list()

indList = 1
mod_year = list.files(path = file.path(main_folder))
for(j in seq_along(mod_year)) {
    
    # Read all results CSV:
    tmpData = read_data_in(eggInclude = FALSE, 
                           path = file.path(main_folder, mod_year[j]))
    tmpData$ageYSLround = round(tmpData$ageFromYSL)
    tmpData$md_inx = lubridate::month(x = tmpData$time) + (lubridate::day(x = tmpData$time)/31)
    tmpData$day_month = paste0(lubridate::day(x = tmpData$time), '_', lubridate::month(x = tmpData$time))
    tmpData = tmpData[tmpData$md_inx < 9.5, ] # Max date: Sep 15th
    this_year = unique(tmpData$year)
    
    # Incorporate rel_number:
    tmpData$dm_init_date = paste0(lubridate::day(x = tmpData$startTime), '-', 
                                  lubridate::month(x = tmpData$startTime))    
    
    # Base initial points: (all fish) Just do it once
    if(j == 1) {
      base_locs = tmpData[tmpData[ , .I[which.min(time)], by = id]$V1]
      base_locs$horizPos1 = ifelse(test = base_locs$horizPos1 > 0, yes = base_locs$horizPos1 - 360, 
                                   no = base_locs$horizPos1)
      base_locs$horizPos1 = base_locs$horizPos1 + 360
      base_locs = base_locs[order(base_locs$id), ]
      baseLocs = base_locs[,c('id', 'horizPos1', 'horizPos2', 'vertPos')]
      baseLocs$id_grid = rep(x = 1:n_grids, times = n_releases)
      baseLocs2 = aggregate(list(lon = baseLocs$horizPos1, lat = baseLocs$horizPos2, 
                                 depth = baseLocs$vertPos), 
                            list(id_grid = baseLocs$id_grid), unique)
      base_wgt = aggregate(list(rel_date = tmpData$dm_init_date), 
                           list(id = tmpData$id), unique)
    }
    
    # Section 0 ----------
    # Subset only fish passed to FDL
    fdl_in = unique(tmpData[tmpData$typeName == 'FDL', 'id'])
    dw_out = tmpData[which(tmpData$mortstarv >= 1000),]
    dw_in = base_locs$id[!(base_locs$id %in% unique(dw_out$id))]    
    
    crit_period = data.frame(year = this_year, id = fdl_in$id)
    cond_fact = data.frame(year = this_year, id = dw_in)
    
    plot_data_0f[[indList]] = crit_period
    plot_data_0g[[indList]] = cond_fact
      
    starv_alive = data.frame(year = this_year, id = intersect(fdl_in$id, dw_in))
    starv_dead = data.frame(year = this_year, id = base_wgt$id[!(base_wgt$id %in% starv_alive$id)])
    starv_alive$rel_date = base_wgt$rel_date[match(starv_alive$id, base_wgt$id)]
    starv_dead$rel_date = base_wgt$rel_date[match(starv_dead$id, base_wgt$id)]
    starv_alive$id_grid = baseLocs$id_grid[match(starv_alive$id, baseLocs$id)]
    starv_dead$id_grid = baseLocs$id_grid[match(starv_dead$id, baseLocs$id)]
    
    plot_data_0a[[indList]] = starv_alive
    plot_data_0b[[indList]] = starv_dead
    tmpData2 = tmpData[tmpData$id %in% starv_alive$id, ] # exclude them
    tmpData3 = tmpData2 # future analysis
    
    # Subset only fish that end up within the EBS:
    end_ind = tmpData[tmpData[ , .I[which.max(time)], by = id]$V1]
    in_data = data.frame(year = this_year, id = end_ind$id[which(end_ind$horizPos1 < 0)])
    out_data = data.frame(year = this_year, id = end_ind$id[which(end_ind$horizPos1 > 0)])
    in_data$rel_date = base_wgt$rel_date[match(in_data$id, base_wgt$id)]
    in_data$id_grid = baseLocs$id_grid[match(in_data$id, baseLocs$id)]
    out_data$rel_date = base_wgt$rel_date[match(out_data$id, base_wgt$id)]
    out_data$id_grid = baseLocs$id_grid[match(out_data$id, baseLocs$id)]
    
    plot_data_0c[[indList]] = in_data
    tmpData2 = tmpData2[tmpData2$id %in% in_data$id, ] # exclude them
    # tmp_dead_ebs = tmpData[!(tmpData$id %in% in_data$id), ]
    
    # Number of fish alive by Sep 15th:
    alive_id = unique(tmpData2$id)
    alive_data = data.frame(year = this_year, id = alive_id)
    alive_data$rel_date = base_wgt$rel_date[match(alive_data$id, base_wgt$id)]
    alive_data$id_grid = baseLocs$id_grid[match(alive_data$id, baseLocs$id)]
    
    # Number of fish dead by Sep 15th:
    dead_id = unique(c(out_data$id, starv_dead$id))
    dead_data = data.frame(year = this_year, id = dead_id)
    dead_data$rel_date = base_wgt$rel_date[match(dead_data$id, base_wgt$id)]
    dead_data$id_grid = baseLocs$id_grid[match(dead_data$id, baseLocs$id)]
    
    plot_data_0d[[indList]] = alive_data
    plot_data_0e[[indList]] = dead_data
    # tmp_dead_all = tmpData[!(tmpData$id %in% alive_data$id), ]
    
    # Find initial and final points (only alive id + out of EBS)
    init_points = tmpData3[tmpData3[ , .I[which.min(time)], by = id]$V1]
    end_points = tmpData3[tmpData3[ , .I[which.max(time)], by = id]$V1]
    
    # Section 1 ----------
    # Hatching success
    tmp_data = tmpData2[tmpData2[ , .I[which.min(time)], by = id]$V1]
    num_dataYSL = tmp_data[,c('year', 'id', 'number')]
    num_dataYSL$init_number = 1E+06
    num_dataYSL$hatsuc = num_dataYSL$number/num_dataYSL$init_number
    # Prepare data to save:
    sel_var = 'hatsuc'
    toPlotData = num_dataYSL
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_1d[[indList]] = toPlotData
    toPlotData = as.data.frame(toPlotData)
    prevAnom = toPlotData[,c('year', 'id_grid', 'relday', sel_var)]
    colnames(prevAnom)[4] = 'value'
    prevAnom2 = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value)) # mean over grids
    prevAnom2$year = this_year
    plot_data_1a[[indList]] = prevAnom2
    prevAnom2$value2 = prevAnom2$value - median(prevAnom2$value) # median?
    plot_data_1b[[indList]] = prevAnom2  # to plot spatial anomalies
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], prob = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5)
    plot_data_1c[[indList]] = fdata
    
    # Section 2 ----------
    # Survival by Sep 15th 
    #tmp_data = tmpData2[tmpData2$ageYSLround == 100, ]
    tmp_data = tmpData2[tmpData2[ , .I[which.max(time)], by = id]$V1]
    survData = aggregate(x = list(psurv = tmp_data$psurvival), list(year = tmp_data$year, 
                                                                    id = tmp_data$id),
                         FUN = mean, na.rm = TRUE)
    # survData$psurv = survData$psurv # 1+06
    surv_data = survData[,c('year', 'id', 'psurv')]
    # Prepare data to save:
    sel_var = 'psurv'
    toPlotData = surv_data
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_2d[[indList]] = toPlotData
    toPlotData = as.data.frame(toPlotData)
    prevAnom = toPlotData[,c('year', 'id_grid', 'relday', sel_var)]
    colnames(prevAnom)[4] = 'value'
    prevAnom2 = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value)) # mean over grids
    prevAnom2$year = this_year
    plot_data_2a[[indList]] = prevAnom2
    prevAnom2$value2 = prevAnom2$value - median(prevAnom2$value) # median?
    plot_data_2b[[indList]] = prevAnom2  # to plot spatial anomalies
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], prob = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5)
    plot_data_2c[[indList]] = fdata
    
   
    # Section 4 ----------
    # Standard length
    sl_data = aggregate(x = list(SL = tmpData2$SL), list(year = tmpData2$year, id = tmpData2$id), 
                        FUN = max, na.rm=TRUE)
    # Prepare data to save:
    sel_var = 'SL'
    toPlotData = sl_data
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_4d[[indList]] = toPlotData
    toPlotData = as.data.frame(toPlotData)
    prevAnom = toPlotData[,c('year', 'id_grid', 'relday', sel_var)]
    colnames(prevAnom)[4] = 'value'
    prevAnom2 = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value)) # mean over grids
    prevAnom2$year = this_year
    plot_data_4a[[indList]] = prevAnom2
    prevAnom2$value2 = prevAnom2$value - median(prevAnom2$value) # median?
    plot_data_4b[[indList]] = prevAnom2  # to plot spatial anomalies
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], prob = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5)
    plot_data_4c[[indList]] = fdata

    # Section 5 ----------
    # DW/DWmax : growth performance
    tmp_data = tmpData2[tmpData2[ , .I[which.max(time)], by = id]$V1]
    tmp_data$Gperf = tmp_data$DW/tmp_data$dwmax
    this_data = tmp_data[,c('year', 'id', 'Gperf')]
    # Prepare data to save:
    sel_var = 'Gperf'
    toPlotData = this_data
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_5d[[indList]] = toPlotData
    toPlotData = as.data.frame(toPlotData)
    prevAnom = toPlotData[,c('year', 'id_grid', 'relday', sel_var)]
    colnames(prevAnom)[4] = 'value'
    prevAnom2 = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value)) # mean over grids
    prevAnom2$year = this_year
    plot_data_5a[[indList]] = prevAnom2
    prevAnom2$value2 = prevAnom2$value - median(prevAnom2$value) # median?
    plot_data_5b[[indList]] = prevAnom2  # to plot spatial anomalies
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], prob = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5)
    plot_data_5c[[indList]] = fdata
    
    
    # Section 2.1 ----------
    # Survival by 100 dph
    data_100dph = tmpData2[tmpData2$ageYSLround == 100, ]
    survData = aggregate(x = list(psurv = data_100dph$psurvival), list(year = data_100dph$year, 
                                                                       id = data_100dph$id),
                         FUN = mean, na.rm = TRUE)
    surv_data = survData[,c('year', 'id', 'psurv')]
    # Prepare data to save:
    sel_var = 'psurv'
    toPlotData = surv_data
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_2e[[indList]] = toPlotData
    
    # Section 4.1 ----------
    # SL by 100 dph
    sl_data_100dph = aggregate(x = list(SL = data_100dph$SL), 
                               list(year = data_100dph$year, id = data_100dph$id), 
                        FUN = max, na.rm=TRUE)
    # Prepare data to save:
    sel_var = 'SL'
    toPlotData = sl_data_100dph
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_4e[[indList]] = toPlotData

    # Section 5.1 ----------
    # Gperf by 100 dph
    data_100dph$Gperf = data_100dph$DW/data_100dph$dwmax
    dw_data_100dph = aggregate(x = list(Gperf = data_100dph$Gperf), 
                               list(year = data_100dph$year, id = data_100dph$id), 
                               FUN = mean, na.rm=TRUE)
    # Prepare data to save:
    sel_var = 'Gperf'
    toPlotData = dw_data_100dph
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_5e[[indList]] = toPlotData
    

    # Section 6 ----------
    # Mortality: 100 dph
    #myData = tmpData2[(tmpData2$ageYSLround <= 100), ]
    myData = tmpData2
    myData2 = myData[(myData$progYSA >= 1 | is.na(myData$progYSA)), ]
    #Calculate fish predator + invertebrate mortality
    myData3 = aggregate(list(mortfish = myData$mortfish, mortinv = myData$mortinv), 
                        list(year = myData$year, id = myData$id), FUN = median) # median?
    myData4 = myData3 %>% 
      gather('type', 'value', -c(year, id))
    #Find prop starvation:
    myData5 = aggregate(list(value = myData2$mortstarv), 
                        list(year = myData2$year, id = myData2$id), FUN = function(x) sum(x>0)/length(x))
    myData5$type = 'mortstarv'
    myData5$value = myData5$value*100 # in %
    # Merge both data:
    myData6 = bind_rows(myData4, myData5)
    plot_data_6b[[indList]] = myData6
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = myData6
    q50 = toPlotData %>%
      dplyr::group_by(type) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = 0.5))
    p2_5 = toPlotData %>%
      dplyr::group_by(type) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thLow1))
    p97_5 = toPlotData %>%
      dplyr::group_by(type) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thHigh1))
    fdata = data.frame(year = this_year, type = q50$type, q50 = q50$quant, 
                       q5 = p2_5$quant, q95 = p97_5$quant)
    plot_data_6[[indList]] = fdata

    
    # Section 6.5 --------
    # Analyze stomach fullness state:
    stomachInfo = tmpData2 %>%
                dplyr::group_by(id) %>% 
                dplyr::summarise(quant = mean(x = stomachFullness))
    stomachInfo$relday = base_wgt$rel_date[match(stomachInfo$id, base_wgt$id)]
    stomachInfo$id_grid = baseLocs$id_grid[match(stomachInfo$id, baseLocs$id)]
    # mean_val = mean( x = tmpData2$stomachFullness)
    # q50 = quantile( x = tmpData2$stomachFullness, probs = 0.5)
    # p2_5 = quantile( x = tmpData2$stomachFullness, probs = thLow1)
    # p97_5 = quantile( x = tmpData2$stomachFullness, probs = thHigh1)
    stomachInfo$year = this_year
    plot_data_6_5[[indList]] = stomachInfo  
    
    
    # Section 8 ----------
    # Environmental variables (alive + dead)
    stageData = tmpData
    env_data = aggregate(x = list(value = stageData$temp), 
                         list(year = stageData$year, id = stageData$id), 
                         FUN = mean, na.rm=TRUE) # mean values
    env_data$variable = 'temperature'
    env_data$state = 'dead'
    env_data$state[env_data$id %in% alive_data$id] = 'alive'
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = env_data
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_8a[[indList]] = toPlotData
    prevAnom = data.frame(toPlotData[,c('id_grid', 'variable', 'state', 'relday', sel_var)])
    #prevAnom = prevAnom[prevAnom$state == 'alive', ] # only alive ID
    prevAnom = prevAnom %>%
      dplyr::group_by(id_grid, variable) %>% 
      dplyr::summarise(value = mean(value), .groups = 'drop')
    prevAnom$year = this_year
    prevAnom$value2 = prevAnom$value - median(prevAnom$value)
    plot_data_8b[[indList]] = prevAnom
    q50 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = 0.5))
    q2_5 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thLow1))
    q97_5 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thHigh1))
    fdata = data.frame(year = this_year, variable = q50$variable, q50 = q50$quant, 
                       q5 = q2_5$quant, q95 = q97_5$quant)
    plot_data_8c[[indList]] = fdata
    
    # Section 9 ----------
    # Prey density field
    prey_data = aggregate(x = list(copepods = stageData$copepod,
                                   neocalanus = stageData$neocalanus, 
                                   neocalanusShelf = stageData$neocalanusShelf, 
                                   euphausiids = stageData$euphausiid + stageData$euphausiidShelf), 
                          list(year = stageData$year, id = stageData$id), 
                          FUN = mean, na.rm=TRUE)
    int_data = gather(prey_data, key = "variable", value = "value",
                      copepods, neocalanus, neocalanusShelf, euphausiids)
    int_data$state = 'dead'
    int_data$state[int_data$id %in% alive_data$id] = 'alive'
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = int_data
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_9a[[indList]] = toPlotData
    prevAnom = data.frame(toPlotData[,c('year', 'id_grid', 'variable', 'state', 'relday', sel_var)])
    #prevAnom = prevAnom[prevAnom$state == 'alive', ] # only alive ID
    prevAnom = prevAnom %>%
      dplyr::group_by(id_grid, variable) %>% 
      dplyr::summarise(value = mean(value), .groups = 'drop')
    prevAnom$year = this_year
    prevAnom$value2 = prevAnom$value - ave(prevAnom$value, prevAnom$variable, FUN  = median)
    plot_data_9b[[indList]] = prevAnom
    q50 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = 0.5))
    q2_5 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thLow1))
    q97_5 = toPlotData %>%
      dplyr::group_by(variable) %>% 
      dplyr::summarise(quant = quantile(x = value, probs = thHigh1))
    fdata = data.frame(year = this_year, variable = q50$variable, q50 = q50$quant, 
                       q5 = q2_5$quant, q95 = q97_5$quant)
    plot_data_9c[[indList]] = fdata
    
    
    # Section 10 ----------
    # Diet species (only alive individuals)
    # rank_data = aggregate(x = list(rank = tmpData2$avgRank), 
    #                       list(year = tmpData2$year, stage = tmpData2$typeName, id = tmpData2$id), 
    #                       FUN = mean, na.rm=TRUE)
    # rank_data = rank_data[!(rank_data$stage %in% c('YSL')), ] # exclude YSL
    # # Prepare data to save:
    # sel_var = 'rank'
    # toPlotData = rank_data
    # p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                 FUN = median)
    # p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                  FUN = quantile, probs = thLow1, na.rm = TRUE)
    # p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                   FUN = quantile, probs = thHigh1, na.rm = TRUE)
    # p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                 FUN = quantile, probs = thLow2, na.rm = TRUE)
    # p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                 FUN = quantile, probs = thHigh2, na.rm = TRUE)
    # fdata = data.frame(year = p50$year, stage = p50$stage, q50 = p50$var, q25 = p25$var, q75 = p75$var,
    #                    q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
    # plot_data_10a[[indList]] = fdata
    # 
    # # Diet size
    # size_data = aggregate(x = list(sized = tmpData2$avgSize), list(year = tmpData2$year, stage = 
    #                                                                 tmpData2$typeName, id = tmpData2$id), 
    #                       FUN = mean, na.rm=TRUE)
    # size_data = size_data[!(size_data$stage %in% c('YSL')), ] # exclude YSL and benthicjuv stage
    # # Prepare data to save:
    # sel_var = 'sized'
    # toPlotData = size_data
    # p50 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                 FUN = median)
    # p2_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                  FUN = quantile, probs = thLow1, na.rm = TRUE)
    # p97_5 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                   FUN = quantile, probs = thHigh1, na.rm = TRUE)
    # p25 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                 FUN = quantile, probs = thLow2, na.rm = TRUE)
    # p75 = aggregate(list(var = toPlotData[,sel_var]), list(year = toPlotData$year, stage = toPlotData$stage), 
    #                 FUN = quantile, probs = thHigh2, na.rm = TRUE)
    # fdata = data.frame(year = p50$year, stage = p50$stage, q50 = p50$var, q25 = p25$var, q75 = p75$var,
    #                    q5 = p2_5$var, q95 = p97_5$var, scenario = scenarios[k])
    # plot_data_10b[[indList]] = fdata

    
    
    # Section 11 -----------
    # Calculate distance per id:
    mergePoints = rbind(init_points[,c('horizPos1', 'horizPos2', 'id')],
                        end_points[,c('horizPos1', 'horizPos2', 'id')])
    distMat = mergePoints %>%
      dplyr::group_by(id)%>%
      dplyr::group_map(~raster::pointDistance(.x[,c('horizPos1', 'horizPos2')], lonlat=TRUE)) %>%
      setNames(unique(sort(mergePoints$id)))
    distVals = unlist(distMat)
    distVals = distVals[distVals > 0 & !is.na(distVals)]
    distValsNm = (distVals/111000)*60 # units: nm
    preDist = data.frame(dist = distValsNm, year = this_year)
    preDist$id = as.numeric(names(distMat))
    # Prepare data to save:
    sel_var = 'dist'
    toPlotData = preDist
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    prevAnom = data.frame(toPlotData[,c('year', 'id_grid', 'relday', sel_var)])
    colnames(prevAnom)[4] = 'value'
    prevAnom = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value))
    prevAnom$year = this_year
    p50 = quantile(x = toPlotData[,sel_var], probs = 0.5)
    p2_5 = quantile(x = toPlotData[,sel_var], probs = thLow1)
    p97_5 = quantile(x = toPlotData[,sel_var], probs = thHigh1)
    fdata = data.frame(year = this_year, q50 = p50, 
                       q5 = p2_5, q95 = p97_5)
    plot_data_11a[[indList]] = fdata
    plot_data_11b[[indList]] = prevAnom
    
    # Section 12 -----------
    # Calculate direction
    direcMat = mergePoints %>%
      dplyr::group_by(id)%>%
      dplyr::group_map(~geosphere::bearing(.x[,c('horizPos1', 'horizPos2')])) %>%
      setNames(unique(sort(mergePoints$id)))
    direcVals = unlist(direcMat)
    direcVals = direcVals[!is.na(direcVals)]
    direcValsNm = ifelse(test = direcVals < 0, yes = 360+direcVals, no = direcVals)
    preDirec = data.frame(direc = direcValsNm, year = unique(tmpData$year)[1])
    preDirec$id = as.numeric(names(direcMat))
    sel_var = 'direc'
    toPlotData = preDirec
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    prevAnom = data.frame(toPlotData[,c('year', 'id_grid', 'relday', sel_var)])
    colnames(prevAnom)[4] = 'value'
    prevAnom = prevAnom %>%
      dplyr::group_by(id_grid) %>% 
      dplyr::summarise(value = mean(value))
    prevAnom$year = this_year
    plot_data_12[[indList]] = prevAnom
    
    # Section 13 -----------
    # Calculate CG and Inertia:
    end_points$horizPos1 = ifelse(test = end_points$horizPos1 > 0, yes = end_points$horizPos1 - 360, 
                                  no = end_points$horizPos1)
    end_points$horizPos1 = end_points$horizPos1 + 360
    
    spatInfoEnd = cgi(x = end_points$horizPos1, y = end_points$horizPos2)
    plot_data_13[[indList]] = data.frame(CG_x_end = spatInfoEnd$xcg,
                                         CG_y_end = spatInfoEnd$ycg,
                                         I_end = spatInfoEnd$I,
                                         year = this_year)
    # Section 14 -----------
    # final locations to plot density
    endLocs_df = end_points[,c('id', 'horizPos1', 'horizPos2', 'year')] 
    plot_data_14[[indList]] = endLocs_df  
    
    # Get to next indicator:
    print(indList)
    indList = indList + 1
    
}
    
# Save processed data:
save(plot_data_0a, file = file.path(save_folder, 'plot_data_0a.RData'))
save(plot_data_0b, file = file.path(save_folder, 'plot_data_0b.RData'))
save(plot_data_0c, file = file.path(save_folder, 'plot_data_0c.RData'))
save(plot_data_0d, file = file.path(save_folder, 'plot_data_0d.RData'))
save(plot_data_0e, file = file.path(save_folder, 'plot_data_0e.RData'))
save(plot_data_1a, file = file.path(save_folder, 'plot_data_1a.RData'))
save(plot_data_1b, file = file.path(save_folder, 'plot_data_1b.RData'))
save(plot_data_1c, file = file.path(save_folder, 'plot_data_1c.RData'))
save(plot_data_1d, file = file.path(save_folder, 'plot_data_1d.RData'))
save(plot_data_2a, file = file.path(save_folder, 'plot_data_2a.RData'))
save(plot_data_2b, file = file.path(save_folder, 'plot_data_2b.RData'))
save(plot_data_2c, file = file.path(save_folder, 'plot_data_2c.RData'))
save(plot_data_2d, file = file.path(save_folder, 'plot_data_2d.RData'))
save(plot_data_2e, file = file.path(save_folder, 'plot_data_2e.RData'))
save(plot_data_4a, file = file.path(save_folder, 'plot_data_4a.RData'))
save(plot_data_4b, file = file.path(save_folder, 'plot_data_4b.RData'))
save(plot_data_4c, file = file.path(save_folder, 'plot_data_4c.RData'))
save(plot_data_4d, file = file.path(save_folder, 'plot_data_4d.RData'))
save(plot_data_4e, file = file.path(save_folder, 'plot_data_4e.RData'))
save(plot_data_5a, file = file.path(save_folder, 'plot_data_5a.RData'))
save(plot_data_5b, file = file.path(save_folder, 'plot_data_5b.RData'))
save(plot_data_5c, file = file.path(save_folder, 'plot_data_5c.RData'))
save(plot_data_5d, file = file.path(save_folder, 'plot_data_5d.RData'))
save(plot_data_5e, file = file.path(save_folder, 'plot_data_5e.RData'))
save(plot_data_6, file = file.path(save_folder, 'plot_data_6.RData'))
save(plot_data_6b, file = file.path(save_folder, 'plot_data_6b.RData'))
save(plot_data_6_5, file = file.path(save_folder, 'plot_data_6_5.RData'))
save(plot_data_8a, file = file.path(save_folder, 'plot_data_8a.RData'))
save(plot_data_8b, file = file.path(save_folder, 'plot_data_8b.RData'))
save(plot_data_8c, file = file.path(save_folder, 'plot_data_8c.RData'))
save(plot_data_9a, file = file.path(save_folder, 'plot_data_9a.RData'))
save(plot_data_9b, file = file.path(save_folder, 'plot_data_9b.RData'))
save(plot_data_9c, file = file.path(save_folder, 'plot_data_9c.RData'))
save(plot_data_10a, file = file.path(save_folder, 'plot_data_10a.RData'))
save(plot_data_10b, file = file.path(save_folder, 'plot_data_10b.RData'))
save(plot_data_11a, file = file.path(save_folder, 'plot_data_11a.RData'))
save(plot_data_11b, file = file.path(save_folder, 'plot_data_11b.RData'))
save(plot_data_12, file = file.path(save_folder, 'plot_data_12.RData'))
save(plot_data_13, file = file.path(save_folder, 'plot_data_13.RData'))
save(plot_data_14, file = file.path(save_folder, 'plot_data_14.RData'))
save(baseLocs, file = file.path(save_folder, 'baseLocs.RData'))
save(baseLocs2, file = file.path(save_folder, 'baseLocs2.RData'))
save(base_wgt, file = file.path(save_folder, 'base_wgt.RData'))


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
load(file = file.path(save_folder, 'baseLocs.RData'))
load(file = file.path(save_folder, 'baseLocs2.RData'))
load(file = file.path(save_folder, 'base_wgt.RData'))

# Analyze results: temporal series ---------------------------------------------------------
# -------------------------------------------------------------------------

# Hatching success + surv prob (Sep 15th) + SL (Sep 15th) ------------------------------

plot_data_1 = bind_rows(plot_data_1d)

plot_1 = ggplot(plot_data_1, aes(x = factor(year))) + 
  geom_boxplot(aes(y = hatsuc), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Hatch success') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) 

plot_data_2 = bind_rows(plot_data_2d)

plot_2 = ggplot(plot_data_2, aes(x = factor(year))) + 
  geom_boxplot(aes(y = psurv*1e-06), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Survival probability') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) 

plot_data_3 = bind_rows(plot_data_4d)

plot_3 = ggplot(plot_data_3, aes(x = factor(year))) + 
  geom_boxplot(aes(y = SL), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Standard length (mm)') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) 

plot_data_4 = bind_rows(plot_data_5d)

plot_4 = ggplot(plot_data_4, aes(x = factor(year))) + 
  geom_boxplot(aes(y = Gperf), alpha=alphaLevel, color = mainCols[1], 
               fill = mainCols[2], outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Growth performance') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) 

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

# Plot:
slobs = ggplot(sl_obs, aes(x = year)) + 
  geom_line(aes(y = value), colour = 'black') +
  theme_bw() +
  xlab(NULL) +
  ylab('Mean length-at-age 0.5 (cm)') +
  scale_x_continuous(expand=c(0, 0), breaks = seq(from = min_plot_year, to = max_plot_year, by = by_plot_year)) 

# Merge plots:
lay = matrix(c(1,2), nrow = 2)
png(filename = 'figures/hind_SL_compare.png', width = 95, height = 150, 
    units = 'mm', res = 500)
grid.arrange(plot_3, slobs, layout_matrix = lay)
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

plot_dataT = bind_rows(plot_data_8a)
plot_dataT$state = ifelse(test = plot_dataT$state == 'alive', yes = 'Surviving', no = 'Dead')

plot_temp_1 = ggplot(plot_dataT, aes(x = factor(year), color = factor(state), 
                               fill = factor(state))) + 
  geom_boxplot(aes(y = value), alpha=0.3, outlier.size = 0.6) +
  theme_bw() +
  xlab(NULL) +
  ylab('Temperature (°C)') +
  scale_x_discrete(limits = as.character(allYears), 
                   labels = x_labs) +
  theme(legend.position = c(0.5, 0.88), legend.background =element_blank()) +
  guides(fill=guide_legend(title=NULL),
         color=guide_legend(title=NULL)) 

# By release date:
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
ggsave(filename = 'figures/initLocations.png', device = 'png', width = 95, height = 80, units = 'mm', dpi = 500)

