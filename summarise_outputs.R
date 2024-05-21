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

# Some important information for plotting:
thLow1 = 0.025
thHigh1 = 0.975
alphaLevel = 0.5
n_releases = 31 # 11 or 31, depending on the n ID released
n_grids = 206
n_id = n_grids*n_releases

# Read output files -------------------------------------------------------
# DisMELS outputs not provided due to file size (too big)

# main_folder = 'E:/DisMELS_save_outputs/save_hindcast' # directory where the DisMELS outputs are
# save_folder = 'output_data/hindcast' # directory where to save the processed data for plotting

main_folder = 'C:/Users/moroncog/Documents/DisMELS_Pcod_model/IBM_output_hindcast_moreID'
save_folder = 'output_data/hindcast_moreID'

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
plot_data_15 = list() # depth
plot_data_16 = list() # light

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

    # Section 8.5 ----------
    # Environmental variables (alive + dead): depth
    stageData = tmpData
    env_data = aggregate(x = list(value = stageData$vertPos), 
                         list(year = stageData$year, id = stageData$id), 
                         FUN = mean, na.rm=TRUE) # mean values
    env_data$variable = 'depth'
    env_data$state = 'dead'
    env_data$state[env_data$id %in% alive_data$id] = 'alive'
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = env_data
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_15[[indList]] = toPlotData
        
    # Section 8.5 ----------
    # Environmental variables (alive + dead): light
    stageData = tmpData
    env_data = aggregate(x = list(value = stageData$eb), 
                         list(year = stageData$year, id = stageData$id), 
                         FUN = mean, na.rm=TRUE) # mean values
    env_data$variable = 'light'
    env_data$state = 'dead'
    env_data$state[env_data$id %in% alive_data$id] = 'alive'
    # Prepare data to save:
    sel_var = 'value'
    toPlotData = env_data
    toPlotData$relday = base_wgt$rel_date[match(toPlotData$id, base_wgt$id)]
    toPlotData$id_grid = baseLocs$id_grid[match(toPlotData$id, baseLocs$id)]
    plot_data_16[[indList]] = toPlotData    
    
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
save(plot_data_15, file = file.path(save_folder, 'plot_data_15.RData'))
save(plot_data_16, file = file.path(save_folder, 'plot_data_16.RData'))
save(baseLocs, file = file.path(save_folder, 'baseLocs.RData'))
save(baseLocs2, file = file.path(save_folder, 'baseLocs2.RData'))
save(base_wgt, file = file.path(save_folder, 'base_wgt.RData'))
