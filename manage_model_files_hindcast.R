rm(list = ls())
setwd('C:/Users/moroncog/Documents/DisMELS_Pcod_model')
library(xml2)
library(lubridate)
source('create_inputs_fun.R')
fixed_date = ymd_hms('1900-01-01 00:00:00')


# Parameters --------------------------------------------------------------
# Specify years to run in each core:
year_vec = 2018:2020
scenario = 'Hindcast' # do NOT change for MIROC or CESM
scenario_true = 'hindcast'
# DO NOT forget to change D or E disk below

n_cores = length(list.dirs(path = scenario, recursive = FALSE))

# Copy and paste standard files that I need:
# WARNING!!! DO NOT FORGET TO CHANGE THE CANONICAL INITIAL PATH IN ROMS WHEN 
# CHANGING RCP SCENARIO

for(j in 1:n_cores) {
  # Copy and paste ROMS file:
  file.copy(from = file.path('main_files_hindcast', 'ROMS.properties'), 
            to = file.path(paste0(scenario, '/Core_', j)), recursive = TRUE)
  # Copy and paste LHS file:
  file.copy(from = file.path('main_files_hindcast', 'LHS_Types.xml'), 
            to = file.path(paste0(scenario, '/Core_', j)), recursive = TRUE)
  # Copy and paste LHS parameters file:
  file.copy(from = file.path('main_files_hindcast', 'LHS_Parameters.Epijuv1kmHSM.RandomMovement.xml'), 
            to = file.path(paste0(scenario, '/Core_', j)), recursive = TRUE)
}

# DO NOT FORGET TO UPDATE THE PCOD SH IN EACH CORE !!!!!!!!

ROMS_files = list.files(path = 'E:/Bering10K/hindcast')
first_file_index = 54

# Specify when to release eggs
ini_day_month = c('03-01', '03-04', '03-07', 
                  paste0('03-', seq(from = 10, to = 31, by = 3))) # month-day
#ini_day_month = c('03-01', '03-06', '03-11', '03-16', '03-21', '03-26', '03-31') # month-day
ini_numbers = rep(x = 1E+6, times = length(ini_day_month))

# Now copy and paste Model.xml file:
for(i in seq_along(year_vec)) {
  
  sel_year = year_vec[i]
  
  # Create initAtt file csv:
  create_init_locations(ini_dates = paste0(sel_year, '-', ini_day_month), 
                        Path = paste0(scenario, '/Core_', i),
                        ini_numbers = ini_numbers)
  
  # Create folder in corresponding core:
  model_path = paste0(scenario, '/Core_', i, '/', 'Connectivity_files_', sel_year)
  dir.create(path = model_path)
  model_path = paste0(scenario, '/Core_', i, '/', 'Results_files_', sel_year)
  dir.create(path = model_path)
  
  # Read Model file XML:
  model_file = xml2::read_xml(x = file.path('main_files_hindcast', "Model_Run_1.xml"))
  # Change Netcdf file name:
  this_file = ROMS_files[first_file_index + (sel_year - 2000)*5] # file
  ncdfName = paste0("E:\\Bering10K\\", scenario_true,"\\", this_file)
  tmp_ind = xml2::xml_find_all(x = model_file, '//void')[4]
  xml_text(tmp_ind) = ncdfName
  # Change Conectivity folder name:
  conec_name = paste0('Connectivity_files_', sel_year,'\\ConnecFile')
  tmp_ind = xml2::xml_find_all(x = model_file, '//void')[1]
  xml_text(tmp_ind) = conec_name
  # Change Results folder name:
  result_name = paste0('Results_files_', sel_year,'\\ResultFile')
  tmp_ind = xml2::xml_find_all(x = model_file, '//void')[5]
  xml_text(tmp_ind) = result_name
  # Change Start time:
  ini_time = ymd_hms(paste0(sel_year, '-01-01 12:00:00'))
  time_seconds = as.numeric(ini_time - fixed_date)*86400
  tmp_ind = xml2::xml_find_all(x = model_file, '//void')[9]
  xml_text(tmp_ind) = as.character(time_seconds)
  
  # Print new Model file in corresponding folder:
  xml2::write_xml(x = model_file, file = 
                    file.path(paste0(scenario, '/Core_', i), 'Model_1.xml'))

}
