library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

# --------------------------(DATA CLEANING)--------------------------------------- #
{
# result: claims #
# Load-in and Clean claims files
{
  claims_path = "data/claims.xlsx"
  claims = read_excel(claims_path, sheet = "claims")
  
  claims = claims %>%
    rename(`Contractor Name` = Subcontractor) %>%
    mutate(Recordables = ifelse(
      claims$`OSHA Classification` %in% c('Medical Only', 
                                          'Restricted Duty', 
                                          'Lost Time Injury'),
      TRUE,
      FALSE)) %>%
    mutate(DaysAwayRec = ifelse(
      claims$`OSHA Classification` %in% c('Restricted Duty', 
                                          'Lost Time Injury'),
      TRUE,
      FALSE)) %>%
    mutate(LostTimeRec = ifelse(
      claims$`OSHA Classification` %in% c('Lost Time Injury'),
      TRUE,
      FALSE)) %>%
    mutate(Month = format(`Date of Loss`, "%m")) %>%
    mutate(Year = format(`Date of Loss`, "%Y"))
}

# result: hours # 
# Load-in and clean hours file
{
  hours_path = "data/hours.xlsx"
  hours = read_excel(hours_path, sheet = "hours")
  
  hours = hours %>%
    rename(`Project #` = `Job Number`) %>%
    rename(`Project Name` = `Job Name`) %>%
    rename(`Contractor Name` = PMJT_Parter_Name)
}
}

# --------------------------( OVERVIEW )--------------------------------------- #
{
# --------------------------(OVERVIEW HOURS)--------------------------------------- #
{
# result: ov_month_hours #
# MTD - df of Workhours by Overview, Year and Month
{
  ov_month_hours = hours %>%
    group_by(Month, Year) %>%
    summarise(Workhours = sum(Workhours), `Worker Count` = sum(`Worker Count`))
}

# result: ov_wh_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_ov = ov_month_hours %>%
    mutate(ID = Year) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_ov_hours = expand.grid(ID = ID_ov$ID,
                                      Month = sprintf("%02d", 1:12))
  ov_wh_filler_months = all_months_ov_hours %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Year"))
}

# result: data_omh -> data for Tables and Plots
{
  #1. Merge filler ZERO months with ov_month_hours
  fm_omh_merged = merge(ov_month_hours, ov_wh_filler_months,
                        by = c("Year", "Month"),
                        all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_omh = fm_omh_merged %>%
    mutate(Workhours = ifelse(is.na(Workhours), 0, Workhours))
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeWH = cumsum(Workhours))
}

# result: ov_year_hours -> YTD - df of Workhours by Overview, Year
{
  ov_year_hours = hours %>%
    group_by(Year) %>%
    summarise(Workhours = sum(Workhours))
}

# result: proj_total_hours -> PTD - df of Workhours by Project
{
  ov_total_hours = hours %>%
    ungroup() %>%
    summarise(Workhours = sum(Workhours))
}
}

# --------------------------(OVERVIEW TRIR)--------------------------------------- #
{
# result: ov_month_rec #
# MTD - df of Recordables by Overview, Year and Month
{
  ov_month_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    group_by(Month, Year) %>%
    summarise(Recordables = n(), 
              `OSHA Classification`= paste(`OSHA Classification`, collapse = ", "))
}

# result: ov_rec_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_rec_ov = ov_month_rec %>%
    mutate(ID = paste(Year)) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_ov_rec = expand.grid(ID = ID_rec_ov$ID,
                                    Month = sprintf("%02d", 1:12))
  ov_rec_filler_months = all_months_ov_rec %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Year"))
}

# result: data_omr -> data for Tables and Plots
{
  #1. Merge filler ZERO months with ov_month_rec
  fm_omr_merged = merge(ov_month_rec, ov_rec_filler_months,
                        by = c("Year", "Month"),
                        all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_omr = fm_omr_merged %>%
    mutate(Recordables = ifelse(is.na(Recordables), 0, Recordables)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: ov_year_rec -> YTD - df of Recordables by Overview, Year
{
  ov_year_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    group_by(Year) %>%
    summarise(Recordables = n())
}

# result: ov_total_rec -> PTD - df of Recordables by Overview
{
  ov_total_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    ungroup() %>%
    summarise(Recordables = n())
}

# result: ov_data_trir -> all TRIR of Overview with hours 
{
  ov_data_trir = merge(data_omh, data_omr,
                    by = c("Year", "Month"),
                    all.x = TRUE) %>%
    mutate(Recordables = ifelse(is.na(Recordables), 0, Recordables))
}
}

# --------------------------(OVERVIEW DART)--------------------------------------- #
{
# result: ov_month_dart #
# MTD - df of dart by Overview, Year and Month
{
  ov_month_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    group_by(Month, Year) %>%
    summarise(DaysAwayRec = n())
}

# result: ov_dart_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_dart_ov = ov_month_dart %>%
    mutate(ID = paste(Year)) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_ov_dart = expand.grid(ID = ID_dart_ov$ID,
                                     Month = sprintf("%02d", 1:12))
  ov_dart_filler_months = all_months_ov_dart %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Year"))
}

# result: data_omdart -> data for Tables and Plots
{
  #1. Merge filler months with ov_month_dart
  fm_omdart_merged = merge(ov_month_dart, ov_dart_filler_months,
                           by = c("Year", "Month"),
                           all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_omdart = fm_omdart_merged %>%
    mutate(DaysAwayRec = ifelse(is.na(DaysAwayRec), 0, DaysAwayRec)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: ov_year_dart -> YTD - df of dart by Overview, Year
{
  ov_year_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    group_by(Year) %>%
    summarise(DaysAwayRec = n())
}

# result: ov_total_dart -> PTD - df of dart by Overview
{
  ov_total_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    ungroup() %>%
    summarise(DaysAwayRec = n())
}

# result: ov_data_dart -> all dart of Overview with hours 
{
  ov_data_dart = merge(data_omh, data_omdart,
                    by = c("Year", "Month"),
                    all.x = TRUE) %>%
    mutate(DaysAwayRec = ifelse(is.na(DaysAwayRec), 0, DaysAwayRec))
}
}
  
# ---------------------------(OVERVIEW LTIR)--------------------------------------- #
{
# result: ov_month_ltir #
# MTD - df of ltir by Overview, Year and Month
{
  ov_month_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    group_by(Month, Year) %>%
    summarise(LostTimeRec = n())
}

# result: ov_ltir_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_ltir_ov = ov_month_ltir %>%
    mutate(ID = paste(Year)) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_ov_ltir = expand.grid(ID = ID_ltir_ov$ID,
                                     Month = sprintf("%02d", 1:12))
  ov_ltir_filler_months = all_months_ov_ltir %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Year"))
}

# result: data_omltir -> data for Tables and Plots
{
  #1. Merge filler months with ov_month_ltir
  fm_omltir_merged = merge(ov_month_ltir, ov_ltir_filler_months,
                           by = c("Year", "Month"),
                           all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_omltir = fm_omltir_merged %>%
    mutate(LostTimeRec = ifelse(is.na(LostTimeRec), 0, LostTimeRec)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: ov_year_ltir -> YTD - df of ltir by Overview, Year
{
  ov_year_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    group_by(Year) %>%
    summarise(LostTimeRec = n())
}

# result: ov_total_ltir -> PTD - df of ltir by Overview
{
  ov_total_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    ungroup() %>%
    summarise(LostTimeRec = n())
}

# result: ov_data_ltir -> all ltir of Overview with hours 
{
  ov_data_ltir = merge(data_omh, data_omltir,
                    by = c("Year", "Month"),
                    all.x = TRUE) %>%
    mutate(LostTimeRec = ifelse(is.na(LostTimeRec), 0, LostTimeRec))
}
}
}

# ---------------------------( PROJECT )--------------------------------------- #
{
# --------------------------(PROJECT HOURS)--------------------------------------- #
{
# result: active_proj # active is a project with hours in the current month
  # Used in Drop Downs
{
  active_proj = hours %>%
    dplyr::filter(Year == max(Year)) %>%
    dplyr::filter(Month == max(Month))
}

# result: proj_month_hours #
# MTD - df of Workhours by Job, Year and Month
{
  proj_month_hours = hours %>%
    group_by(`Project #`, `Project Name`, Month, Year) %>%
    summarise(Workhours = sum(Workhours), `Worker Count` = sum(`Worker Count`))
}

# result: wh_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_proj = proj_month_hours %>%
    mutate(ID = paste(`Project #`,`Project Name`, Year,sep = ";")) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
    
  all_months_proj_hours = expand.grid(ID = ID_proj$ID,
                           Month = sprintf("%02d", 1:12))
  wh_filler_months = all_months_proj_hours %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Project #",
                          "Project Name",
                          "Year"),
             sep = ";")
}

# result: data_pmh -> data for Tables and Plots
{
  #1. Merge filler ZERO months with proj_month_hours
  fm_pmh_merged = merge(proj_month_hours, wh_filler_months,
        by = c("Project #", "Project Name", "Year", "Month"),
        all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_pmh = fm_pmh_merged %>%
    mutate(Workhours = ifelse(is.na(Workhours), 0, Workhours))
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeWH = cumsum(Workhours))
}

# result: proj_year_hours -> YTD - df of Workhours by Project, Year
{
  proj_year_hours = hours %>%
    group_by(`Project #`, `Project Name`, Year) %>%
    summarise(Workhours = sum(Workhours))
}
  
# result: proj_total_hours -> PTD - df of Workhours by Project
{
  proj_total_hours = hours %>%
    group_by(`Project #`, `Project Name`) %>%
    summarise(Workhours = sum(Workhours))
}
}

# --------------------------(PROJECT TRIR)--------------------------------------- #
{
# result: proj_month_rec #
# MTD - df of Recordables by Job, Year and Month
{
  proj_month_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`, Month, Year) %>%
    summarise(Recordables = n(), 
              `OSHA Classification`= paste(`OSHA Classification`, collapse = ", "))
}

# result: rec_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_rec = proj_month_rec %>%
    mutate(ID = paste(`Project #`,`Project Name`, Year, sep = ";")) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_proj_rec = expand.grid(ID = ID_rec$ID,
                           Month = sprintf("%02d", 1:12))
  rec_filler_months = all_months_proj_rec %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Project #",
                      "Project Name",
                      "Year"),
             sep = ";")
}

# result: data_pmr -> data for Tables and Plots
{
  #1. Merge filler ZERO months with proj_month_rec
  fm_pmr_merged = merge(proj_month_rec, rec_filler_months,
                        by = c("Project #", "Project Name", "Year", "Month"),
                        all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_pmr = fm_pmr_merged %>%
    mutate(Recordables = ifelse(is.na(Recordables), 0, Recordables)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: proj_year_rec -> YTD - df of Recordables by Project, Year
{
  proj_year_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`, Year) %>%
    summarise(Recordables = n())
}

# result: proj_total_rec -> PTD - df of Recordables by Project
{
  proj_total_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`) %>%
    summarise(Recordables = n())
}

# result: data_trir -> all TRIR of projects with hours 
{
data_trir = merge(data_pmh, data_pmr,
                  by = c("Project #", "Project Name", "Year", "Month"),
                  all.x = TRUE) %>%
    mutate(Recordables = ifelse(is.na(Recordables), 0, Recordables))
}
}

# --------------------------(PROJECT DART)--------------------------------------- #
{
# result: proj_month_dart #
# MTD - df of dart by Job, Year and Month
{
  proj_month_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`, Month, Year) %>%
    summarise(DaysAwayRec = n())
}

# result: dart_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_dart = proj_month_dart %>%
    mutate(ID = paste(`Project #`,`Project Name`, Year, sep = ";")) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_proj_dart = expand.grid(ID = ID_dart$ID,
                           Month = sprintf("%02d", 1:12))
  dart_filler_months = all_months_proj_dart %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Project #",
                      "Project Name",
                      "Year"),
             sep = ";")
}

# result: data_pmdart -> data for Tables and Plots
{
  #1. Merge filler months with proj_month_dart
  fm_pmdart_merged = merge(proj_month_dart, dart_filler_months,
                        by = c("Project #", "Project Name", "Year", "Month"),
                        all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_pmdart = fm_pmdart_merged %>%
    mutate(DaysAwayRec = ifelse(is.na(DaysAwayRec), 0, DaysAwayRec)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: proj_year_dart -> YTD - df of dart by Project, Year
{
  proj_year_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`, Year) %>%
    summarise(DaysAwayRec = n())
}

# result: proj_total_dart -> PTD - df of dart by Project
{
  proj_total_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`) %>%
    summarise(DaysAwayRec = n())
}

# result: data_dart -> all dart of projects with hours 
{
  data_dart = merge(data_pmh, data_pmdart,
                    by = c("Project #", "Project Name", "Year", "Month"),
                    all.x = TRUE) %>%
    mutate(DaysAwayRec = ifelse(is.na(DaysAwayRec), 0, DaysAwayRec))
}
}

# ---------------------------(PROJECT LTIR)--------------------------------------- #
{
# result: proj_month_ltir #
# MTD - df of ltir by Job, Year and Month
{
  proj_month_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`, Month, Year) %>%
    summarise(LostTimeRec = n())
}

# result: ltir_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_ltir = proj_month_ltir %>%
    mutate(ID = paste(`Project #`,`Project Name`, Year, sep = ";")) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_proj_ltir = expand.grid(ID = ID_ltir$ID,
                           Month = sprintf("%02d", 1:12))
  ltir_filler_months = all_months_proj_ltir %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Project #",
                      "Project Name",
                      "Year"),
             sep = ";")
}

# result: data_pmltir -> data for Tables and Plots
{
  #1. Merge filler months with proj_month_ltir
  fm_pmltir_merged = merge(proj_month_ltir, ltir_filler_months,
                           by = c("Project #", "Project Name", "Year", "Month"),
                           all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_pmltir = fm_pmltir_merged %>%
    mutate(LostTimeRec = ifelse(is.na(LostTimeRec), 0, LostTimeRec)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: proj_year_ltir -> YTD - df of ltir by Project, Year
{
  proj_year_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`, Year) %>%
    summarise(LostTimeRec = n())
}

# result: proj_total_ltir -> PTD - df of ltir by Project
{
  proj_total_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    group_by(`Project #`, `Project Name`) %>%
    summarise(LostTimeRec = n())
}

# result: data_ltir -> all ltir of projects with hours 
{
  data_ltir = merge(data_pmh, data_pmltir,
                    by = c("Project #", "Project Name", "Year", "Month"),
                    all.x = TRUE) %>%
    mutate(LostTimeRec = ifelse(is.na(LostTimeRec), 0, LostTimeRec))
}
}
}

# --------------------------( CONTRACTOR )--------------------------------------- #
{
# --------------------------(CONTRACTOR HOURS)--------------------------------------- #
{
# result: active_cont # active is a contractor with hours in the current month
# Used in Drop Downs
{
  active_cont = hours %>%
    dplyr::filter(Year == max(Year)) %>%
    dplyr::filter(Month == max(Month))
}

# result: cont_month_hours #
# MTD - df of Workhours by Contractor, Year and Month
{
  cont_month_hours = hours %>%
    group_by(`Contractor Name`, Month, Year) %>%
    summarise(Workhours = sum(Workhours), `Worker Count` = sum(`Worker Count`))
}

# result: cont_filler_months # 
# Create filler month with ZERO value for contractors
{      
  ID_cont = cont_month_hours %>%
    mutate(ID = paste(`Contractor Name`, Year,sep = ";")) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_cont_hours = expand.grid(ID = ID_cont$ID,
                           Month = sprintf("%02d", 1:12))
  cont_filler_months = all_months_cont_hours %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Contractor Name",
                      "Year"),
             sep = ";")
}

# result: data_cmh -> data for Tables and Plots
{
  #1. Merge filler ZERO months with cont_month_hours
  fm_cmh_merged = merge(cont_month_hours, cont_filler_months,
                        by = c("Contractor Name", "Year", "Month"),
                        all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_cmh = fm_cmh_merged %>%
    mutate(Workhours = ifelse(is.na(Workhours), 0, Workhours))
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeWH = cumsum(Workhours))
}

# result: cont_year_hours -> YTD - df of Workhours by Contractor, Year
{
  cont_year_hours = hours %>%
    group_by(`Contractor Name`, Year) %>%
    summarise(Workhours = sum(Workhours))
}

# result: cont_total_hours -> PTD - df of Workhours by Contractor
{
  cont_total_hours = hours %>%
    group_by(`Contractor Name`) %>%
    summarise(Workhours = sum(Workhours))
}
}

# --------------------------(CONTRACTOR TRIR)--------------------------------------- #
{
# result: cont_month_rec #
# MTD - df of Recordables by Job, Year and Month
{
  cont_month_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    group_by(`Contractor Name`, Month, Year) %>%
    summarise(Recordables = n(), 
              `OSHA Classification`= paste(`OSHA Classification`, collapse = ", "))
}

# result: cont_rec_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_rec_cont = cont_month_rec %>%
    mutate(ID = paste(`Contractor Name`, Year, sep = ";")) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_cont_rec = expand.grid(ID = ID_rec_cont$ID,
                           Month = sprintf("%02d", 1:12))
  cont_rec_filler_months = all_months_cont_rec %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Contractor Name",
                      "Year"),
             sep = ";")
}

# result: data_cmr -> data for Tables and Plots
{
  #1. Merge filler ZERO months with cont_month_rec
  fm_cmr_merged = merge(cont_month_rec, cont_rec_filler_months,
                        by = c("Contractor Name", "Year", "Month"),
                        all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_cmr = fm_cmr_merged %>%
    mutate(Recordables = ifelse(is.na(Recordables), 0, Recordables)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: cont_year_rec -> YTD - df of Recordables by Contractor, Year
{
  cont_year_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    group_by(`Contractor Name`, Year) %>%
    summarise(Recordables = n())
}

# result: cont_total_rec -> PTD - df of Recordables by Contractor
{
  cont_total_rec = claims %>%
    dplyr::filter(Recordables %in% "TRUE") %>%
    group_by(`Contractor Name`) %>%
    summarise(Recordables = n())
}

# result: cont_data_trir -> all TRIR of contractor with hours 
{
  cont_data_trir = merge(data_cmh, data_cmr,
                    by = c("Contractor Name", "Year", "Month"),
                    all.x = TRUE) %>%
    mutate(Recordables = ifelse(is.na(Recordables), 0, Recordables))
}
}
  
# --------------------------(CONTRACTOR DART)--------------------------------------- #
{
# result: cont_month_dart #
# MTD - df of dart by Contractor, Year and Month
{
  cont_month_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    group_by(`Contractor Name`, Month, Year) %>%
    summarise(DaysAwayRec = n())
}

# result: cont_dart_filler_months # 
# Create filler month with ZERO value for projects
{      
  ID_dart_cont = cont_month_dart %>%
    mutate(ID = paste(`Contractor Name`, Year, sep = ";")) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_cont_dart = expand.grid(ID = ID_dart_cont$ID,
                           Month = sprintf("%02d", 1:12))
  cont_dart_filler_months = all_months_cont_dart %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Contractor Name",
                      "Year"),
             sep = ";")
}

# result: data_cmdart -> data for Tables and Plots
{
  #1. Merge filler months with proj_month_dart
  fm_cmdart_merged = merge(cont_month_dart, cont_dart_filler_months,
                           by = c("Contractor Name", "Year", "Month"),
                           all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_cmdart = fm_cmdart_merged %>%
    mutate(DaysAwayRec = ifelse(is.na(DaysAwayRec), 0, DaysAwayRec)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: cont_year_dart -> YTD - df of dart by Contractor, Year
{
  cont_year_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    group_by(`Contractor Name`, Year) %>%
    summarise(DaysAwayRec = n())
}

# result: cont_total_dart -> PTD - df of dart by Contractor
{
  cont_total_dart = claims %>%
    dplyr::filter(DaysAwayRec %in% "TRUE") %>%
    group_by(`Contractor Name`) %>%
    summarise(DaysAwayRec = n())
}

# result: cont_data_dart -> all dart of Contractors with hours 
{
  cont_data_dart = merge(data_cmh, data_cmdart,
                    by = c("Contractor Name", "Year", "Month"),
                    all.x = TRUE) %>%
    mutate(DaysAwayRec = ifelse(is.na(DaysAwayRec), 0, DaysAwayRec))
}
}
  
# ---------------------------(CONTRACTOR LTIR)--------------------------------------- #
{
# result: cont_month_ltir #
# MTD - df of ltir by Contractor, Year and Month
{
  cont_month_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    group_by(`Contractor Name`, Month, Year) %>%
    summarise(LostTimeRec = n())
}

# result: cont_ltir_filler_months # 
# Create filler month with ZERO value for Contractor
{      
  ID_ltir_cont = cont_month_ltir %>%
    mutate(ID = paste(`Contractor Name`, Year, sep = ";")) %>%
    ungroup() %>%
    select(ID) %>%
    distinct(ID)
  
  all_months_cont_ltir = expand.grid(ID = ID_ltir_cont$ID,
                                     Month = sprintf("%02d", 1:12))
  cont_ltir_filler_months = all_months_cont_ltir %>%
    arrange(ID) %>%
    separate(ID, 
             into = c("Contractor Name",
                      "Year"),
             sep = ";")
}

# result: data_cmltir -> data for Tables and Plots
{
  #1. Merge filler months with proj_month_ltir
  fm_cmltir_merged = merge(cont_month_ltir, cont_ltir_filler_months,
                           by = c("Contractor Name", "Year", "Month"),
                           all = TRUE)
  
  #2. Replace NA Workhours with 0
  data_cmltir = fm_cmltir_merged %>%
    mutate(LostTimeRec = ifelse(is.na(LostTimeRec), 0, LostTimeRec)) 
  #3. Add Workhours CumSum Column
  # %>%
  #   group_by(`Project #`, `Project Name`) %>%
  #   mutate(CumulativeRec = cumsum(Recordables))
}

# result: cont_year_ltir -> YTD - df of ltir by Contractor, Year
{
  cont_year_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    group_by(`Contractor Name`, Year) %>%
    summarise(LostTimeRec = n())
}

# result: cont_total_ltir -> PTD - df of ltir by Contractor
{
  cont_total_ltir = claims %>%
    dplyr::filter(LostTimeRec %in% "TRUE") %>%
    group_by(`Contractor Name`) %>%
    summarise(LostTimeRec = n())
}

# result: cont_data_ltir -> all ltir of Contractors with hours 
{
  cont_data_ltir = merge(data_cmh, data_cmltir,
                    by = c("Contractor Name", "Year", "Month"),
                    all.x = TRUE) %>%
    mutate(LostTimeRec = ifelse(is.na(LostTimeRec), 0, LostTimeRec))
}
}
}
