#########################################################################
#												                                                #
#	      R Code for HydroProcessor and HCAM Capabilities 		            #
#												                                                #
#########################################################################

# Authors: Kate Munson, Alyssa Le

# Clear global environment prior to running this code
rm(list=ls())

library(reader)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

options(stringsAsFactors=FALSE)

left = function(text, num_char) {
  substr(text, 1, num_char)
}

## NOTE TO USER: This code needs to be run in two parts. First, run the code up to Part 4A. This will output another
## temporary user specifications file. Fill out the file and then run the code from Part 4B through Part 5.

## NOTE TO USER: the R code assumes that data will be extracted from  oput.hru, .gw, and .sol file headers
## formatted exactly as the  oput.hru, .gw, and .sol files in the R_programs/Inputs/ExampleFiles folder.
## Code denoted with #-DB-# denotes locations where information could be read from a database, 
## rather than individual oput.hru, .gw, and .sol files

################################################
# VARIABLES TO ADJUST
################################################

## NOTE TO USER: we recommend setting up a folder structure that designates separate input and output folders.
## The input folder should include your WMOST shell files and your Specs Results file (_LogFileTEMP.csv output from WMOSTv3.1) 
## should be renamed to template_SpecsResults.csv
inpath <- "C:/Users/50367/ICF/WMOST - R_Code_Development/HCAM_R/Inputs/"
outpath <- "C:/Users/50367/ICF/WMOST - R_Code_Development/HCAM_R/Outputs/"

## NOTE TO USER: this file path should include all of the SWAT models you would like to process.
SWAT_model_inpath <- "C:/Users/50367/ICF/WMOST - R_Code_Development/SWAT_Models/Amendment_Models/"

## NOTE TO USER: these statements will process WMOST files for nitrogen, phosphorus, and sediment. 
## Adjust these statements if you would like to run only a subset of these constituents.
const <- c("TN","TP","TSS")
ru_const_lbs <- c("Ru_N_lbs_wmean","Ru_P_lbs_wmean","SYLD_lbs_wmean")
ru_const_kg <- c("Ru_N_kg_wmean","Ru_P_kg_wmean","SYLD_kg_wmean")
re_const_lbs <- c("RCHG_N_lbs_wmean","Re_P_lbs_wmean","Re_SYLD_wmean")
re_const_kg <- c("RCHG_N_wmean","Re_P_kg_wmean","Re_SYLD_wmean")
ru_sum_const <- c("Ru_N_lbs_wmean_sum","Ru_P_lbs_wmean_sum","SYLD_lbs_wmean_sum")
re_sum_const <- c("RCHG_N_lbs_wmean_sum","Re_P_lbs_wmean_sum","Re_SYLD_wmean_sum")

## NOTE TO USER: select "YES" to add direct deposition to stream from cattle grazers to RNGE HRU runoff loadings.
## Select "NO" if not considering direct deposition to stream for RNGE land use types. 
dir_dep <- "YES"

## NOTE TO USER: Specify subfolders for managed sets included in the analysis,
## ensuring that the folder names are listed in the same order as the managed sets specified in UserSpecs.csv.
## (In other words, UserSpecs.csv "Managed_Sets" column value = 1 should correspond to a subfolder listed in the "Scenario_Name"
## column that matches the first element listed in ManagedSets <- c([1],[2]))
ManagedSets <- c("ExistingBMPs","Contouring") 

################################################
# PART 1: READ IN REVISED USER SPECIFICATIONS
################################################

# oput_variables lists variables we expect to be included in SWAT oput.hru files
oput_variables <- c("LULC","HRU","GIS","SUB","MGT","MO","DA","YR","AREAkm2","PRECIP",
                    "PET","SW_INIT","PERC","SA_ST","SURQ_CNT","LATQGEN","GW_Q",
                    "TMP_AV","SYLD","ORGN","ORGP","SEDP","NSURQ","SOLP","RCHG_N")

# oput_variables_retain lists oput.hru file variables we would like to keep in dataset
oput_variables_retain <- c("LULC","HRU","SUB","MO","DA","YR","AREAkm2","PRECIP",
                           "PET","SW_INIT","PERC","SA_ST","SURQ_CNT","LATQGEN","GW_Q",
                           "TMP_AV","SYLD","ORGN","ORGP","SEDP","NSURQ","SOLP","RCHG_N")

# Read in and interpret user specifications from 01_HP_HCAM_DefineUserSpecs.R
user_specs <- read.csv(paste0(inpath,"UserSpecs.csv"))

### Model characterization

# Number of subbasins
paste0("Number of subbasins to be processed: ",length(user_specs$SUB_CHOOSE[user_specs$User_Choose_SUB=="X"]))

# Number of HRUs per subbasin
sub_hru_count <- distinct(user_specs[,c("SUB","HRU")]) %>%
  group_by(SUB) %>%
  summarise(HRU_Count = length(HRU))
sub_hru_count$SUB_LEAD <- lead(sub_hru_count$SUB)
sub_hru_count$HRU_Count_rev <- cumsum(sub_hru_count$HRU_Count)

# Unique HRUs and subbasins
hrus_sub <- distinct(user_specs[user_specs$User_Choose_HRU=="X",c("SUB","HRU","LULC")])

SUBS <- c(user_specs$SUB_CHOOSE[user_specs$User_Choose_SUB=="X"])

HRUS_agg <- data.frame("SUB" = user_specs$SUB[user_specs$User_Choose_HRU=="X"],
                       "HRU"=user_specs$HRU[user_specs$User_Choose_HRU=="X"],
                       "AGG_HRU"=user_specs$User_Agg_HRU_ID[user_specs$User_Choose_HRU=="X"])

sub_hru_rev <- sub_hru_count[sub_hru_count$SUB_LEAD %in% unique(user_specs$SUB[user_specs$User_Choose_HRU=="X"]),] %>% select("HRU_Count_rev","SUB_LEAD")
names(sub_hru_rev) <- c("HRU_Count_rev","SUB")

HRUS_agg_rev <- merge(HRUS_agg,sub_hru_rev,by="SUB",all=TRUE)

HRUS_agg_rev[is.na(HRUS_agg_rev)] <- 0

HRUS_agg_rev$HRU_rev <- with(HRUS_agg_rev,HRU-HRU_Count_rev)

HRUS_orig <- c(HRUS_agg_rev$HRU)

## Create vectors of subbasins, subbasin-HRU combinations, and selected management options for loop
sub_vec <- as.character(str_pad(as.factor(SUBS), width=5, pad="0"))
# (above for oput.hru files)

sub_hru_vec <- as.character(unique(paste0(str_pad(as.factor(HRUS_agg_rev$SUB), width=5, pad="0"),
                                   str_pad(as.factor(HRUS_agg_rev$HRU_rev), width=4, pad="0"))))
# (above for .gw, and .sol files)

# Reformat dates if in DD/MM/YYYY form user_specs$User_StartDate_YYYY.MM.DD[1]
StartDate <- as.Date(as.character(user_specs$User_StartDate_YYYY.MM.DD[1]), format = "%m/%d/%Y") # Assumes excel will always format dates as mm/dd/yyyy
EndDate <- as.Date(as.character(user_specs$User_EndDate_YYYY.MM.DD[1]), format = "%m/%d/%Y") # Assumes excel will always format dates as mm/dd/yyyy

# Develop vector of scenario options for reading in data
scen_vec <- c(user_specs$Scenario_Name[!is.na(user_specs$Managed_Sets)])

# Develop vector of climate scenarios and management options for data output
clim_vec <- as.vector(unique(user_specs$Climate_Scenario[!is.na(user_specs$Managed_Sets)]))
mgmt_vec <- as.vector(unique(user_specs$Managed_Sets[!is.na(user_specs$Managed_Sets)]))

# Build in aggregation functionality
agg_HRU_vec <- as.vector(unique(HRUS_agg$AGG_HRU))

# Allow the user to revise order of HRUs manually
## Message to users to update temp_HRUSpecs.csv
Message_1 <- "NOTE TO USER: If another order of aggregate HRUs is desired, uncomment the following code and adjust order."
Message_1

#*# KM: Note - HRUSpecs file from EPA lists HRUs in different order than UserSpecs.csv, so must retain EPA order
agg_HRU_vec
agg_HRU_vec <- c(agg_HRU_vec[2],agg_HRU_vec[4],agg_HRU_vec[1],agg_HRU_vec[3],agg_HRU_vec[5],agg_HRU_vec[6],agg_HRU_vec[7])

###############################################################
# PART 2: READ IN REMAINING DATA BASED ON USER SPECIFICATIONS
###############################################################

#-DB-# User could extract .gw header information from SWAT run database. User would also need to extract 
#-DB-# GWSOLP values from the database.

#-DB-# User could extract .sol header information from SWAT run database for future merge 
#-DB-# with .gw file header information. User would also need to extract Ksat and Depth values from .sol files.

## Loop through the managed set scenarios
for (RunMgmt in 1:length(ManagedSets)) {

  print(paste0("Evaluate managed sets - set = ", ManagedSets[RunMgmt]))
  
  Mgmt <- ManagedSets[RunMgmt]
  
  ### urban.dat files
  
  urb_filename <- paste0(SWAT_model_inpath,ManagedSets[RunMgmt],"/TxtInOut","/urban.dat")
  
  file_urb_read <- readLines(urb_filename)
  
  file_urb_read_Line1 <- as.data.frame(file_urb_read[grepl("U",file_urb_read)])
  names(file_urb_read_Line1) <- "string"
  
  file_urb_read_Line1$LULC_Gen <- trimws(substr(file_urb_read_Line1$string,5,8),
                                         which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  
  file_urb_read_Line1$FIMP <- as.numeric(trimws(substr(file_urb_read_Line1$string,65,72),
                                                which = c("both", "left", "right"), whitespace = "[ \t\r\n]"))
  
  file_urb_read_Line1$FCIMP <- as.numeric(trimws(substr(file_urb_read_Line1$string,73,80),
                                                 which = c("both", "left", "right"), whitespace = "[ \t\r\n]"))
  
  urb_form <- file_urb_read_Line1 %>% select(c("LULC_Gen","FIMP","FCIMP"))
  
  # From WMOST theoretical doc: EIA = FIMP if FIMP > 0.3; EIA = FIMP-0.5(FIMP-FCIMP) if FIMP < 0.3
  
  urb_form$EIA <- with(urb_form,ifelse(FIMP > 0.3,FIMP,FIMP-0.5*(FIMP-FCIMP)))

  urb_form$Mgmt_Set <- RunMgmt
  
  # Bind together
  if (exists("urb_bind")) {
    urb_bind <- rbind(urb_bind, urb_form)
  } else {
    urb_bind <- urb_form
  }
  
  ## Loop through the HRUs
  for (RunHRU in 1:length(sub_hru_vec)) {
    
    SubHRU <- sub_hru_vec[RunHRU]
    
    print(paste0("Read HRU data - HRU = ", sub_hru_vec[RunHRU]))
    
    ### .gw files
    
    gw_filename <- paste0(SWAT_model_inpath,ManagedSets[RunMgmt],"/TxtInOut","/",SubHRU,".gw")
    gw_data <- readLines(gw_filename)

    SUB <- sub(" \\H.*", "", sub(".*\\Subbasin:", "", gw_data[1])) 
    HRU <- sub(".* \\HRU:", "", sub("\\ Subbasin:.*", "", gw_data[1]))
    LULC <- sub(" \\S.*", "", sub(".*\\Luse:", "", gw_data[1])) 
    SOIL <- sub(" \\S.*", "", sub(".*\\Soil:", "", gw_data[1])) 
    SLOPE <- left(sub(".*\\Slope:", "", gw_data[1]),3) 

    GWSOLP <- trimws(substr(gw_data[grep("GWSOLP",gw_data)],0,16), 
                     which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    
    # From WMOST theoretical doc: GWSOLP is multiplied by tot recharge entering aquifers 
    # during each time step to estimate recharge P loads
    
    GW_form <- data.frame("SUB" = SUB, "HRU" = HRU, "LULC_Gen" = LULC, "SOIL" = SOIL, "SLOPE" = SLOPE, "GWSOLP" = as.numeric(GWSOLP))
    
    GW_form$Mgmt_Set <- RunMgmt
    
    # Bind together
    if (exists("GW_bind")) {
      GW_bind <- rbind(GW_bind, GW_form)
    } else {
      GW_bind <- GW_form
    }
    
    ### .sol files
    
    sol_filename <- paste0(SWAT_model_inpath,ManagedSets[RunMgmt],"/TxtInOut","/",SubHRU,".sol")
    sol_data <- readLines(sol_filename)
    
    SUB <- sub(" \\H.*", "", sub(".*\\Subbasin:", "", sol_data[1])) 
    HRU <- sub(".* \\HRU:", "", sub("\\ Subbasin:.*", "", sol_data[1])) 

    sol_depth <- trimws(substr(sol_data[grep("Depth",sol_data)],0,64),
                        which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    
    sol_depth_split_unlist <- unlist(lapply(".", grep, unlist(strsplit(sol_depth, split=" ")), value = TRUE))
    
    sol_depth_elements <- as.numeric(sol_depth_split_unlist[-c(1,2)])
    
    sol_ksat <- trimws(substr(sol_data[grep("Ksat",sol_data)],0,64),
                       which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    
    sol_ksat_split <- strsplit(sol_ksat, split=" ")
    
    sol_ksat_split_unlist <- unlist(lapply(".", grep, unlist(strsplit(sol_ksat, split=" ")), value = TRUE))
    
    sol_ksat_elements <- as.numeric(sol_ksat_split_unlist[-c(1,2,3)])
    
    sol_form <- data.frame("SUB" = SUB, "HRU" = HRU, 
                           "DEPTH" = c(sol_depth_elements),
                           "KSAT_EST" = c(sol_ksat_elements))
    
    sol_form$inter_depth <- with(sol_form,ifelse(is.na(lag(DEPTH)),DEPTH,DEPTH-lag(DEPTH)))
    sol_form$inter_sat <- with(sol_form,KSAT_EST*inter_depth)
    
    # Calculate infiltration rate, converting from mm/hr to in/hr.
    infil_rate <- (sum(sol_form$inter_sat)/sum(sol_form$inter_depth))/25.4
    
    sol_form$infil_rate <- infil_rate
    
    sol_form$Mgmt_Set <- RunMgmt
    
    # Bind together
    if (exists("sol_bind")) {
      sol_bind <- rbind(sol_bind, sol_form)
    } else {
      sol_bind <- sol_form
    }
  }
  
}


## Loop through the scenario options
for (RunScen in 1:length(scen_vec)) {
  
  Scen <- scen_vec[RunScen]
  
  print(paste0("Read scenario outputs - Scenario = ", Scen))
    
  ## Loop through the subbasins
  for (RunSub in 1:length(sub_vec)) {

    Subbasin <- sub_vec[RunSub]

    print(paste0("Read scenario outputs - Subbasin = ", sub_vec[RunSub]))
      
    oput_filename <- paste0(Scen,"/","oput",Subbasin,".hru")
    oput_data <- readLines(oput_filename)
        
      oput_header <- oput_data[grepl("HRU",oput_data)]
        
      # Replace all units with space to ensure proper reading of variable names
      
      ## NOTE TO USER: Please check documentation; we assume that the units below are the units reflected in the
      ## columns following the "AREAkm2" column of the oputXXXX.hru file. If additional units are present among variables 
      ## following "AREAkm2", add another line specifying "oput_header_rev <- gsub(["alternate unit"], " ", oput_header_rev)".

      oput_header_rev <- gsub("mm", " ", oput_header) 
      oput_header_rev <- gsub("kg/ha", " ", oput_header_rev) 
      oput_header_rev <- gsub("dgC", " ", oput_header_rev) 
      oput_header_rev <- gsub("t/ha", " ", oput_header_rev) 
      oput_header_rev <- gsub("sqKM", " ", oput_header_rev) 
        
      split_header <- strsplit(oput_header_rev, split=" ") 
      
      split_header_unlist <- unlist(split_header)
        
      # Identify matches with oput_variables character vector
      header_var <- unlist(lapply(oput_variables, grep, split_header_unlist, value = TRUE))
        
      # Retain order of split_header_unlist
      split_header_final <- split_header_unlist[split_header_unlist %in% header_var]

      ## NOTE TO USER: Check oput files to ensure that last 16 variables include (order of variables is not important):
      ## "PRECIP", "PET", "SW_INIT", "PERC", "SA_ST", "SURQ_CNT", "LATQGEN", 
      ## "GW_Q", "TMP_AV", "SYLD", "ORGN", "ORGP", "SEDP", "NSURQ", "SOLP", "RCHG_N"
      ## and follow the variable AREAkm2
        
      ## NOTE TO USER: We expect that there will be 16 variables following the "AREAkm2" variable. If there are more or
      ## fewer variables, adjust the code below from "16F10" to "[Number of variables following AREAkm2]F10".
      oput <- read.fortran(oput_filename, c("1A4","1I5","1I10","2I5","2I3","1I5","1F11","16F10"), header = FALSE, skip=3)
      
      names(oput) <- split_header_final
        
      oput <- oput %>% select(all_of(oput_variables_retain))
        
      # Subset to only HRUs of interest
      oput <- oput[oput$HRU %in% HRUS_orig,]
        
      # Calculate runoff and recharge time series
      # Convert to Tm3pHa units from inches (used within WMOST) from mm.
      oput$SURQ_CNT_Tm3 <- oput$SURQ_CNT*(0.03937/3.93700787401575)
      oput$SURQ_CNT_in <- oput$SURQ_CNT*0.03937
      oput$PERC_Tm3 <- oput$PERC*(0.03937/3.93700787401575)
      oput$PERC_in <- oput$PERC*0.03937 # = (PERC, mm)*(0.0393701 in/1mm)
        
      ## Convert to lbs/ac from kg/ha or t/ha
      oput$Ru_N_kg <- with(oput,(ORGN+NSURQ))
      oput$Ru_N_lbs <- oput$Ru_N*0.89217943789
      oput$Ru_P_kg <- with(oput,(ORGP+SEDP+SOLP))
      oput$Ru_P_lbs <- oput$Ru_P*0.89217943789
      oput$RCHG_N_lbs <- oput$RCHG_N*0.89217943789
      oput$SYLD_kg <- oput$SYLD*1000
      oput$SYLD_lbs <- oput$SYLD*892.179   
      ## Convert precipitation and temperature
      # Precipitation from mm to in
      oput$PRECIP_in <- oput$PRECIP * 0.03937
      # Temperature from deg C to deg F
      oput$TMP_AV_degF <- (oput$TMP_AV * (9/5)) + 32
        
      oput$date <- as.Date(with(oput,format(as.Date(paste0(YR,"-",MO,"-",DA)),"%m/%d/%Y")), "%m/%d/%Y") 
       
      # Sort by date
      oput <- oput[order(oput$date),]
        
      # Filter to only dates specified by user and columns of interest
      oput_rev <- oput[(oput$date>=StartDate)&
                                     (oput$date<=EndDate),] %>%
        select("date","MO","DA","YR","LULC","HRU","SUB","AREAkm2","LATQGEN","GW_Q","SA_ST","SW_INIT","PRECIP","PRECIP_in","PET",
		 "TMP_AV","TMP_AV_degF","SURQ_CNT_Tm3","SURQ_CNT_in","Ru_N_kg","Ru_N_lbs","Ru_P_kg","Ru_P_lbs","SYLD_kg","SYLD_lbs",
		 "PERC","PERC_Tm3","PERC_in","RCHG_N","RCHG_N_lbs")
    
      oput_rev$Mgmt_Set <- user_specs$Managed_Set[user_specs$Scenario_Name==Scen]
      oput_rev$Clim_Scen <- user_specs$Climate_Scenario[user_specs$Scenario_Name==Scen]
        
      ## Bind together 
      if (exists("oput_bind")) {
        oput_bind <- rbind(oput_bind, oput_rev)
      } else {
        oput_bind <- oput_rev
      }
  }
}

#-DB-# If user extracted .gw, .sol, and urban.dat information from SWAT run database,
#-DB-# they would need to merge with climate scenario-specific oput file information (.hru). 
#-DB-# This would be a data frame with header information, calculated infiltration rate (.sol), 
#-DB-# and calculated EIA (urban.dat) by managed set and climate scenario.
#-DB-# See "Example_oput_gw.csv" within ExampleFiles folder for what must be extracted from database.

oput_gw <- merge(merge(merge(oput_bind,GW_bind,by=c("SUB","HRU","Mgmt_Set"),all.x=TRUE), ### LULC differs between oput_bind and GW_bind
                 distinct(sol_bind[,c("SUB","HRU","Mgmt_Set","infil_rate")]),by=c("SUB","HRU","Mgmt_Set"),all.x=TRUE),
                 urb_form[,c("LULC_Gen","EIA")],by=c("LULC_Gen"),all.x=TRUE)



# Based on user selection above, potentially add direct deposition to stream 
# from cattle grazers to RNGE HRU runoff loadings: 0.000070832 lb TP/acre/day
oput_gw$Ru_P_lbs <- ifelse(dir_dep=="YES"&oput_gw$LULC_Gen=="RNGE", oput_gw$Ru_P_lbs+0.000070832,oput_gw$Ru_P_lbs)

## NOTE TO USER: if you do not want to add direct deposition when runoff loadings are 0, use the following code and 
## comment out the line above.
# oput_gw$Ru_P_lbs <- ifelse(dir_dep=="YES"&oput_gw$LULC_Gen=="RNGE"&oput_gw$Ru_P_lbs>0, 
# 			oput_gw$Ru_P_lbs+0.000070832,oput_gw$Ru_P_lbs)

# Convert from mg to lbs and kg
# oput_gw$Re_P_lbs <- with(oput_gw,(((PERC/1e+6)*AREAkm2)*1e+12)*GWSOLP/453592,na.omit=TRUE)
# lb/acre: m x 1/m3 x mg/L x L/m3 x lb/453,952mg x 1m2/0.00024711 acre 
## NOTE TO USER: The value used in the equation below is specific to the Upper Soldier Creek case study. Change this
## constant value to the "GWSOLP" variable if modeling a different area.
oput_gw$Re_P_lbs <- with(oput_gw,PERC*0.04*(1/453952 )*(1/0.00024711),na.omit=TRUE) 
oput_gw$Re_P_kg <- oput_gw$Re_P_lbs/2.205
    
oput_gw[is.na(oput_gw)] <- 0
    
HRUS_agg$AGG_HRU <- as.factor(HRUS_agg$AGG_HRU)


temp_oput_gw_area <- unique(as.data.frame(merge(oput_gw,HRUS_agg,by=c("SUB","HRU"),all.x=TRUE)) %>%
                              select(c("SUB","HRU","AGG_HRU","AREAkm2","Mgmt_Set","Clim_Scen"))) 

oput_gw_area <- temp_oput_gw_area %>%
  group_by(AGG_HRU,Mgmt_Set,Clim_Scen) %>%
  summarise_all(list(sum)) 

oput_gw_agg <- as.data.frame(merge(oput_gw,HRUS_agg,by=c("SUB","HRU"),all.x=TRUE)) %>% 
  select(c("Mgmt_Set","Clim_Scen","date","MO","DA","YR","AREAkm2","LATQGEN","GW_Q","SA_ST","SW_INIT","PRECIP","PRECIP_in",
  	   "PET","TMP_AV","TMP_AV_degF","SURQ_CNT_Tm3","SURQ_CNT_in","Ru_N_kg","Ru_N_lbs","Ru_P_kg","Ru_P_lbs","SYLD_kg",
	   "SYLD_lbs","PERC_Tm3","PERC_in","RCHG_N","RCHG_N_lbs","Re_P_lbs","Re_P_kg","AGG_HRU","EIA","infil_rate")) %>%
  group_by(AGG_HRU,date,MO,DA,YR,Mgmt_Set,Clim_Scen) %>%
  summarise_all(funs(wmean=sum(.*AREAkm2)/sum(AREAkm2)))
    
oput_gw_agg$AREAkm2_wmean <- NULL
oput_gw_agg$Re_SYLD_wmean <- 0
    
oput_gw_agg <- merge(oput_gw_agg,oput_gw_area,by=c("AGG_HRU","Mgmt_Set","Clim_Scen"),all=TRUE)
    
# Add sequence number
oput_gw_agg <- oput_gw_agg[order(oput_gw_agg$AGG_HRU,oput_gw_agg$date),]
oput_gw_seq <- as.data.frame(unique(oput_gw_agg$date))
oput_gw_seq$seq_no <- seq.int(nrow(oput_gw_seq))
names(oput_gw_seq) <- c("date","seq_no")
    
oput_final_bind <- merge(oput_gw_agg,oput_gw_seq,by=c("date"),all.x=TRUE)
    
oput_final_bind$KGw <- with(oput_final_bind, (LATQGEN_wmean+GW_Q_wmean)/(SA_ST_wmean+SW_INIT_wmean))
    
KGw_avg <- as.data.frame(oput_final_bind %>% select(c("Clim_Scen","KGw")) %>%
  group_by(Clim_Scen) %>%
  summarise_all(funs(mean)))
    
## Monthly totals dataset

# Develop monthly sequence value so if more than 1 year of analysis selected, Jan of second year = 13
oput_mo_seq <- as.data.frame(unique(oput_final_bind[,c("MO","YR")])) ## Need to re-add "YR" to dataframes
oput_mo_seq$mo_seq_no <- seq.int(nrow(oput_mo_seq))
names(oput_mo_seq) <- c("MO","YR","mo_seq_no")

oput_final_month <- merge(oput_final_bind,oput_mo_seq,by=c("MO","YR"),all.x=TRUE) %>% 
  select(c("Mgmt_Set","Clim_Scen","mo_seq_no","SURQ_CNT_in_wmean","Ru_N_lbs_wmean","Ru_P_lbs_wmean",
           "SYLD_lbs_wmean","PERC_in_wmean","RCHG_N_lbs_wmean","Re_P_lbs_wmean")) %>%
  group_by(mo_seq_no,Mgmt_Set,Clim_Scen) %>%
  summarise_all(funs(sum=sum(.)))

oput_final_month$Re_SYLD_wmean_sum <- 0

##########################################
# PART 3: DEVELOP WDATA_SCENARIO.DAT FILES  
##########################################

# Read in template for Wdata
temp_wdata <- readLines(paste0(inpath,"Wdata.dat"))

## Loop through climate scenarios
for (ClimScen in 1:length(clim_vec)) {

  Clim <- clim_vec[ClimScen]
  
  print(paste0("Printing Wdata.dat for Scenario = ", Clim))
  
  ## Loop through constituents
  ## NOTE TO USER: If not running all constituents, edit const <- c("TN","TP","TSS") character vector 
  ## (See VARIABLES TO ADJUST section) to remove the unwanted constituent.
  
  for (C in 1:length(const)) {
    c <- const[C]
    ru <- ru_const_lbs[C]
    re <- re_const_lbs[C]
    
    write(temp_wdata, file = paste0(outpath,"Wdata_Scen_",Clim,"_",c,".dat"))
    
    ## Loop through managed sets
    for (RunMgmt in 1:length(mgmt_vec)) {

      Mgmt <- mgmt_vec[RunMgmt]
      
      ## Loop through aggregate HRU IDs, as specified in UserSpecs.csv
      for (HRU in 1:length(agg_HRU_vec)) {
      
        hru <- agg_HRU_vec[HRU]
      
        # Runoff - Hydrology
        write(paste0("param QRuU",HRU,Mgmt," :="),file=paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""),sep="\n", append = T)
        write.table(oput_final_bind[oput_final_bind$AGG_HRU==hru&oput_final_bind$Clim_Scen==Clim&oput_final_bind$Mgmt_Set==Mgmt,c("seq_no","SURQ_CNT_Tm3_wmean")],
                    file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""), 
                    append = T,
                    sep = "\t",
                    row.names = F,
                    col.names = F,
                    na="",
                    quote = F)
        write( ";", file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""),sep="\n", append = T)
        
        # Recharge - Hydrology
        cat(paste0("\n"),file=paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""),sep="", append = T)
        cat(paste0("param QReU",HRU,Mgmt," :="),file=paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""),sep="\n", append = T)
        write.table(oput_final_bind[oput_final_bind$AGG_HRU==hru&oput_final_bind$Clim_Scen==Clim&oput_final_bind$Mgmt_Set==Mgmt,c("seq_no","PERC_Tm3_wmean")],
                    file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""), 
                    append = T,
                    sep = "\t",
                    row.names = F,
                    col.names = F,
                    na="",
                    quote = F)
        write( ";", file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""), append = T)
          
        # Runoff - Constituents
        cat(paste0("\nparam LRuU",HRU,Mgmt,"1"," :="), file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""),sep="\n", append = T)
        write.table(oput_final_bind[oput_final_bind$AGG_HRU==hru&oput_final_bind$Clim_Scen==Clim&oput_final_bind$Mgmt_Set==Mgmt,c("seq_no",ru)], ## May need to code c(ru)
                    file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""), 
                    append = T,
                    sep = "\t",
                    row.names = F,
                    col.names = F,
                    na="",
                    quote = F)
        write( ";", file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""), append = T)
        
        # Recharge - Constituents
        cat(paste0("\nparam LReU",HRU,Mgmt,"1"," :="), file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""),sep="\n", append = T)
        write.table(oput_final_bind[oput_final_bind$AGG_HRU==hru&oput_final_bind$Clim_Scen==Clim&oput_final_bind$Mgmt_Set==Mgmt,c("seq_no",re)],
                    file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""), 
                    append = T,
                    sep = "\t",
                    row.names = F,
                    col.names = F,
                    na="",
                    quote = F)
        write( ";", file = paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""),sep="\n", append = T)
        cat(paste0("\n"),file=paste(outpath,"Wdata_Scen_",Clim,"_",c,".dat",sep=""),sep="", append = T)
      }
    }
  }
}

###########################################
# PART 4: DEVELOP WMODEL_SCENARIO.MOD FILES  
###########################################

### PART 4A: Develop temp_HRUSpecs.csv file

temp_HRUSpecs <- as.data.frame(oput_gw_area) %>% 
  select(c("AGG_HRU","AREAkm2")) %>%
  group_by(AGG_HRU) %>%
  summarise_all(funs(mean))

names(temp_HRUSpecs) <- c("WMOST_HRU_Name","Baseline_Area_km2")

temp_HRUSpecs$Baseline_Area_acre <- with(temp_HRUSpecs,Baseline_Area_km2*247.105)

temp_HRUSpecs$WMOST_HRU_ID <- match(temp_HRUSpecs$WMOST_HRU_Name,agg_HRU_vec)

temp_HRUSpecs <- temp_HRUSpecs[,c("WMOST_HRU_ID","WMOST_HRU_Name","Baseline_Area_acre")]

temp_HRUSpecs <- temp_HRUSpecs[order(temp_HRUSpecs$WMOST_HRU_ID),]

for (RunMgmt in 1:length(mgmt_vec)) {
  
  Mgmt <- mgmt_vec[RunMgmt]
  
  Min_var <- paste0("Minimum_Area_Managed_Set_",Mgmt,"_acre")
  Max_var <- paste0("Maximum_Area_Managed_Set_",Mgmt,"_acre")
  CC_var <- paste0("Initial_Cost_to_Conserve_Managed_Set_",Mgmt,"_peracre")
  OM_var <- paste0("OM_Cost_Managed_Set_",Mgmt,"_peracre")
  
  temp_HRUSpecs[,Min_var] <- ""
  temp_HRUSpecs[,Max_var] <- ""
  temp_HRUSpecs[,CC_var] <- ""
  temp_HRUSpecs[,OM_var] <- ""
}

temp_HRUSpecs[,"Interest_Rate_perc"] <- ""
temp_HRUSpecs[,"Planning_Horizon_yrs"] <- ""

write.csv(temp_HRUSpecs,paste0(inpath,"temp_HRUSpecs.csv"),row.names = FALSE)

## Message to users to update temp_HRUSpecs.csv
Message_2 <- "NOTE TO USER: Fill in area and cost data in temp_HRUSpecs.csv and save as HRUSpecs.csv prior to running remaining code. Both should be saved in the Inputs folder."
Message_2

### PART 4B: Develop Wmodel_Scen_.mod files based on user specifications in the HRUSpecs.csv file

hru_specs <- read.csv(paste0(inpath,"HRUSpecs.csv"))

# Read in template for Wmodel.mod
temp_wmodel <- readLines(paste0(inpath,"Wmodel.mod"))

wmodel <- temp_wmodel

FTPlan <- (hru_specs[1,"Interest_Rate_perc"]/100*(1+hru_specs[1,"Interest_Rate_perc"]/100)^
             hru_specs[1,"Planning_Horizon_yrs"]) / ((1+hru_specs[1,"Interest_Rate_perc"]/100)^
                                                       hru_specs[1,"Planning_Horizon_yrs"]-1)

## Loop through climate scenarios
for (ClimScen in 1:length(clim_vec)) {
  
  Clim <- clim_vec[ClimScen]
  
  print(paste0("Printing Wmodel.mod for Scenario = ", Clim))
  
  text_find_gw <- "subject to gw_sw_one"
  split_gw <- str_split(temp_wmodel[grep(text_find_gw, temp_wmodel)+1], "[ =;]+")
  gw <- as.numeric(split_gw[[1]][3])
  wmodel[grep(text_find_gw, wmodel)+1] <- paste0("  QGwSw[first(time)] = ",gw*KGw_avg$KGw[KGw_avg$Clim_Scen==Clim],";")

  wmodel[grep(text_find_gw, wmodel)+4] <- paste0("  QGwSw[t] = ",KGw_avg$KGw[KGw_avg$Clim_Scen==Clim]," * VGw[t-1];")

  counter_cluset <- 1
  temp_lusubtotal <- ""
  temp_cluset <- ""

  
  ## Loop through managed sets
  for (RunMgmt in 1:length(mgmt_vec)) {
    
    Mgmt <- mgmt_vec[RunMgmt]
    
    ALuBase <- "Baseline_Area_acre"
    ALuMin <- paste0("Minimum_Area_Managed_Set_",Mgmt,"_acre")
    ALuMax <- paste0("Maximum_Area_Managed_Set_",Mgmt,"_acre")
    CCLu <- paste0("Initial_Cost_to_Conserve_Managed_Set_",Mgmt,"_peracre")
    COmLu <- paste0("OM_Cost_Managed_Set_",Mgmt,"_peracre")
    
    ## Loop through aggregate HRU IDs, as specified in UserSpecs.csv
    for (HRU in 1:length(agg_HRU_vec)) {
      
      if (Mgmt==1) {
        text_find <- paste0("var DALu",HRU,Mgmt," >=")
        wmodel[grep(text_find, wmodel)] <- 
          paste0("var DALu",HRU,Mgmt," >=",(hru_specs[HRU, ALuMin]/2.471053815)," <=",(hru_specs[HRU, ALuMax]/2.471053815),
                 " :=",(hru_specs[HRU, ALuBase]/2.471053815),";")


        
        if (hru_specs[HRU, CCLu] >= 0) { 
          if (HRU==1) {
            temp_cluset <- paste0(FTPlan*hru_specs[HRU, CCLu]/0.404685642," * DALu",HRU,Mgmt," - ",
                                  FTPlan*hru_specs[HRU, CCLu]/0.404685642*hru_specs[HRU, ALuBase]/2.471053815,
                                  " + ",hru_specs[HRU, COmLu]/0.404685642," * DALu",HRU,Mgmt," - ",
                                  hru_specs[HRU, COmLu]/0.404685642*hru_specs[HRU, ALuBase]/2.471053815)
          } else {
            cluset <- paste0(temp_cluset, " + ",FTPlan*hru_specs[HRU, CCLu]/0.404685642," * DALu",HRU,Mgmt," - ",
                             FTPlan*hru_specs[HRU, CCLu]/0.404685642*hru_specs[HRU, ALuBase]/2.471053815,
                             " + ",hru_specs[HRU, COmLu]/0.404685642," * DALu",HRU,Mgmt," - ",
                             hru_specs[HRU, COmLu]/0.404685642*hru_specs[HRU, ALuBase]/2.471053815)
          }
        }

        text_find <- paste0("subject to dalu",HRU,Mgmt,":")
        if (hru_specs[HRU, CCLu] >= 0) {
          wmodel[grep(text_find, wmodel)+1] <- paste0("  DALu",HRU,Mgmt," - ",hru_specs[HRU, ALuBase]/2.471053815," >= 0;")
        } else {
          wmodel[grep(text_find, wmodel)+1] <- paste0("  DALu",HRU,Mgmt," - ",hru_specs[HRU, ALuBase]/2.471053815," <= 0;")
        }
        
      } else {
        text_find <- paste0("var DALu",HRU,Mgmt," >=")
        wmodel[grep(text_find, wmodel)] <- 
          paste0("var DALu",HRU,Mgmt," >=",(hru_specs[HRU, ALuMin]/2.471053815)," <=",
                 (hru_specs[HRU, ALuMax]/2.471053815),";")
      
        if (hru_specs[HRU, CCLu] >= 0) {
          if (counter_cluset==1) {
            cluset_managed <- paste0(FTPlan*hru_specs[HRU, CCLu]/0.404685642," * DALu",HRU,Mgmt," + ",
                                     hru_specs[HRU, COmLu]/0.404685642," * DALu",HRU,Mgmt)
            counter_cluset <- 2
          } else {
            cluset_managed <- paste0(cluset_managed, " + ",FTPlan*hru_specs[HRU, CCLu]/0.404685642," * DALu",HRU,
                                     Mgmt," + ",hru_specs[HRU, COmLu]/0.404685642," * DALu",HRU,Mgmt)
            counter_cluset <- counter_cluset + 1
          }
        } else {
          text_find_1 <- paste0("subject to alu_excl",HRU,Mgmt,":")
          if(text_find_1 %in% wmodel==TRUE) {
            wmodel[grep(text_find_1, wmodel)+1] <- paste0("  DALu",HRU,Mgmt," = 0;")           
          } else {
            wmodel[length(wmodel)+1] <- ""
            wmodel[length(wmodel)+1] <- paste0("subject to alu_excl",HRU,Mgmt,":")
            wmodel[length(wmodel)+1] <- paste0("  DALu",HRU,Mgmt," = 0;")
          }
        }
        
        if (Mgmt==2) {
          temp_lusubtotal <- paste0("  DALu",HRU,"1 >= DALu",HRU,Mgmt,";")
        } else {
          lusubtotal <- paste0(temp_lusubtotal, " + ","DALu",HRU,Mgmt,";")
        }
        
        text_find <- paste0("subject to alu_subtotal",HRU)
        if (length(mgmt_vec)>2) {
          wmodel[grep(text_find, wmodel)+1] <- paste0(lusubtotal,";")
        } else {
          wmodel[grep(text_find, wmodel)+1] <- paste0(temp_lusubtotal,";")
        }
      }
    }
    
    if (Mgmt==1) {
      text_find <- paste0("subject to c_luset1")   
      if (temp_cluset=="") {
        wmodel[grep(text_find, wmodel)+1] <- paste0("  CLuSet",Mgmt," = 0;")
      } else {
        wmodel[grep(text_find, wmodel)+1] <- paste0("  CLuSet",Mgmt," = ",cluset,";")
      }
    } else {
      text_find <- paste0("subject to c_luset",Mgmt)
      if (counter_cluset==1 & cluset_managed=="") {

        wmodel[grep(text_find, wmodel)+1] <- paste0("  CLuSet",Mgmt," = 0;")
      } else if((counter_cluset = 1 & !(cluset_managed == "")) | counter_cluset > 1) {
        # wmodel[grep(text_find, wmodel)+1] <- paste0("  CLuSet",Mgmt," = ",temp_cluset_managed,";") 
        wmodel[grep(text_find, wmodel)+1] <- paste0("  CLuSet",Mgmt," = ",cluset_managed,";") 

      } 
    }
    
  }
  write(wmodel, file = paste0(outpath,"Wmodel_Scen_",Clim,".mod"))
}

###########################################
# PART 5: DEVELOP SPECSRESULTS.CSV FILES  
###########################################

## Message to users to update temp_HRUSpecs.csv
Message_3 <- "NOTE TO USER: Your Specs Results file (_LogFileTEMP.csv output from WMOSTv3.1) must be renamed to template_SpecsResults.csv prior to running this code."
Message_3

temp_template_sr_part1 <- read.csv(paste0(inpath,"template_SpecsResults.csv"),header=TRUE,nrows=7)

# Remove any NA rows (if they show up)
template_sr_part1 <- temp_template_sr_part1[,colSums(is.na(temp_template_sr_part1))<nrow(temp_template_sr_part1)]
names(template_sr_part1) <- c("Variable","Identifier")
template_sr_part1$Value <- ""
template_sr_part1$Units <- ""

template_sr_part2 <- read.csv(paste0(inpath,"template_SpecsResults.csv"),header=FALSE,skip=8)
names(template_sr_part2) <- c("Variable","Identifier","Value","Units")

template_sr <- rbind(template_sr_part1,template_sr_part2)

for (ClimScen in 1:length(clim_vec)) {
  
  Clim <- clim_vec[ClimScen]

  print(paste0("Printing SpecsResults for Scenario = ", Clim))

  for (C in 1:length(const)) { 
    c <- const[C]
    ru <- ru_sum_const[C]
    re <- re_sum_const[C]
    
    # Up front information
    mod_details <- data.frame("Variable" = c("************Model Details************","StudyAreaName","ScenarioName","RunStartTime",
                                             "StartDate","EndDate","ModelMode","************Model Input Data************","Variable"),
                                "Identifier" = c("","Upper Soldier Creek",paste0("Scen_",Clim,"_",c),as.character(Sys.time()),
                                                 as.character(StartDate),as.character(EndDate),"Hydrology & Loadings","","Identifier"),
                              "Value" = c("","","","","","","","","Value"),
                              "Units" = c("","","","","","","","","Units"))
  
    write.table(mod_details,paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), row.names = FALSE, col.names=F, sep=",")
    
    mod_input_info <-  data.frame("Variable" = c("NDateHydro","Dt","NLu"),
                                  "Identifier" = c("None","None","None"),
                                  "Value"=c(max(oput_final_bind$seq_no),1,length(unique(oput_final_bind$AGG_HRU))),
                                  "Units"=c("time steps in model","days in time step","# HRUs"))
    
    write.table(mod_input_info,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)

    
    # NLuName
    mod_input_NLuName <- as.data.frame(hru_specs[,c("WMOST_HRU_ID","WMOST_HRU_Name")])
    mod_input_NLuName$Variable <- "NLuName"
    mod_input_NLuName$Units <- "-" 
    
    names(mod_input_NLuName) <- c("Identifier","Value","Variable","Units")
    mod_input_NLuName <- mod_input_NLuName[,c("Variable","Identifier","Value","Units")]
    
    write.table(mod_input_NLuName,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
  
    # ALuBase
    mod_input_ALuBase <- as.data.frame(hru_specs[,c("WMOST_HRU_ID","Baseline_Area_acre")])
    mod_input_ALuBase$Variable <- "ALuBase"
    mod_input_ALuBase$Units <- "acre"
    
    names(mod_input_ALuBase) <- c("Identifier","Value","Variable","Units")
    mod_input_ALuBase <- mod_input_ALuBase[,c("Variable","Identifier","Value","Units")]
    
    write.table(mod_input_ALuBase,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
  
    # EIALu
    EIALu <- oput_final_bind %>% select(c("Mgmt_Set","Clim_Scen","AGG_HRU","EIA_wmean")) %>%
      group_by(Mgmt_Set,Clim_Scen,AGG_HRU) %>% summarise_all(funs(mean))
    
    names(EIALu) <- c("Mgmt_Set","Clim_Scen","WMOST_HRU_Name","EIA")
    
    mod_input_EIALu <- as.data.frame(merge(hru_specs[,c("WMOST_HRU_ID","WMOST_HRU_Name")],
                                           unique(EIALu[EIALu$Clim_Scen==Clim,c("WMOST_HRU_Name","EIA")]),
                                           by=c("WMOST_HRU_Name"),all.x=TRUE))

    mod_input_EIALu$Variable <- "EIALu"
    mod_input_EIALu$Units <- "fraction"
    
    mod_input_EIALu <- mod_input_EIALu[,c("Variable","WMOST_HRU_ID","EIA","Units")]
    names(mod_input_EIALu) <- c("Variable","Identifier","Value","Units")
    mod_input_EIALu <- mod_input_EIALu[order(mod_input_EIALu$Identifier),]
    
    write.table(mod_input_EIALu,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
    
    # InfiltLu
    InfiltLu <- oput_final_bind %>% select(c("AGG_HRU","infil_rate_wmean")) %>%
      group_by(AGG_HRU) %>% summarise_all(funs(mean))
    
    names(InfiltLu) <- c("WMOST_HRU_Name","infil_rate")
    
    mod_input_InfiltLu <- as.data.frame(merge(hru_specs[,c("WMOST_HRU_ID","WMOST_HRU_Name")],
                                              InfiltLu,by=c("WMOST_HRU_Name"),all.x=TRUE))
    mod_input_InfiltLu$Variable <- "InfiltLu"
    mod_input_InfiltLu$Units <- "in/hr"
    
    mod_input_InfiltLu <- mod_input_InfiltLu[,c("Variable","WMOST_HRU_ID","infil_rate","Units")] 
    names(mod_input_InfiltLu) <- c("Variable","Identifier","Value","Units")
    mod_input_InfiltLu <- mod_input_InfiltLu[order(mod_input_InfiltLu$Identifier),]
    
    write.table(mod_input_InfiltLu,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
    
    # NLuSet
    mod_input_NLuSet <- data.frame("Variable" = "NLuSet",
                                   "Identifier" = "None","Value"=length(mgmt_vec),
                                   "Units"="# of HRU Sets")
    
    # NLuSetName
    NLuSetName <- user_specs[!is.na(user_specs$Climate_Scenario)&(user_specs$Climate_Scenario==Clim),c("Scenario_Name","Managed_Sets")]
    NLuSetName$Scenario_Name_Rev <- gsub("\\\\|[^[:print:]]", "/", NLuSetName$Scenario_Name)  ## May need to gsub "//" instead
    NLuSetName$Value <- gsub("/","_",sub(".*\\Results/", "", NLuSetName$Scenario_Name_Rev)) ## Change based on subfolder name
    
    mod_input_NLuSetName <- as.data.frame(NLuSetName[,c("Managed_Sets","Value")])
    names(mod_input_NLuSetName) <- c("Identifier","Value")
    
    mod_input_NLuSetName$Variable <- "NLuSetName"
    mod_input_NLuSetName$Units <- "-"
    mod_input_NLuSetName <- mod_input_NLuSetName[,c("Variable","Identifier","Value","Units")]
    
    write.table(mod_input_NLuSetName,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)

    # Append with data from template file (Data between AgCN and before KGw, where this code picks up again)
    template_sr_mid <- template_sr
    start <- which(template_sr_mid$Variable == "AgCN")
    end   <- which(template_sr_mid$Variable == "KGw")
    template_sr_mid <- template_sr_mid[start[1]:(end-1), ]
    
    write.table(template_sr_mid,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
    
    mod_input_KGw <- data.frame("Variable" = c("KGw"),
                                "Identifier" = c("None"),
                                "Value" = KGw_avg$KGw[KGw_avg$Clim_Scen==Clim],
                                "Units" = c("1/time step"))
    
    write.table(mod_input_KGw,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"),
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
    
    # Append with data from template file (Data between VGwI and before Cons, where this code picks up again)
    template_sr_mid2 <- template_sr
    start_2 <- which(template_sr_mid2$Variable == "VGwI")
    end_2 <- which(template_sr_mid2$Variable == "Cons")
    template_sr_mid2 <- template_sr_mid2[start_2:(end_2-1), ]
    
    write.table(template_sr_mid2,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
    
    mod_input_Cons <-  data.frame("Variable" = c("Cons"),
                                  "Identifier" = c("1"),
                                  "Value"=c, 
                                  "Units"=c("-"))
    
    write.table(mod_input_Cons,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
    
    template_sr_end <- template_sr
    start_3 <- which(template_sr_end$Variable == "CStreamU")
    end_3   <- which(template_sr_end$Variable == "MeasuredConc"&template_sr_end$Identifier == 
                       paste0(max(oput_final_bind$seq_no),";1"))
    template_sr_end <- template_sr_end[start_3:end_3, ]
    
    write.table(template_sr_end,
                file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                append = T,
                sep = ",",
                row.names = F,
                col.names = F,
                na="",
                quote = F)
    
    ## Loop through managed sets
    for (RunMgmt in 1:length(mgmt_vec)) {
      
      Mgmt <- mgmt_vec[RunMgmt]
      
      ID_var <- paste0("Identifier_",Mgmt)
      
      Min_var <- paste0("Minimum_Area_Managed_Set_",Mgmt,"_acre")
      Max_var <- paste0("Maximum_Area_Managed_Set_",Mgmt,"_acre")
      CC_var <- paste0("Initial_Cost_to_Conserve_Managed_Set_",Mgmt,"_peracre")
      OM_var <- paste0("OM_Cost_Managed_Set_",Mgmt,"_peracre")

      # ALuMin
      temp_ALuMin <- hru_specs %>% dplyr::select(starts_with("WMOST"),all_of(Min_var))
      temp_ALuMin[,ID_var] <- with(temp_ALuMin,paste0(WMOST_HRU_ID,";",Mgmt))
    
      temp_ALuMin$Variable <- "ALuMin"
      temp_ALuMin$Units <- "acre"

      mod_input_ALuMin <- temp_ALuMin[,c("Variable",ID_var,Min_var,"Units")]
      names(mod_input_ALuMin) <- c("Variable","Identifier","Value","Units")
      
      write.table(mod_input_ALuMin,
                  file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                  append = T,
                  sep = ",",
                  row.names = F,
                  col.names = F,
                  na="",
                  quote = F)
      
      # ALuMax
      temp_ALuMax <- hru_specs %>% dplyr:: select(starts_with("WMOST"),all_of(Max_var))
      temp_ALuMax[,ID_var] <- with(temp_ALuMax,paste0(WMOST_HRU_ID,";",Mgmt))
      
      temp_ALuMax$Variable <- "ALuMax"
      temp_ALuMax$Units <- "acre"

      mod_input_ALuMax <- temp_ALuMax[,c("Variable",ID_var,Max_var,"Units")]
      names(mod_input_ALuMax) <- c("Variable","Identifier","Value","Units")
      
      write.table(mod_input_ALuMax,
                  file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                  append = T,
                  sep = ",",
                  row.names = F,
                  col.names = F,
                  na="",
                  quote = F)
  
      # CCLu
      temp_CCLu <- hru_specs %>% dplyr:: select(starts_with("WMOST"),all_of(CC_var))
      temp_CCLu[,ID_var] <- with(temp_CCLu,paste0(WMOST_HRU_ID,";",Mgmt))
      
      temp_CCLu$Variable <- "CCLu"
      temp_CCLu$Units <- "$/acre"

      mod_input_CCLu <- temp_CCLu[,c("Variable",ID_var,CC_var,"Units")]
      names(mod_input_CCLu) <- c("Variable","Identifier","Value","Units")
      
      write.table(mod_input_CCLu,
                  file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                  append = T,
                  sep = ",",
                  row.names = F,
                  col.names = F,
                  na="",
                  quote = F)
      
      # COmLu
      temp_COmLu <- hru_specs %>% dplyr:: select(starts_with("WMOST"),all_of(OM_var))
      temp_COmLu[,ID_var] <- with(temp_COmLu,paste0(WMOST_HRU_ID,";",Mgmt))
      
      temp_COmLu$Variable <- "COmLu"
      temp_COmLu$Units <- "$/acre/yr"
      
      mod_input_COmLu <- temp_COmLu[,c("Variable",ID_var,OM_var,"Units")]
      names(mod_input_COmLu) <- c("Variable","Identifier","Value","Units")
      
      write.table(mod_input_COmLu,
                  file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"), 
                  append = T,
                  sep = ",",
                  row.names = F,
                  col.names = F,
                  na="",
                  quote = F)

      ## Monthly totals of runoff and recharge 
      # Runoff - Hydrology
      temp_QRuT <- oput_final_month[oput_final_month$Clim_Scen==Clim&oput_final_month$Mgmt_Set==Mgmt,
                                    c("mo_seq_no","SURQ_CNT_in_wmean_sum")]
      
      temp_QRuT$Variable <- "QRuT"
      temp_QRuT$Units <- "in"

      temp_QRuT[,ID_var] <- with(temp_QRuT,paste0(mo_seq_no,";",Mgmt))
      
      mod_input_QRuT <- temp_QRuT[,c("Variable",ID_var,"SURQ_CNT_in_wmean_sum","Units")]
      names(mod_input_QRuT) <- c("Variable","Identifier","Value","Units")

      write.table(mod_input_QRuT,
                  file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"),
                  append = T,
                  sep = ",",
                  row.names = F,
                  col.names = F,
                  na="",
                  quote = F)
      
      # Recharge - Hydrology
      temp_QReT <- oput_final_month[oput_final_month$Clim_Scen==Clim&oput_final_month$Mgmt_Set==Mgmt,
                                    c("mo_seq_no","PERC_in_wmean_sum")]

      temp_QReT$Variable <- "QReT"
      temp_QReT$Units <- "in"
      
      temp_QReT[,ID_var] <- with(temp_QReT,paste0(mo_seq_no,";",Mgmt))
      
      mod_input_QReT <- temp_QReT[,c("Variable",ID_var,"PERC_in_wmean_sum","Units")]
      names(mod_input_QReT) <- c("Variable","Identifier","Value","Units")
  
      write.table(mod_input_QReT,
                  file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"),
                  append = T,
                  sep = ",",
                  row.names = F,
                  col.names = F,
                  na="",
                  quote = F)
      
      # Runoff - Constituents
      temp_LRuT <- oput_final_month[oput_final_month$Clim_Scen==Clim&oput_final_month$Mgmt_Set==Mgmt,
                                    c("mo_seq_no",ru)]
      
      temp_LRuT[,ID_var] <- with(temp_LRuT,paste0(mo_seq_no,";",Mgmt,";1"))
      
      temp_LRuT$Variable <- "LRuT"
      temp_LRuT$Units <- "lbs"
      
      mod_input_LRuT <- temp_LRuT[,c("Variable",ID_var,ru,"Units")]
      names(mod_input_LRuT) <- c("Variable","Identifier","Value","Units")
     
      write.table(mod_input_LRuT,
                  file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"),
                  append = T,
                  sep = ",",
                  row.names = F,
                  col.names = F,
                  na="",
                  quote = F)
      
      # Recharge - Constituents
      temp_LReT <- oput_final_month[oput_final_month$Clim_Scen==Clim&oput_final_month$Mgmt_Set==Mgmt,
                                    c("mo_seq_no",re)]
      
      temp_LReT[,ID_var] <- with(temp_LReT,paste0(mo_seq_no,";",Mgmt,";1"))
  
      temp_LReT$Variable <- "LReT"
      temp_LReT$Units <- "lbs"
      
      mod_input_LReT <- temp_LReT[,c("Variable",ID_var,re,"Units")]
      names(mod_input_LReT) <- c("Variable","Identifier","Value","Units")

      write.table(mod_input_LReT,
                  file = paste0(outpath,"SpecsResults","_Scen_",Clim,"_",c,".csv"),
                  append = T,
                  sep = ",",
                  row.names = F,
                  col.names = F,
                  na="",
                  quote = F)
    }
  }
}

#############################################################
# PART 6: DEVELOP CLIMATE TIME SERIES AND OUTFILES.CSV FILES  
#############################################################

## Develop Climate_[climate_scenario].csv files

for (i in seq(clim_vec)) {
  Clim <- clim_vec[i]
  
  temp_oput_outFiles <-
    oput_bind[oput_bind$Clim_Scen == Clim &
                oput_bind$Mgmt_Set == mgmt_vec[1], ] # only need climate data from one of the managed sets
  
  
  # Add area-weighting to match HCAM output
  # Get average temp and precip per subbasin
  oput_outFiles <- temp_oput_outFiles %>%
    select(c("MO", "DA", "YR", "SUB", "PRECIP_in", "TMP_AV_degF", "AREAkm2")) %>%
    mutate(date = as.Date(paste(DA, MO, YR, sep = "-"), "%d-%m-%Y")) %>%
    group_by(date) %>%
   summarise(PRECIP = weighted.mean(PRECIP_in, w=AREAkm2, na.rm=T),
              TMP_AV = weighted.mean(TMP_AV_degF, w=AREAkm2, na.rm=T))


  oput_outFiles <- oput_outFiles[, c("date", "PRECIP", "TMP_AV")]
  
  names(oput_outFiles) <- c("Timestamp", "PREC", "TEMP")
  
  write.csv(oput_outFiles,
            paste0(outpath, "Climate_", Clim, ".csv"),
            row.names = FALSE)
}

## Develop outFiles.csv files
# If the model is "Hydrology Only", only need to print the files on one row.
# If the model is "Hydrology & Loadings", need to repeat the same files.

# Revise slash direction within outpath for outFiles.csv output
outpath_rev  <- gsub("/", "\\", outpath,fixed=TRUE)

if (template_sr_part1$Identifier[template_sr_part1$Variable == "ModelMode"] == "Hydrology & Loadings") {
  for (i in seq(const)) {
    c <- const[i]
    

    rm(outFile_bind,outFile)
    
    for (j in seq(clim_vec)) {
      Clim <- clim_vec[j]
      

      outFile <- data.frame(
        Clim_Run_Abbrev  = c(Clim, Clim),
        Clim_TS_Path = paste0(outpath_rev, "Climate_", Clim, ".csv"),
        SpecsResults_Path = paste0(outpath_rev, "SpecsResults", "_Scen_", Clim, "_", c, ".csv")
      )

      # Bind together
      if (exists("outFile_bind")) {
        outFile_bind <- rbind(outFile_bind, outFile)
      } else {
        outFile_bind <- outFile
      }
    }
      
      write.table(
        outFile_bind,
        paste0(outpath, "outFiles_", c, ".csv"),
        row.names = FALSE,
        col.names = FALSE,
        quote = FALSE,
        sep = ","
      )
    
  }
  
  
} else {
  for (i in seq(const)) {

    c <- const[i]
    
    rm(outFile_bind,outFile)
    

    
    for (j in seq(clim_vec)) {
      Clim <- clim_vec[j]
      
      

      outFile <- data.frame(
        Clim_Run_Abbrev  = Clim,
        Clim_TS_Path = paste0(outpath_rev, "Climate_", Clim, ".csv"),
        SpecsResults_Path = paste0(outpath_rev, "SpecsResults", "_Scen_", Clim, "_", c, ".csv")
      )
    
    }
    
    # Bind together
    if (exists("outFile_bind")) {
      outFile_bind <- rbind(outFile_bind, outFile)
    } else {
      outFile_bind <- outFile
    }
    
  }
    

    write.table(
      outFile_bind,
      paste0(outpath, "outFiles_", c, ".csv"),
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      sep = ","
    )
  
}



########## QA Code


for (i in seq(clim_vec)) {
  Clim <- clim_vec[i]
  
temp_oput_QA <-
    oput_final_bind[oput_final_bind$Clim_Scen == Clim &
                oput_final_bind$Mgmt_Set == mgmt_vec[1] & 
                  oput_final_bind$AGG_HRU == agg_HRU_vec[1], ] # only need climate data from one of the managed sets and QRu for HRU 1

  oput_QA <- temp_oput_QA[, c("date", "SURQ_CNT_in_wmean")]
  
  names(oput_QA) <- c("Timestamp", "QRuU_in")
  
  write.csv(oput_QA,
            paste0(outpath, "QA/AL/Climate_QRu_", Clim, ".csv"),
            row.names = FALSE)
}

for (i in seq(clim_vec)) {
  Clim <- clim_vec[i]
  
  temp_oput_QA <-
    oput_final_bind[oput_final_bind$Clim_Scen == Clim &
                      oput_final_bind$Mgmt_Set == mgmt_vec[1] & 
                      oput_final_bind$AGG_HRU == agg_HRU_vec[1], ] # only need climate data from one of the managed sets and QRu for HRU 1
  
  oput_QA <- temp_oput_QA[, c("date", "Ru_P_lbs_wmean")]
  
  names(oput_QA) <- c("Timestamp", "LRuU_lbs")
  

  
  write.csv(oput_QA,
            paste0(outpath, "QA/Climate_LRu_", Clim, ".csv"),
            row.names = FALSE)
}

for (i in seq(clim_vec)) {
  Clim <- clim_vec[i]
  
  temp_oput_QA <-
    oput_final_bind[oput_final_bind$Clim_Scen == Clim &
                      oput_final_bind$Mgmt_Set == mgmt_vec[1] & 
                      oput_final_bind$AGG_HRU == agg_HRU_vec[1], ] # only need climate data from one of the managed sets and QRu for HRU 1
  
  oput_QA <- temp_oput_QA[, c("date", "Re_P_lbs_wmean")]
  
  names(oput_QA) <- c("Timestamp", "LReU_lbs")
  
  write.csv(oput_QA,
            paste0(outpath, "QA/Climate_LRe_", Clim, ".csv"),
            row.names = FALSE)
}