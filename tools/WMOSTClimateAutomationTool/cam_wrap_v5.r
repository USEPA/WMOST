#########################################################################
#												                                                #
#	      R Code for HydroProcessor and HCAM Capabilities 		            #
#												                                                #
#########################################################################

# Authors: Kate Munson, Sam Ennett, Alyssa Le, affiliated with ICF, Inc.

# Clear global environment prior to running this code
rm(list = ls())

## Only required to install pacman once.
install.packages("pacman")

# Install and load packages
pacman::p_load(reader, dplyr, tidyr, stringr, data.table)

options(stringsAsFactors = FALSE)

## NOTE TO USER: This code combines the output files from HCAM-R and the HCAM to develop
## WMOST files for optimization of costs to meet loading targets based on several managed sets.

################################################
# VARIABLES TO ADJUST
################################################

## NOTE TO USER: we recommend setting up a folder structure designates separate input and output folders.
## The input folders should include WMOST optimization files and Specs Results files output from HCAM-R and HCAM.

inpath_HCAM <-
  "C:/Users/50367/ICF/WMOST - R_Code_Development/From_EPA/TP2024_030322/"
inpath_HCAMR <-
  "C:/Users/50367/ICF/WMOST - R_Code_Development/HCAM_R/Outputs/"
outpath <-
  "C:/Users/50367/ICF/WMOST - R_Code_Development/CAM_WRAP/Outputs/"

## NOTE: This code provides example for the AC, CE, and CM climate scenarios only; total phosphorus only
##       This code is also only meant to be run with the Upper Soldier Creek case study.

## Message to users to update filenames below
Message_1 <-
  "NOTE TO USER: add filenames to be read into CAMWRAP below."
Message_1

## Name of SpecsResults.csv files in inpath_HCAM\HCAM Processing\model_outputs and inpath_HCAMR
hcam_spec_filenames <-
  list(
    "Usc_Climate_SpecsResults_2.csv",
    "Usc_Climate_SpecsResults_3.csv",
    "Usc_Climate_SpecsResults_4.csv"
  )
hcamr_spec_filenames <-
  list(
    "SpecsResults_Scen_AC_TP.csv",
    "SpecsResults_Scen_CE_TP.csv",
    "SpecsResults_Scen_CM_TP.csv"
  )

## Name of Wdata.dat files in inpath_HCAM\HCAM Processing\model_outputs and inpath_HCAMR
hcam_wdata_filenames <-
  list("Wdata_2.dat",
       "Wdata_3.dat",
       "Wdata_4.dat")
hcamr_wdata_filenames <-
  list("Wdata_Scen_AC_TP.dat",
       "Wdata_Scen_CE_TP.dat",
       "Wdata_Scen_CM_TP.dat")

## Name of Wmodel.mod files in inpath_HCAM\HCAM Processing and inpath_HCAMR
hcam_wmodel_filenames <- list("Wmodel.mod")
hcamr_wmodel_filenames <-
  list("Wmodel_Scen_AC.mod",
       "Wmodel_Scen_CE.mod",
       "Wmodel_Scen_CM.mod")

## Name of climate time series files in inpath_HCAM\Time Series and inpath_HCAMR
# NOTE TO USER: this information is used for QA purposes only
hcam_climate_filenames <-
  list(
    "swat_USC2014_3huc12_hydrology_AC_2.csv",
    "swat_USC2014_3huc12_hydrology_CE_3.csv",
    "swat_USC2014_3huc12_hydrology_CM_4.csv"
  )
hcamr_climate_filenames <-
  list("Climate_AC.csv",
       "Climate_CE.csv",
       "Climate_CM.csv")

Message_2 <-
  "NOTE TO USER: specify climate scenario identifiers below, listed in the same order as climate_filenames."
Message_2

clim_vec <- c("AC", "CE", "CM")

################################################
# CONVERSION FACTORS
################################################

# Thousand cubic meters to millions of gallons
Tm3ToMG <- 0.264172052
# Hectare to acre
HaToAcre <- 2.471053815
# Dollar per hectare to dollar per acre
DpHaToDpAcre <- 0.404685642

################################################
# PART 1: READ IN WMOST FILES
################################################

## SpecsResults Files
hcam_specsresults <- list()
for (i in 1:length(hcam_spec_filenames)) {
  hcam_specsresults[[i]] <-
    read.csv(paste0(
      inpath_HCAM,
      "HCAM Processing/model_outputs/",
      hcam_spec_filenames[i]
    ))
  
  names(hcam_specsresults[[i]]) <-
    c("Variable", "Identifier", "Value", "Units")
  
}

hcamr_specsresults <- list()
for (i in 1:length(hcamr_spec_filenames)) {
  hcamr_specsresults[[i]] <-
    read.csv(paste0(inpath_HCAMR, hcamr_spec_filenames[i]))
  
  names(hcamr_specsresults[[i]]) <-
    c("Variable", "Identifier", "Value", "Units")
}

## Wdata Files
hcam_wdata <- list()
for (i in 1:length(hcam_wdata_filenames)) {
  hcam_wdata[[i]] <-
    read.csv(paste0(
      inpath_HCAM,
      "HCAM Processing/model_outputs/",
      hcam_wdata_filenames[i]
    ))
}

hcamr_wdata <- list()
for (i in 1:length(hcamr_wdata_filenames)) {
  hcamr_wdata[[i]] <-
    read.csv(paste0(inpath_HCAMR, hcamr_wdata_filenames[i])) # ReadLines?
}

## Wmodel Files
hcam_wmodel <- list()
for (i in 1:length(hcam_wmodel_filenames)) {
  hcam_wmodel[[i]] <-
    readLines(paste0(inpath_HCAM, "HCAM Processing/", hcam_wmodel_filenames[i]))
}

hcamr_wmodel <- list()
for (i in 1:length(hcamr_wmodel_filenames)) {
  hcamr_wmodel[[i]] <-
    readLines(paste0(inpath_HCAMR, hcamr_wmodel_filenames[i]))
}


hcam_climate <- list()
for (i in 1:length(hcam_climate_filenames)) {
  hcam_climate[[i]] <-
    read.csv(paste0(inpath_HCAM, "Time Series/", hcam_climate_filenames[i]))
}

hcamr_climate <- list()
for (i in 1:length(hcamr_climate_filenames)) {
  hcamr_climate[[i]] <-
    read.csv(paste0(inpath_HCAMR, hcamr_climate_filenames[i]))
}

###########################################
# PART 2: OUTLINE CAM-WRAP SPECIFICATIONS
###########################################

##### ----- SpecsResults.csv ----- #####


# Determine unique NLuSetName from SpecsResults files
# Assumes that first identifier will always be the base case

hcamr_set <-
  unique(hcamr_specsresults[[1]]$Value[hcamr_specsresults[[1]]$Variable ==
                                         "NLuSetName" &
                                         hcamr_specsresults[[1]]$Identifier > 1])
## Message to users on what managed sets are coming from HCAM-R
Message_3 <-
  paste0("NOTE TO USER: Managed set(s) modeled by HCAM-R: ", hcamr_set, ".")
Message_3

hcam_set <-
  unique(hcam_specsresults[[1]]$Value[hcam_specsresults[[1]]$Variable == "NLuSetName" &
                                        hcam_specsresults[[1]]$Identifier > 1])
Message_4 <-
  paste0("NOTE TO USER: Managed set(s) modeled by HCAM: ", hcam_set, ".")
Message_4

# Specify order of the variables in the SpecsResults.csv files
Specs_Order <- unique(hcamr_specsresults[[1]]$Variable)

# Specify variables for which identifiers will be adjusted
Specs_Adj <-
  c("ALuMin",
    "ALuMax",
    "CCLu",
    "COmLu",
    "QRuT",
    "QReT",
    "LRuT",
    "LReT")

# Specify variables to be adjusted in .dat files
wdata_Qvars <- c("QRuU", "QReU")
wdata_Lvars <- c("LRuU", "LReU")
wdata_QLvars <- c("QRuU", "QReU", "LRuU", "LReU")

# Initialize lists for SpecsResults.csv files
temp_set_id <- list()
NLuSetName_hcam <- list()
hcam_mgmt_identifier <- list()
hcam_HRUs <- list()
hcam_mgmt_info <- list()
hcam_mgmt_info_rev <- list()
hcam_mgmt_fin <- list()
bind_hcam_hcamr <- list()
hcamr_specsresults_append <- list()

# Initialize lists for Wdata.dat files

hcam_rownum <- list()
hcam_rownum_param <- list()
hcam_rownum_sc <- list()
hcam_rownum_expl <- list()
rownum_diff_hcam <- list()
hcamr_rownum <- list()
hcamr_rownum_param <- list()
hcamr_rownum_sc <- list()
hcamr_rownum_expl <- list()
rownum_diff_hcamr <- list()
rownum_LExtSw1 <- list()
wdata_q_l_hcamr <- list()
mgmt_set_wdata_uU <- list()
mgmt_set_wdata_eU <- list()
rownum_param <- list()
wdata_q_hcam <- list()
wdata_q_hcam_append <- list()
wdata_l_hcam <- list()
wdata_l_hcam_append <- list()
camwrap_ql_bind <- list()
wdata_ql_camwrap <- list()
wdata_ql_camwrap_append <- list()
camwrap_wdata_bind <- list()
wdata_LExtSw1_hcamr <- list()

# Initialize lists for Wmodel.mod files
camwrap_wmodel <- list()

# Define outFiles.csv data locations
outpath_specs  <- gsub("/", "\\", outpath,fixed=TRUE)
outpath_clim  <- gsub("/", "\\", inpath_HCAMR,fixed=TRUE)

###########################################
# PART 3: COMBINE WMOST FILES
###########################################

for (i in 1:length(clim_vec)) {
  clim <- clim_vec[i]
  
  ##### ----- SpecsResults.csv ----- #####
  
  ## Add on to HCAM-R identifier number
  
  # Obtain maximum managed set identifier from HCAM-R
  temp_set_id <-
    max(hcamr_specsresults[[i]]$Identifier[hcamr_specsresults[[i]]$Variable ==
                                             "NLuSetName"])
  
  # Create dataframe of original managed set identifiers from HCAM, minus the identifier for the baseline set
  NLuSetName_hcam[[i]] <-
    hcam_specsresults[[i]][hcam_specsresults[[i]]$Variable == "NLuSetName" &
                             hcam_specsresults[[i]]$Identifier > 1,]
  
  # Isolate unique original managed set identifiers from HCAM, minus the identifier for the baseline set
  hcam_mgmt_identifier <-
    unique(hcam_specsresults[[i]]$Identifier[hcam_specsresults[[i]]$Variable ==
                                               "NLuSetName" &
                                               hcam_specsresults[[i]]$Identifier > 1])
  
  # Isolate HRU identifiers from HCAM (should be the same as HRU identifiers in HCAM-R)
  hcam_HRUs <-
    unique(hcam_specsresults[[i]]$Identifier[hcam_specsresults[[i]]$Variable ==
                                               "NLuName"])
  
  # Adjust HCAM managed set identifiers to follow HCAM-R managed set identifiers numerically
  NLuSetName_hcam[[i]]$Identifier <-
    as.numeric(NLuSetName_hcam[[i]]$Identifier) + as.numeric(temp_set_id) - 1
  
  # Isolate HCAM SpecsResults.csv information that relates to managed sets
  
  hcam_mgmt_info[[i]] <-
    hcam_specsresults[[i]][hcam_specsresults[[i]]$Variable %in% Specs_Adj,]
  
  # Split up HRU, original managed set, and other information into separate columns
  hcam_mgmt_info_rev[[i]] <- mutate(
    hcam_mgmt_info[[i]],
    HRU_Month = # For runoff and recharge variables, the first number in the identifier refers to the month; for all other variables, this number refers to the HRU
      sapply(strsplit(Identifier, ";"), function(x)
        x[1]),
    
    temp_managed_set =
      sapply(strsplit(Identifier, ";"), function(x)
        x[2]),
    
    cons_num =
      sapply(strsplit(Identifier, ";"), function(x)
        x[3])
  )
  
  # Adjust HCAM managed set identifiers to follow HCAM-R managed set identifiers numerically
  hcam_mgmt_info_rev[[i]]$managed_set_rev <-
    as.numeric(hcam_mgmt_info_rev[[i]]$temp_managed_set) + as.numeric(temp_set_id) - 1
  
  # Isolate dataframe to non-baseline managed sets
  hcam_mgmt_info_rev[[i]] <-
    hcam_mgmt_info_rev[[i]][!(hcam_mgmt_info_rev[[i]]$temp_managed_set == 1),]
  
  # Concatenate HRU, adjusted managed set, and other information into a single column, separated by ";"
  hcam_mgmt_info_rev[[i]]$Identifier_rev <-
    ifelse(
      is.na(hcam_mgmt_info_rev[[i]]$cons_num),
      paste0(
        hcam_mgmt_info_rev[[i]]$HRU_Month,
        ";",
        hcam_mgmt_info_rev[[i]]$managed_set_rev
      ),
      paste0(
        hcam_mgmt_info_rev[[i]]$HRU_Month,
        ";",
        hcam_mgmt_info_rev[[i]]$managed_set_rev,
        ";",
        hcam_mgmt_info_rev[[i]]$cons_num
      )
    )
  
  # Finalize adjusted HCAM managed set dataframe
  hcam_mgmt_fin[[i]] <-
    hcam_mgmt_info_rev[[i]][, c("Variable", "Identifier_rev", "Value", "Units")]
  names(hcam_mgmt_fin[[i]]) <-
    c("Variable", "Identifier", "Value", "Units")
  
  hcam_mgmt_fin[[i]] <-
    rbind(hcam_mgmt_fin[[i]], NLuSetName_hcam[[i]][, c("Variable", "Identifier", "Value", "Units")])
  
  ## Loop based on order of variables in HCAM-R specsresults.csv file
  for (j in 1:length(Specs_Order)) {
    var <- Specs_Order[j]
    
    bind_hcam_hcamr[[i]] <-
      rbind(hcamr_specsresults[[i]][hcamr_specsresults[[i]]$Variable == var, c("Variable", "Identifier", "Value", "Units")],
            hcam_mgmt_fin[[i]][hcam_mgmt_fin[[i]]$Variable == var, c("Variable", "Identifier", "Value", "Units")])
    
    # Bind the specsresults variable data together
    if (exists("hcamr_specsresults_bind")) {
      hcamr_specsresults_bind <-
        rbind(hcamr_specsresults_bind, bind_hcam_hcamr[[i]])
    } else {
      hcamr_specsresults_bind <- bind_hcam_hcamr[[i]]
    }
    
  }
  
  # Select the bound results for each index of the climate scenario indices
  hcamr_specsresults_append[[i]] <- hcamr_specsresults_bind
  

  KGw <- c(as.numeric(hcam_specsresults[[i]]$Value[hcam_specsresults[[i]]$Variable == "KGw"]),
           as.numeric(hcamr_specsresults[[i]]$Value[hcamr_specsresults[[i]]$Variable == "KGw"]))
  
  
  hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable == "KGw"] <-
    mean(KGw)
  
  rm(hcamr_specsresults_bind)
  
  # Write to file
  # Up front information
  mod_details <- data.frame("Variable" = c("************Model Details************"),
                            "Identifier" = c(""),
                            "Value" = c(""),
                            "Units" = c(""))
  
  write.table(rbind(mod_details,hcamr_specsresults_append[[i]]),
              paste0(outpath, "SpecsResults_", clim, ".csv"),row.names = FALSE,
              col.names = FALSE, sep = ",")
  
  ##### ----- Wdata.dat ----- #####
  
  names(hcam_wdata[[i]]) <- c("Wdata")
  names(hcamr_wdata[[i]]) <- c("Wdata")
  
  ## Identify whether there is a consistent distance in number of rows between param and ;
  # HCAM
  hcam_rownum[[i]] <- hcam_wdata[[i]]
  hcam_rownum[[i]]$rownum <- seq.int(nrow(hcam_rownum[[i]]))
  
  hcam_rownum_param <-
    hcam_rownum[[i]][hcam_rownum[[i]]$Wdata %like% "param",]
  names(hcam_rownum_param) <- c("w_param", "rownum_param")
  
  hcam_rownum_sc[[i]] <-
    hcam_rownum[[i]][hcam_rownum[[i]]$Wdata %like% ";",]
  names(hcam_rownum_sc[[i]]) <- c("w_sc", "rownum_sc")
  
  hcam_rownum_expl[[i]] <-
    cbind(hcam_rownum_param, hcam_rownum_sc[[i]])
  
  hcam_rownum_expl[[i]]$rowdiff <-
    with(hcam_rownum_expl[[i]], rownum_sc - rownum_param)
  
  rownum_diff_hcam <- mean(hcam_rownum_expl[[i]]$rowdiff) # 366
  
  # HCAM-R
  hcamr_rownum[[i]] <- hcamr_wdata[[i]]
  hcamr_rownum[[i]]$rownum <- seq.int(nrow(hcamr_rownum[[i]]))
  
  hcamr_rownum_param <-
    hcamr_rownum[[i]][hcamr_rownum[[i]]$Wdata %like% "param",]
  names(hcamr_rownum_param) <- c("w_param", "rownum_param")
  
  hcamr_rownum_sc[[i]] <-
    hcamr_rownum[[i]][hcamr_rownum[[i]]$Wdata %like% ";",]
  names(hcamr_rownum_sc[[i]]) <- c("w_sc", "rownum_sc")
  
  hcamr_rownum_expl[[i]] <-
    cbind(hcamr_rownum_param, hcamr_rownum_sc[[i]])
  
  hcamr_rownum_expl[[i]]$rowdiff <-
    with(hcamr_rownum_expl[[i]], rownum_sc - rownum_param)
  
  rownum_diff_hcamr <- mean(hcamr_rownum_expl[[i]]$rowdiff) # 366
  
  # HCAM-R - subset the Wdata.dat file to just the information after "param LExtSw1 :="
  text_find_wdata <- "param LExtSw1 :="
  
  rownum_LExtSw1[[i]] <-
    hcamr_rownum[[i]]$rownum[hcamr_rownum[[i]]$Wdata == text_find_wdata]
  
  wdata_LExtSw1_hcamr[[i]] <-
    hcamr_rownum[[i]][1:(rownum_LExtSw1[[i]] + rownum_diff_hcamr), ]
  
  # Isolate runoff/recharge information
  wdata_q_l_hcamr[[i]] <-
    hcamr_rownum[[i]][(rownum_LExtSw1[[i]] + rownum_diff_hcamr + 1):max(hcamr_rownum[[i]]$rownum), ]
  
  # HCAM - Identify runoff and recharge parameters and adjust managed set order
  mgmt_set_wdata_uU[[i]] <-
    hcamr_wdata[[i]][hcamr_wdata[[i]]$Wdata %like% "uU",]
  
  mgmt_set_wdata_eU[[i]] <-
    hcamr_wdata[[i]][hcamr_wdata[[i]]$Wdata %like% "eU",]
  
  # Q values
  for (k in 1:length(wdata_Qvars)) {
    var <- wdata_Qvars[k]
    
    for (j in 1:length(hcam_mgmt_identifier)) {
      mgmt <- hcam_mgmt_identifier[j]
      
      for (h in 1:length(hcam_HRUs)) {
        HRU <- hcam_HRUs[h]
        
        rownum_param <-
          hcam_rownum[[i]]$rownum[hcam_rownum[[i]]$Wdata == paste0("param ", var, HRU, mgmt, " :=")]
        
        wdata_q_hcam[[i]] <-
          hcam_rownum[[i]][rownum_param:(rownum_param + rownum_diff_hcam), ]
        
        wdata_q_hcam[[i]]$Wdata <-
          gsub(paste0(mgmt, " :="),
               paste0(as.character(
                 as.numeric(mgmt) + as.numeric(temp_set_id) - 1
               ), " :="),
               wdata_q_hcam[[i]]$Wdata)
        
        # Bind together
        if (exists("wdata_q_hcam_bind")) {
          wdata_q_hcam_bind <- rbind(wdata_q_hcam_bind, wdata_q_hcam[[i]])
        } else {
          wdata_q_hcam_bind <- wdata_q_hcam[[i]]
        }
        
      }
      
    }
    
  }
  
  # Select the bound results for each index of the climate scenario indices
  wdata_q_hcam_append[[i]] <- wdata_q_hcam_bind
  
  rm(wdata_q_hcam_bind)
  
  # L values
  for (k in 1:length(wdata_Lvars)) {
    var <- wdata_Lvars[k]
    
    for (j in 1:length(hcam_mgmt_identifier)) {
      mgmt <- hcam_mgmt_identifier[j]
      
      for (h in 1:length(hcam_HRUs)) {
        HRU <- hcam_HRUs[h]
        
        rownum_param <-
          hcam_rownum[[i]]$rownum[hcam_rownum[[i]]$Wdata == paste0("param ", var, HRU, mgmt, "1 :=")]
        
        wdata_l_hcam[[i]] <-
          hcam_rownum[[i]][rownum_param:(rownum_param + rownum_diff_hcam), ]
        
        wdata_l_hcam[[i]]$Wdata <-
          gsub(paste0(mgmt, "1 :="),
               paste0(as.character(
                 as.numeric(mgmt) + as.numeric(temp_set_id) - 1
               ), "1 :="),
               wdata_l_hcam[[i]]$Wdata)
        
        # Bind together
        if (exists("wdata_l_hcam_bind")) {
          wdata_l_hcam_bind <- rbind(wdata_l_hcam_bind, wdata_l_hcam[[i]])
        } else {
          wdata_l_hcam_bind <- wdata_l_hcam[[i]]
        }
      }
      
    }
    
  }
  
  # Select the bound results for each index of the climate scenario indices
  wdata_l_hcam_append[[i]] <- wdata_l_hcam_bind
  
  rm(wdata_l_hcam_bind)
  
  # Compile all vars based on wdata_QLvars
  camwrap_mgmt <-
    unique(hcamr_specsresults_append[[i]]$Identifier[hcamr_specsresults_append[[i]]$Variable ==
                                                       "NLuSetName"])
  camwrap_ql_bind[[i]] <-
    rbind(wdata_q_l_hcamr[[i]],
          wdata_q_hcam_append[[i]],
          wdata_l_hcam_append[[i]])
  camwrap_ql_bind[[i]]$rownum <- seq.int(nrow(camwrap_ql_bind[[i]]))
  
  for (k in 1:length(wdata_QLvars)) {
    var <- wdata_QLvars[k]
      
    for (j in 1:length(camwrap_mgmt)) {
      mgmt <- camwrap_mgmt[j]
      
      for (h in 1:length(hcam_HRUs)) {
        HRU <- hcam_HRUs[h]
        
        rownum_param <-
          ifelse(
            var == "QRuU" |
              var == "QReU",
            camwrap_ql_bind[[i]]$rownum[camwrap_ql_bind[[i]]$Wdata == paste0("param ", var, HRU, mgmt, " :=")],
            camwrap_ql_bind[[i]]$rownum[camwrap_ql_bind[[i]]$Wdata == paste0("param ", var, HRU, mgmt, "1 :=")]
          )
        
        wdata_ql_camwrap[[i]] <-
          camwrap_ql_bind[[i]][rownum_param:(rownum_param + rownum_diff_hcam), ]
        
        # Bind together
        if (exists("wdata_ql_camwrap_bind")) {
          wdata_ql_camwrap_bind <-
            rbind(wdata_ql_camwrap_bind, wdata_ql_camwrap[[i]])
        } else {
          wdata_ql_camwrap_bind <- wdata_ql_camwrap[[i]]
        }
        
      }
      
    }
    
  }
  
  # Select the bound results for each index of the climate scenario indices
  wdata_ql_camwrap_append[[i]] <- wdata_ql_camwrap_bind
  
  rm(wdata_ql_camwrap_bind)
  
  # Bind together with initial .dat information
  camwrap_wdata_bind[[i]] <-
    as.data.frame(rbind(wdata_LExtSw1_hcamr[[i]], wdata_ql_camwrap_append[[i]]))
  
  # Write to file
  write(camwrap_wdata_bind[[i]][, c("Wdata")],
        paste0(outpath, "Wdata_", clim, ".dat"))
  
  ##### ----- Wmodel.mod ----- #####
  
  PInt <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable=="PInt"])
  TPlan <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable=="TPlan"])
  NDrSet <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable=="NDrSet"])
  NRipSet <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable=="NRipSet"])
  NRipConv <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable=="NRipConv"])
  NRipLoads <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable=="NRipConv"])
  CSOMax <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable=="CSOMax"])
  
  FTPlan <- (PInt/100*(1+PInt/100)^TPlan) / ((1+TPlan/100)^TPlan-1)
  
  camwrap_wmodel[[i]] <- as.data.frame(hcamr_wmodel[[i]])
  names(camwrap_wmodel[[i]]) <- c("Wmodel")
  
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row <- nrow(camwrap_wmodel[[i]])
  
  # minimize cost -------------------------------------------------------------
  text_find_mincost <- "minimize cost:"
  
  cost_eqn <- camwrap_wmodel[[i]]$Wmodel[grep(text_find_mincost, camwrap_wmodel[[i]]$Wmodel)]
  
  min_pre <- sub("CLuSet.*", "", cost_eqn)
  min_post <- substr(sub(".*CLuSet", "", cost_eqn),2,nchar(sub(".*CLuSet", "", cost_eqn)))
  
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    min_upd <- paste0("CLuSet",Mgmt)
    
    if(exists("min_eqn")) {
      min_eqn <- paste0(min_eqn," + ",min_upd)
    } else {
      min_eqn <- min_upd
    }
    
  }
  
  cost_eqn_upd <- paste0(min_pre, min_eqn, min_post)
  
  rm(min_eqn)
  
  camwrap_wmodel[[i]]$Wmodel[grep(text_find_mincost, camwrap_wmodel[[i]]$Wmodel)] <- cost_eqn_upd
  
  # param QRuU ----------------------------------------------------------------
  paramQRu_start <- head(camwrap_wmodel[[i]]$rownum[grep("param QRuU",camwrap_wmodel[[i]]$Wmodel)],n=1)
  paramQRu_end <- tail(camwrap_wmodel[[i]]$rownum[grep("param QRuU",camwrap_wmodel[[i]]$Wmodel)],n=1)
  paramQRu_pre <- camwrap_wmodel[[i]][1:(paramQRu_start-1), ]
  paramQRu_post <- camwrap_wmodel[[i]][paramQRu_end+1:final_row, ]
  
  counter = 0
  paramQRu <- data.frame()
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    for (HRU in 1:length(hcam_HRUs)) {
      
      paramQRu[counter+1,"Wmodel"] <- paste0("param QRuU",HRU,Mgmt,"{t in time} >=0;")
      paramQRu[counter+1,"rownum"] <- paramQRu_start + counter
      counter <- counter + 1
      
    }
  }
  
  camwrap_wmodel[[i]] <- rbind(paramQRu_pre, paramQRu, paramQRu_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # param QReU ----------------------------------------------------------------
  paramQRe_start <- head(camwrap_wmodel[[i]]$rownum[grep("param QReU",camwrap_wmodel[[i]]$Wmodel)],n=1)
  paramQRe_end <- tail(camwrap_wmodel[[i]]$rownum[grep("param QReU",camwrap_wmodel[[i]]$Wmodel)],n=1)
  paramQRe_pre <- camwrap_wmodel[[i]][1:(paramQRe_start-1), ]
  paramQRe_post <- camwrap_wmodel[[i]][paramQRe_end+1:final_row_upd, ]
  
  counter = 0
  paramQRe <- data.frame()
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    for (HRU in 1:length(hcam_HRUs)) {
      
      paramQRe[counter+1,"Wmodel"] <- paste0("param QReU",HRU,Mgmt,"{t in time} >=0;")
      paramQRe[counter+1,"rownum"] <- paramQRe_start + counter
      counter <- counter + 1
      
    }
  }
  
  camwrap_wmodel[[i]] <- rbind(paramQRe_pre, paramQRe, paramQRe_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # param LRuU ----------------------------------------------------------------
  paramLRu_start <- head(camwrap_wmodel[[i]]$rownum[grep("param LRuU",camwrap_wmodel[[i]]$Wmodel)],n=1)
  paramLRu_end <- tail(camwrap_wmodel[[i]]$rownum[grep("param LRuU",camwrap_wmodel[[i]]$Wmodel)],n=1)
  paramLRu_pre <- camwrap_wmodel[[i]][1:(paramLRu_start-1), ]
  paramLRu_post <- camwrap_wmodel[[i]][paramLRu_end+1:final_row_upd, ]
  
  counter = 0
  paramLRu <- data.frame()
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    for (HRU in 1:length(hcam_HRUs)) {
      
      paramLRu[counter+1,"Wmodel"] <- paste0("param LRuU",HRU,Mgmt,"1{t in time} >=0;")
      paramLRu[counter+1,"rownum"] <- paramLRu_start + counter
      counter <- counter + 1
      
    }
  }
  
  camwrap_wmodel[[i]] <- rbind(paramLRu_pre, paramLRu, paramLRu_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # param LReU ----------------------------------------------------------------
  paramLRe_start <- head(camwrap_wmodel[[i]]$rownum[grep("param LReU",camwrap_wmodel[[i]]$Wmodel)],n=1)
  paramLRe_end <- tail(camwrap_wmodel[[i]]$rownum[grep("param LReU",camwrap_wmodel[[i]]$Wmodel)],n=1)
  paramLRe_pre <- camwrap_wmodel[[i]][1:(paramLRe_start-1), ]
  paramLRe_post <- camwrap_wmodel[[i]][paramLRe_end+1:final_row_upd, ]
  
  counter = 0
  paramLRe <- data.frame()
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    for (HRU in 1:length(hcam_HRUs)) {
      
      paramLRe[counter+1,"Wmodel"] <- paste0("param LReU",HRU,Mgmt,"1{t in time} >=0;")
      paramLRe[counter+1,"rownum"] <- paramLRe_start + counter
      counter <- counter + 1
      
    }
  }
  
  camwrap_wmodel[[i]] <- rbind(paramLRe_pre, paramLRe, paramLRe_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # var DALu ------------------------------------------------------------------
  varDALu_start <- head(camwrap_wmodel[[i]]$rownum[grep("var DALu",camwrap_wmodel[[i]]$Wmodel)],n=1)
  varDALu_end <- tail(camwrap_wmodel[[i]]$rownum[grep("var DALu",camwrap_wmodel[[i]]$Wmodel)],n=1)
  varDALu_pre <- camwrap_wmodel[[i]][1:(varDALu_start-1), ]
  varDALu_post <- camwrap_wmodel[[i]][varDALu_end+1:final_row_upd, ]
  
  counter = 0
  varDALu <- data.frame()
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    for (HRU in 1:length(hcam_HRUs)) {
      
      ident <- paste0(HRU,";",Mgmt)
      
      ALuMin <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuMin"&
                                                              hcamr_specsresults_append[[i]]$Identifier==ident),3]) / HaToAcre
      ALuMax <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuMax"&
                                                              hcamr_specsresults_append[[i]]$Identifier==ident),3]) / HaToAcre
      
      if (Mgmt==1) {
        
        ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                 hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),3]) / HaToAcre
        
        
        varDALu[counter+1,"Wmodel"] <- paste0("var DALu",HRU,Mgmt," >=",ALuMin," <=",ALuMax," :=",ALuBase,";")
        varDALu[counter+1,"rownum"] <- varDALu_start + counter
        counter <- counter + 1
        
      } else {
        
        varDALu[counter+1,"Wmodel"] <- paste0("var DALu",HRU,Mgmt," >=",ALuMin," <=",ALuMax,";")
        varDALu[counter+1,"rownum"] <- varDALu_start + counter
        counter <- counter + 1
        
      }
    }
  }
  
  camwrap_wmodel[[i]] <- rbind(varDALu_pre, varDALu, varDALu_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])  
  
  # var CLuSet ----------------------------------------------------------------
  varCLu_start <- head(camwrap_wmodel[[i]]$rownum[grep("var CLuSet",camwrap_wmodel[[i]]$Wmodel)],n=1)
  varCLu_end <- tail(camwrap_wmodel[[i]]$rownum[grep("var CLuSet",camwrap_wmodel[[i]]$Wmodel)],n=1)
  varCLu_pre <- camwrap_wmodel[[i]][1:(varCLu_start-1), ]
  varCLu_post <- camwrap_wmodel[[i]][varCLu_end+1:final_row_upd, ]
  
  counter = 0
  varCLu <- data.frame()
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    varCLu[counter+1,"Wmodel"] <- paste0("var CLuSet",Mgmt," >=0 :=0;")
    varCLu[counter+1,"rownum"] <- varCLu_start + counter
    counter <- counter + 1
    
  }
  
  camwrap_wmodel[[i]] <- rbind(varCLu_pre, varCLu, varCLu_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])  
  
  # subject to dalu -----------------------------------------------------------
  STDALu_start <- head(camwrap_wmodel[[i]]$rownum[grep("subject to dalu",camwrap_wmodel[[i]]$Wmodel)],n=1)
  STDALu_end <- tail(camwrap_wmodel[[i]]$rownum[grep("subject to dalu",camwrap_wmodel[[i]]$Wmodel)],n=1)
  STDALu_pre <- camwrap_wmodel[[i]][1:(STDALu_start-1), ]
  STDALu_post <- camwrap_wmodel[[i]][STDALu_end+2:final_row_upd, ]
  
  Mgmt <- 1
  counter = 0
  STDALu <- data.frame()
  for (HRU in 1:length(hcam_HRUs)) {
    
    ident <- paste0(HRU,";",Mgmt)
    
    STDALu[counter+1,"Wmodel"] <- paste0("subject to dalu",HRU,Mgmt,":")
    STDALu[counter+1,"rownum"] <- STDALu_start + counter
    counter <- counter + 1
    
    ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                             hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),3]) / HaToAcre
    
    if (as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="CCLu"&
                                                    hcamr_specsresults_append[[i]]$Identifier==ident),3])>=0) {
      
      STDALu[counter+1,"Wmodel"] <- paste0("DALu",HRU,Mgmt," - ",ALuBase," >= 0;")
      STDALu[counter+1,"rownum"] <- STDALu_start + counter
      counter <- counter + 1
      
    } else {
      
      STDALu[counter+1,"Wmodel"] <- paste0("DALu",HRU,Mgmt," - ",ALuBase," <= 0;")
      STDALu[counter+1,"rownum"] <- STDALu_start + counter
      counter <- counter + 1
      
    }
    
  }
  
  camwrap_wmodel[[i]] <- rbind(STDALu_pre, STDALu, STDALu_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # subject to alu_subtotal ---------------------------------------------------
  STALusub_start <- head(camwrap_wmodel[[i]]$rownum[grep("subject to alu_subtotal",camwrap_wmodel[[i]]$Wmodel)],n=1)
  STALusub_end <- tail(camwrap_wmodel[[i]]$rownum[grep("subject to alu_subtotal",camwrap_wmodel[[i]]$Wmodel)],n=1)
  STALusub_pre <- camwrap_wmodel[[i]][1:(STALusub_start-1), ]
  STALusub_post <- camwrap_wmodel[[i]][STALusub_end+2:final_row_upd, ]
  
  counter = 0
  STALusub <- data.frame()
  for (HRU in 1:length(hcam_HRUs)) {
    
    STALusub[counter+1,"Wmodel"] <- paste0("subject to alu_subtotal",HRU,":")
    STALusub[counter+1,"rownum"] <- STALusub_start + counter
    counter <- counter + 1
    
    for (Mgmt in 2:length(camwrap_mgmt)) {
      
      alu_start <- paste0("  DALu",HRU,"1 >= ")
      alu_eqn <- paste0("DALu",HRU,Mgmt)
      
      # Bind together
      if (exists("alu_eqn_rev")) {
        alu_eqn_rev <- paste0(alu_eqn_rev," + ",alu_eqn)
      } else {
        alu_eqn_rev <- alu_eqn
      }
      
    } 
    
    STALusub[counter+1,"Wmodel"] <- paste0(alu_start,alu_eqn_rev,";")
    STALusub[counter+1,"rownum"] <- STALusub_start + counter
    counter <- counter + 1
    
    rm(alu_eqn_rev)
  }
  
  camwrap_wmodel[[i]] <- rbind(STALusub_pre, STALusub, STALusub_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # subject to a_lu_total -----------------------------------------------------
  STALutot <- camwrap_wmodel[[i]]$rownum[grep("subject to a_lu_total",camwrap_wmodel[[i]]$Wmodel)]
  STALutot_pre <- camwrap_wmodel[[i]][1:(STALutot-1), ]
  STALutot_post <- camwrap_wmodel[[i]][STALutot+2:final_row_upd, ]
  
  Mgmt <- 1  
  counter = 0
  STALutot_df <- data.frame()
  
  STALutot_df[counter+1,"Wmodel"] <- "subject to a_lu_total:"
  STALutot_df[counter+1,"rownum"] <- STALutot + counter
  counter <- counter + 1
  
  ALuT <- sum(as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="ALuBase","Value"])) / HaToAcre
  
  a_lu_total_start <- paste0("  ",ALuT," - 0.01 <= ")
  a_lu_total_end <- paste0(" <= ",ALuT," + 0.01;")
  for (HRU in 1:length(hcam_HRUs)) {
    
    a_lu_total_eqn <- paste0("DALu",HRU,Mgmt)
    
    # Bind together
    if (exists("a_lu_total_eqn_rev")) {
      a_lu_total_eqn_rev <- paste0(a_lu_total_eqn_rev," + ",a_lu_total_eqn)
    } else {
      a_lu_total_eqn_rev <- a_lu_total_eqn
    }
    
  }
  
  STALutot_df[counter+1,"Wmodel"] <- paste0(a_lu_total_start, a_lu_total_eqn_rev, a_lu_total_end)
  STALutot_df[counter+1,"rownum"] <- STALutot + counter
  
  rm(a_lu_total_eqn_rev)
  
  camwrap_wmodel[[i]] <- rbind(STALutot_pre, STALutot_df, STALutot_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # subject to a_lu_excl- -----------------------------------------------------
  STALu_excl_start <- head(camwrap_wmodel[[i]]$rownum[grep("subject to alu_excl",camwrap_wmodel[[i]]$Wmodel)],n=1)
  STALu_excl_end <- tail(camwrap_wmodel[[i]]$rownum[grep("subject to alu_excl",camwrap_wmodel[[i]]$Wmodel)],n=1)
  STALu_excl_pre <- camwrap_wmodel[[i]][1:(STALu_excl_start-1), ]
  STALu_excl_post <- camwrap_wmodel[[i]][STALu_excl_end+2:final_row_upd, ]
  
  counter = 0
  STALu_excl <- data.frame()
  for (HRU in 1:length(hcam_HRUs)) {
    
    for (Mgmt in 2:length(camwrap_mgmt)) {
      ident <- paste0(HRU,";",Mgmt)
      
      if (as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="CCLu"&
                                                      hcamr_specsresults_append[[i]]$Identifier==ident),3])<0) {
        
        STALu_excl[counter+1,"Wmodel"] <- paste0("subject to alu_excl",HRU,Mgmt,":")
        STALu_excl[counter+1,"rownum"] <- STALu_excl_start + counter
        counter <- counter + 1
        
        STALu_excl[counter+1,"Wmodel"] <- paste0("  DALu",HRU,Mgmt," = 0;")
        STALu_excl[counter+1,"rownum"] <- STALu_excl_start + counter
        counter <- counter + 1
        
      }
      
    }
    
  }
  
  camwrap_wmodel[[i]] <- rbind(STALu_excl_pre, STALu_excl, STALu_excl_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # CLuSet equation -----------------------------------------------------------
  CLu_start <- head(camwrap_wmodel[[i]]$rownum[grep("subject to c_luset",camwrap_wmodel[[i]]$Wmodel)],n=1)
  CLu_end <- tail(camwrap_wmodel[[i]]$rownum[grep("subject to c_luset",camwrap_wmodel[[i]]$Wmodel)],n=1)
  CLu_pre <- camwrap_wmodel[[i]][1:(CLu_start-1), ]
  CLu_post <- camwrap_wmodel[[i]][CLu_end+2:final_row_upd, ]
  
  counter = 0
  CLu <- data.frame()
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    CLu[counter+1,"Wmodel"] <- paste0("subject to c_luset",Mgmt,":")
    CLu[counter+1,"rownum"] <- CLu_start + counter
    counter <- counter + 1
    
    
    Cluset_start <- paste0("  CLuSet",Mgmt," = ")
    if(Mgmt==1) {
      
      for (HRU in 1:length(hcam_HRUs)) {
        ident <- paste0(HRU,";",Mgmt)
        
        CCLu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="CCLu"&
                                                              hcamr_specsresults_append[[i]]$Identifier==ident),3]) / DpHaToDpAcre
        COmLu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="COmLu"&
                                                               hcamr_specsresults_append[[i]]$Identifier==ident),3]) / DpHaToDpAcre
        ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                 hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),3]) / HaToAcre
        
        if (CCLu>=0) {
          
          Cluset <- paste0(FTPlan*CCLu," * DALu",HRU,Mgmt, " - ",FTPlan*CCLu*ALuBase," + ",COmLu," * DALu",HRU,Mgmt," - ",COmLu*ALuBase)
          
          # Bind together
          if (exists("Cluset_rev")) {
            Cluset_rev <- paste0(Cluset_rev," + ",Cluset)
          } else {
            Cluset_rev <- Cluset
          }
          
        }
        
      }
      
      if(exists("Cluset_rev")) {
        Cluset_rev <- Cluset_rev
      } else {
        Cluset_rev <- "0"
      }
      
      CLu[counter+1,"Wmodel"] <- paste0(Cluset_start, Cluset_rev,";")
      CLu[counter+1,"rownum"] <- CLu_start + counter
      counter <- counter + 1
      
    } else {
      
      for (HRU in 1:length(hcam_HRUs)) {
        ident <- paste0(HRU,";",Mgmt)
        
        CCLu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="CCLu"&
                                                              hcamr_specsresults_append[[i]]$Identifier==ident),3]) / DpHaToDpAcre
        COmLu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="COmLu"&
                                                               hcamr_specsresults_append[[i]]$Identifier==ident),3]) / DpHaToDpAcre
        
        if (CCLu>=0) {
          
          Cluset_else <- paste0(FTPlan*CCLu," * DALu",HRU,Mgmt, " + ",COmLu," * DALu",HRU,Mgmt)
          
          # Bind together
          if (exists("Cluset_rev_else")) {
            Cluset_rev_else <- paste0(Cluset_rev_else," + ",Cluset_else)
          } else {
            Cluset_rev_else <- Cluset_else
          }
          
        }
        
      }
      
      CLu[counter+1,"Wmodel"] <- paste0(Cluset_start, Cluset_rev_else,";")
      CLu[counter+1,"rownum"] <- CLu_start + counter
      counter <- counter + 1
      
    }
    
    if(exists("Cluset_rev")) {
      rm(Cluset_rev)
    }
    if(exists("Cluset_rev_else")) {
      rm(Cluset_rev_else)
    }
    
  }
  
  camwrap_wmodel[[i]] <- rbind(CLu_pre, CLu, CLu_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]]) 
  
  # QRu equation --------------------------------------------------------------
  STQRu <- camwrap_wmodel[[i]]$rownum[grep("subject to runoff",camwrap_wmodel[[i]]$Wmodel)]
  STQRu_pre <- camwrap_wmodel[[i]][1:(STQRu-1), ]
  STQRu_post <- camwrap_wmodel[[i]][STQRu+2:final_row_upd, ]
  
  counter = 0
  STQRu_df <- data.frame()
  
  STQRu_df[counter+1,"Wmodel"] <- "subject to runoff{t in time}:"
  STQRu_df[counter+1,"rownum"] <- STQRu + counter
  counter <- counter + 1
  
  qru_start <- "  QRu[t] = "
  
  for (Mgmt in 1:length(camwrap_mgmt)) {  
    
    if (Mgmt==1) {
    
      for (HRU in 1:length(hcam_HRUs)) { 
        
        ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                 hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
        PCSS <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="PCSS"&
                                                              hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"]) / ALuBase
        PnCSS <- 1 - PCSS
        
        W_Qru <- paste0("QRuU",HRU,Mgmt,"[t]"," * DALu",HRU,Mgmt)
        
        if (PCSS > 0) {
            
            W_Qru <- paste0(W_Qru," * ",PnCSS)
            
        }
          
        # Bind together
        if (exists("W_Qru_rev")) {
          W_Qru_rev <- paste0(W_Qru_rev," + ",W_Qru)
        } else {
          W_Qru_rev <- W_Qru
        }
      
      }
      
      if (NRipConv > 0) {
        
        for (b in 1:length(NRipSet)) {
          
          for (f in 1:length(NRipConv)) {
            
            for (g in 1:length(NRipLoads)) {
              
              step = 1
              
              for (HRU in 1:length(hcam_HRUs)) { 
              
                ARip <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="ARip"&
                                                                  hcamr_specsresults_append[[i]]$Identifier==paste(b,f,g,sep=";"),
                                                                "Value"])
                ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                         hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
                PCSS <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="PCSS"&
                                                                      hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"]) / ALuBase
                PnCSS <- 1 - PCSS
                
                if (ARip > 0) {
                
                  FromHRU <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="FromHRU"&
                                                                       hcamr_specsresults_append[[i]]$Identifier==paste(b,f,sep=";"),
                                                                     "Value"])
                  ToHRU <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="ToHRU"&
                                                                       hcamr_specsresults_append[[i]]$Identifier==paste(b,f,sep=";"),
                                                                     "Value"])
                
                  if (FromHRU = HRU) {
                    
                    if (step = 1) {
                    
                      W_Qru <- paste0(W_Qru_rev," + DRipSet",b,f,g,"1 * (QRuU",HRU,Mgmt,"[t]"," * (-ARip",b,f,g,")")
                  
                      if (PCSS > 0) {
                      
                        W_Qru <- paste0(W_Qru," * ",PnCSS)
                      
                      }
                      
                      step = step + 1
                    
                    } else if (step > 1) {
                      
                      W_Qru <- paste0(W_Qru_rev," + QRuU",HRU,Mgmt,"[t]"," * (-ARip",b,f,g,"))")
                      
                      if (PCSS > 0) {
                        
                        W_Qru <- paste0(W_Qru," * ",PnCSS)
                        
                      }
                      
                    }
                    
                  } else if (ToHRU = HRU) {
                    
                    if (step = 1) {
                      
                      W_Qru <- paste0(W_Qru_rev," + DRipSet",b,f,g,"1 * (QRuU",HRU,Mgmt,"[t] * ARip",b,f,g)
                      
                      if (PCSS > 0) {
                        
                        W_Qru <- paste0(W_Qru," * ",PnCSS)
                        
                      }
                      
                      step <- step + 1
                      
                    } else if (step > 1) {
                      
                      W_Qru <- paste0(W_Qru_rev," + QRuU",HRU,Mgmt,"[t] * ARip",b,f,g,")")
                      
                      if (PCSS > 0) {
                        
                        W_Qru <- paste0(W_Qru," * ",PnCSS)
                        
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    } else if (Mgmt > 1) {
      
      NLuSetName <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="NLuSetName"&
                                                             hcamr_specsresults_append[[i]]$Identifier==Mgmt,
                                                           "Value"])      
      if (NLuSetName = "Green Roof") {
        
        for (HRU in 1:length(hcam_HRUs)) {
          
          ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                   hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
          EIALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIALu"&
                                                                   hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])          
          EIARoof <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIARoof"&
                                                                   hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
          RoofALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="RoofALu"&
                                                                   hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
          
          EIALuNoRoof <- ((EIALu * ALuBase) - (EIARoof * RoofALu)) / ALuBase
          
          PCSS <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="PCSS"&
                                                                hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"]) / ALuBase
          PnCSS <- 1 - PCSS          
          
          W_Qru <- paste0(W_Qru," + ((QRuU",HRU,Mgmt,"[t] * ",EIARoof * EIALuNoRoof,") - QRuU",HRU,"1[t]) * DALu",HRU,Mgmt)
          
          if (PCSS > 0) {
            
            W_Qru <- paste0(WQ_ru," * ",PnCSS)
            
          }
    
        }
      
      } else {
      
        for (HRU in 1:length(hcam_HRUs)) {
          
          ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                   hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
          
          PCSS <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="PCSS"&
                                                                hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"]) / ALuBase
          PnCSS <- 1 - PCSS          
          
          W_Qru <- paste0(W_Qru," + (QRuU",HRU,Mgmt,"[t] - QRuU",HRU,"1[t]) * DALu",HRU,Mgmt)
          
          if (PCSS > 0) {
            
            W_Qru <- paste0(WQ_ru," * ",PnCSS)
            
          }
        }
      }
    }
  }
  
  STQRu_df[counter+1,"Wmodel"] <- paste0(qru_start, W_Qru, ";")
  STQRu_df[counter+1,"rownum"] <- STQRu + counter
  
  rm(W_Qru)
  
  camwrap_wmodel[[i]] <- rbind(STQRu_pre, STQRu_df, STQRu_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  If (CSOMax > 0) {
    # QRuCS equation --------------------------------------------------------------
    STQRuCS <- camwrap_wmodel[[i]]$rownum[grep("subject to runoff_cs",camwrap_wmodel[[i]]$Wmodel)]
    STQRuCS_pre <- camwrap_wmodel[[i]][1:(STQRuCS-1), ]
    STQRuCS_post <- camwrap_wmodel[[i]][STQRuCS+2:final_row_upd, ]
    
    counter = 0
    STQRuCS_df <- data.frame()
    
    STQRuCS_df[counter+1,"Wmodel"] <- "subject to runoff_cs{t in time}:"
    STQRuCS_df[counter+1,"rownum"] <- STQRuCS + counter
    counter <- counter + 1
    
    qrucs_start <- "  QRuCS[t] = (DPCS1 / 100) * ("
    
    for (Mgmt in 1:length(camwrap_mgmt)) {  
      
      for (HRU in 1:length(hcam_HRUs)) { 
        
        ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                 hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
        PCSS <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="PCSS"&
                                                              hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"]) / ALuBase
        
        if (Mgmt==1) {
          
          if (PCSS > 0) {
            
            W_QruCS <- paste0("QRuU",HRU,Mgmt,"[t]"," * DALu",HRU,Mgmt," * ",PCSS)
            
          }
          
          # Bind together
          if (exists("W_QruCS_rev")) {
            W_QruCS_rev <- paste0(W_QruCS_rev," + ",W_QruCS)
          } else {
            W_QruCS_rev <- W_QruCS
          }
          
        } else {
          
          if (PCSS > 0) {
            
            qrucs_else <- paste0(" + (QRuU",HRU,Mgmt,"[t] - QRuU",HRU,"1[t]) * DALu",HRU,Mgmt," * ",PCSS)
          
          }
          
          # Bind together
          if (exists("qrucs_rev_else")) {
            qrucs_rev_else <- paste0(qrucs_rev_else,qrucs_else)
          } else {
            qrucs_rev_else <- qrucs_else
          }
        }
      }
    }
    
    STQRuCS_df[counter+1,"Wmodel"] <- paste0(qrucs_start, W_QruCS_rev, qrucs_rev_else, ");")
    STQRuCS_df[counter+1,"rownum"] <- STQRuCS + counter
    
    rm(W_QruCS_rev, qrucs_rev_else)
    
    camwrap_wmodel[[i]] <- rbind(STQRuCS_pre, STQRuCS_df, STQRuCS_post)
    camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
    final_row_upd <- nrow(camwrap_wmodel[[i]])
    
    # QRuStS equation --------------------------------------------------------------
    STQRuStS <- camwrap_wmodel[[i]]$rownum[grep("subject to runoff_sts",camwrap_wmodel[[i]]$Wmodel)]
    STQRuStS_pre <- camwrap_wmodel[[i]][1:(STQRuStS-1), ]
    STQRuStS_post <- camwrap_wmodel[[i]][STQRuStS+2:final_row_upd, ]
    
    counter = 0
    STQRuStS_df <- data.frame()
    
    STQRuStS_df[counter+1,"Wmodel"] <- "subject to runoff_sts{t in time}:"
    STQRuStS_df[counter+1,"rownum"] <- STQRuStS + counter
    counter <- counter + 1
    
    qrusts_start <- "  QRuStS[t] = (1 - (DPCS / 100)) * ("
    
    for (Mgmt in 1:length(camwrap_mgmt)) {  
      
      for (HRU in 1:length(hcam_HRUs)) { 
        
        ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                 hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
        PCSS <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="PCSS"&
                                                              hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"]) / ALuBase
        
        if (Mgmt==1) {
          
          if (PCSS > 0) {
            
            W_QruStS <- paste0("QRuU",HRU,Mgmt,"[t]"," * DALu",HRU,Mgmt," * ",PCSS)
            
          }
          
          # Bind together
          if (exists("W_QruStS_rev")) {
            W_QruStS_rev <- paste0(W_QruStS_rev," + ",W_QruStS)
          } else {
            W_QruStS_rev <- W_QruStS
          }
          
        } else {
          
          if (PCSS > 0) {
            
            qrusts_else <- paste0(" + (QRuU",HRU,Mgmt,"[t] - QRuU",HRU,"1[t]) * DALu",HRU,Mgmt," * ",PCSS)
            
          }
          
          # Bind together
          if (exists("qrusts_rev_else")) {
            qrusts_rev_else <- paste0(qrusts_rev_else,qrusts_else)
          } else {
            qrusts_rev_else <- qrusts_else
          }
        }
      }
    }
    
    STQRuStS_df[counter+1,"Wmodel"] <- paste0(qrusts_start, W_QruStS_rev, qrusts_rev_else, ");")
    STQRuStS_df[counter+1,"rownum"] <- STQRuStS + counter
    
    rm(W_QruStS)
    
    camwrap_wmodel[[i]] <- rbind(STQRuStS_pre, STQRuStS_df, STQRuStS_post)
    camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
    final_row_upd <- nrow(camwrap_wmodel[[i]])
    
  }
    
  # QRe equation --------------------------------------------------------------
  STQRe <- camwrap_wmodel[[i]]$rownum[grep("subject to recharge",camwrap_wmodel[[i]]$Wmodel)]
  STQRe_pre <- camwrap_wmodel[[i]][1:(STQRe-1), ]
  STQRe_post <- camwrap_wmodel[[i]][STQRe+2:final_row_upd, ]
  
  counter = 0
  STQRe_df <- data.frame()
  
  STQRe_df[counter+1,"Wmodel"] <- "subject to recharge{t in time}:"
  STQRe_df[counter+1,"rownum"] <- STQRe + counter
  counter <- counter + 1
  
  qre_start <- "  QRe[t] = "
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    for (HRU in 1:length(hcam_HRUs)) {
      
      if (Mgmt==1) {
        qre <- paste0("QReU",HRU,Mgmt,"[t]"," * DALu",HRU,Mgmt)
        
        # Bind together
        if (exists("qre_rev")) {
          qre_rev <- paste0(qre_rev," + ",qre)
        } else {
          qre_rev <- qre
        }
        
      } else { 
        
        qre_else <- paste0(" + (QReU",HRU,Mgmt,"[t] - QReU",HRU,"1[t]) * DALu",HRU,Mgmt)
        
        # Bind together
        if (exists("qre_rev_else")) {
          qre_rev_else <- paste0(qre_rev_else,qre_else)
        } else {
          qre_rev_else <- qre_else
        }
      }
    }
  }
  
  STQRe_df[counter+1,"Wmodel"] <- paste0(qre_start, qre_rev, qre_rev_else, ";")
  STQRe_df[counter+1,"rownum"] <- STQRe + counter
  
  rm(qre_rev, qre_rev_else)
  
  camwrap_wmodel[[i]] <- rbind(STQRe_pre, STQRe_df, STQRe_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
   # LRu equation --------------------------------------------------------------
  STLRu <- camwrap_wmodel[[i]]$rownum[grep("subject to lru",camwrap_wmodel[[i]]$Wmodel)]
  STLRu_pre <- camwrap_wmodel[[i]][1:(STLRu-1), ]
  STLRu_post <- camwrap_wmodel[[i]][STLRu+2:final_row_upd, ]
  
  counter = 0
  STLRu_df <- data.frame()
  
  STLRu_df[counter+1,"Wmodel"] <- "subject to lru1{t in time}:"
  STLRu_df[counter+1,"rownum"] <- STLRu + counter
  counter <- counter + 1
  
  lru_start <- "  LRu1[t] = "

  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    for (HRU in 1:length(hcam_HRUs)) {
      
      if (Mgmt==1) {
        
        lru <- paste0("LRuU",HRU,Mgmt,"1[t]"," * DALu",HRU,Mgmt)
        
        if (NDrSet>0) {
          
          for (DrSet in 1:NDrSet) {
            
            DrRemovalRate <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="DrRemovalRate"&
                                                                         hcamr_specsresults_append[[i]]$Identifier==paste(HRU,DrSet,"1",sep=";"),
                                                                       "Value"]) / 100
            
            dr_set1 <- paste0("(1 - (",DrRemovalRate," * DDrSet",DrSet,"))")
            
            lru <- paste0(lru," * ",dr_set1)
            
          }
          
        }
        
        if (exists("lru_set1")) {
          lru_set1 <- paste0(lru_set1, " + ", lru)
        } else {
          lru_set1 <- lru
        }
        
      } else {
        
        NLuSetName <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="NLuSetName"&
                                                                  hcamr_specsresults_append[[i]]$Identifier==Mgmt,
                                                                "Value"])   
        if(NDrSet>0) {
   
          if (NLuSetName = "Green Roof") {
            
            ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                     hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
            EIALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIALu"&
                                                                   hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])          
            EIARoof <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIARoof"&
                                                                     hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
            RoofALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="RoofALu"&
                                                                     hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
      
            EIALuNoRoof <- ((EIALu * ALuBase) - (EIARoof * RoofALu)) / ALuBase
            
            lru_set <- paste0("DALu",HRU,Mgmt," * ((LRuU",HRU,Mgmt,"1[t] * ",EIARoof * EIALuNoRoof,
                              ") - LRuU",HRU,"11[t] - (LRuU",HRU,Mgmt,"1[t] * ",EIARoof * EIALuNoRoof,")")
            
          } else {
            
            lru_set <- paste0("DALu",HRU,Mgmt," * (LRuU",HRU,Mgmt,"1[t] - LRuU",HRU,"11[t] - LRuU",HRU,Mgmt,"1[t]")
            
          }
          
          for(DrSet in 1:NDrSet) {
            
            DrRemovalRate <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="DrRemovalRate"&
                                                                         hcamr_specsresults_append[[i]]$Identifier==paste(HRU,DrSet,"1",sep=";"),
                                                                       "Value"]) / 100
            
            dr_set_else <- paste0("(",DrRemovalRate," * DDrSet",DrSet,")")
            
            lru_set <- paste0(lru_set," * ",dr_set_else)
            
          }
          
          lru_set <- paste0(lru_set,")")
          
        } else {
          
          if (NLuSetName = "Green Roof") {
            
            ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                     hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
            EIALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIALu"&
                                                                   hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])          
            EIARoof <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIARoof"&
                                                                     hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
            RoofALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="RoofALu"&
                                                                     hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
            
            EIALuNoRoof <- ((EIALu * ALuBase) - (EIARoof * RoofALu)) / ALuBase
            
            lru_set <- paste0("((LRuU",HRU,Mgmt,"1[t] * ",EIARoof * EIALuNoRoof,
                              ") - LRuU",HRU,"11[t]) * DALu",HRU,Mgmt)
            
          } else {
            
            lru_set <- paste0("(LRuU",HRU,Mgmt,"1[t] - LRuU",HRU,"11[t]) * DALu",HRU,Mgmt)
            
          }
        }
        
        if (exists("lru_else")) {
          lru_else <- paste0(lru_else, " + ", lru_set)
        } else {
          lru_else <- lru_set
        }
      }
    }
  }
  
  STLRu_df[counter+1,"Wmodel"] <- paste0(lru_start, lru_set1, " + ",lru_else, ";")
  STLRu_df[counter+1,"rownum"] <- STLRu + counter
  
  rm(lru_set1, lru_else, lru_set, lru)
  
  camwrap_wmodel[[i]] <- rbind(STLRu_pre, STLRu_df, STLRu_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  if (NRipConv > 0) {
    
    # Set variables used in the riparian loadings equations first
    ARipTotal <- data.frame()
    for (b in 1:NRipSet) {
      
      for (g in 1:NRipLoads) {
        
        ARipTotal_temp <- 0
        
        for (f in 1:NRipConv) {
          
          ARip <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARip"&
                                                                hcamr_specsresults_append[[i]]$Identifier==paste(b,f,g,sep=";")),"Value"])
          ARipTotal_temp <- ARipTotal_temp + ARip
          
        }
        
        ARipTotal[b, g] <- ARipTotal_temp
        
      }
    }
    
    Gcounter <- data.frame()
    for (b in 1:NRipSet) {
      
      for (HRU in 1:length(hcam_HRUs)) {
        
        Gcounter[HRU,1] <- 0
        
        for (g in 1:NRipLoads) {
          
          ARipUp <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARipUp"&
                                                                  hcamr_specsresults_append[[i]]$Identifier==paste(b,c,g,sep=";")),"Value"])
          if (ARipUp > 0) { Gcounter[HRU,1] <- Gcounter[HRU,1] + 1 }
          
        }
      }
    }
    
    # LRuAdjUp equation --------------------------------------------------------------
    STLRuAdjUp <- camwrap_wmodel[[i]]$rownum[grep("subject to lruadjup",camwrap_wmodel[[i]]$Wmodel)]
    STLRuAdjUp_pre <- camwrap_wmodel[[i]][1:(STLRuAdjUp-1), ]
    STLRuAdjUp_post <- camwrap_wmodel[[i]][STLRuAdjUp+2:final_row_upd, ]
    
    counter = 0
    STLRuAdjUp_df <- data.frame()
    
    STLRuAdjUp_df[counter+1,"Wmodel"] <- "subject to lruadjup1{t in time}:"
    STLRuAdjUp_df[counter+1,"rownum"] <- STLRuAdjUp + counter
    counter <- counter + 1
    
    lruadjup_start <- "  LRuAdjUp1[t] = "
    
    for (Mgmt in 1:length(camwrap_mgmt)) {
      
      if (Mgmt==1) {
        
        for (b in 1:NRipSet) {
          
          for (f in 1:NRipConv) {
            
            for (HRU in 1:length(hcam_HRUs)) {
              
              ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                       hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
              if (Gcounter[HRU,1] > 0) {
                
                lruadjup_set1 <- paste0("LRuU",HRU,Mgmt,"1[t]"," * DALu",HRU,Mgmt," * (")
                
                for (g in 1:NRipLoads) {
                  
                  ARipUp <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARipUp"&
                                                                          hcamr_specsresults_append[[i]]$Identifier==paste(b,c,g,sep=";")),"Value"])
                  if (ARipUp > 0) {
                    
                    AdjEff <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="AdjEff"&
                                                                            hcamr_specsresults_append[[i]]$Identifier==paste(b,f,"1",sep=";")),"Value"])
                    ARip <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARip"&
                                                                          hcamr_specsresults_append[[i]]$Identifier==paste(b,f,g,sep=";")),"Value"])
                    
                    FLAdj <- (ARipUp / ALuBase) * AdjEff * (ARip / ARipTotal[b, g])
                    
                    rip_set1 <- paste0(FLAdj," * DRipSet",b,f,g,"1")
                    
                    if (NDrSet>0) {
                      
                      for (DrSet in 1:NDrSet) {
                        
                        DrRemovalRate <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="DrRemovalRate"&
                                                                                     hcamr_specsresults_append[[i]]$Identifier==paste(HRU,DrSet,"1",sep=";"),
                                                                                   "Value"]) / 100
                        
                        dr_set1 <- paste0("(1 - (",DrRemovalRate," * DDrSet",DrSet,"))")
                        
                        rip_set1 <- paste0(rip_set1," * ",dr_set1)
                        
                      }
                    }
                  }
                  
                  if (exists("rip_set1_rev")) {
                    rip_set1_rev <- paste0(rip_set1_rev, " + ", rip_set1)
                  } else {
                    rip_set1_rev <- rip_set1
                  }
                  
                }
                
                rip_set1_rev <- paste0(rip_set1_rev,")")
                
              }
              
              if (exists("lruadjup_set1_rev")) {
                lruadjup_set1_rev <- paste0(lruadjup_set1_rev, " + ", lruadjup_set1)
              } else {
                lruadjup_set1_rev <- lruadjup_set1
              }
            }
          }
        }
      } else {
        
        for (b in 1:NRipSet) {
          
          for (f in 1:NRipConv) {
            
            for (HRU in 1:length(hcam_HRUs)) {
              
              ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                       hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
              if (Gcounter[HRU,1] > 0) {
                
                NLuSetName <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="NLuSetName"&
                                                                          hcamr_specsresults_append[[i]]$Identifier==Mgmt,
                                                                        "Value"])   
                if (NLuSetName = "Green Roof") {
                  
                  ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                           hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
                  EIALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIALu"&
                                                                         hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])          
                  EIARoof <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIARoof"&
                                                                           hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
                  RoofALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="RoofALu"&
                                                                           hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
                  
                  EIALuNoRoof <- ((EIALu * ALuBase) - (EIARoof * RoofALu)) / ALuBase
                  
                  lruadjup_set <- paste0(" + DALu",HRU,Mgmt," * ((LRuU",HRU,Mgmt,"1[t] * ",EIARoof * EIALuNoRoof,
                                         ") - LRuU",HRU,"11[t]) * (")
                  
                } else {
                  
                  lruadjup_set <- paste0(" + DALu",HRU,Mgmt," * (LRuU",HRU,Mgmt,"1[t] - LRuU",HRU,"11[t] * (")
                  
                }
                
                for (g in 1:NRipLoads) {
                  
                  ARipUp <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARipUp"&
                                                                          hcamr_specsresults_append[[i]]$Identifier==paste(b,c,g,sep=";")),"Value"])
                  if (ARipUp > 0) {
                    
                    AdjEff <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="AdjEff"&
                                                                            hcamr_specsresults_append[[i]]$Identifier==paste(b,f,"1",sep=";")),"Value"])
                    ARip <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARip"&
                                                                          hcamr_specsresults_append[[i]]$Identifier==paste(b,f,g,sep=";")),"Value"])
                    
                    FLAdj <- (ARipUp / ALuBase) * AdjEff * (ARip / ARipTotal[b, g])
                    
                    rip_set <- paste0(FLAdj," * DRipSet",b,f,g,"1")
                    
                  }
                  
                  if (exists("rip_set_rev")) {
                    rip_set_rev <- paste0(rip_set_rev, " + ", rip_set)
                  } else {
                    rip_set_rev <- rip_set
                  }
                }
                
                lruadjup_set <- paste0(lruadjup_set,rip_set_rev,")")
                
                if (NDrSet>0) {
                  
                  if (NLuSetName = "Green Roof") {
                    
                    ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                             hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
                    EIALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIALu"&
                                                                           hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])          
                    EIARoof <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="EIARoof"&
                                                                             hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
                    RoofALu <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="RoofALu"&
                                                                             hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
                    
                    EIALuNoRoof <- ((EIALu * ALuBase) - (EIARoof * RoofALu)) / ALuBase
                    
                    lruadjup_set <- paste0(lruadjup_set, " - (LRuU",HRU,Mgmt,"1[t]) * ",EIARoof * EIALuNoRoof)
                    
                  } else {
                    
                    lruadjup_set <- paste0(lruadjup_set, " - LRuU",HRU,Mgmt,"1[t]")
                    
                  }
                  
                  for (g in 1:NRipLoads) {
                    
                    ARipUp <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARipUp"&
                                                                            hcamr_specsresults_append[[i]]$Identifier==paste(b,c,g,sep=";")),"Value"])
                    if (ARipUp > 0) {
                      
                      AdjEff <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="AdjEff"&
                                                                              hcamr_specsresults_append[[i]]$Identifier==paste(b,f,"1",sep=";")),"Value"])
                      ARip <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARip"&
                                                                            hcamr_specsresults_append[[i]]$Identifier==paste(b,f,g,sep=";")),"Value"])
                      
                      FLAdj <- (ARipUp / ALuBase) * AdjEff * (ARip / ARipTotal[b, g])
                      
                      rip_dr_set <- paste0(" * (",FLAdj," * DRipSet",b,f,g,"1")
                      
                    }
                    
                    if (exists("rip_dr_set_rev")) {
                      rip_dr_set_rev <- paste0(rip_dr_set_rev, " + ", rip_dr_set)
                    } else {
                      rip_dr_set_rev <- rip_dr_set
                    }
                    
                    for (DrSet in 1:NDrSet) {
                      
                      DrRemovalRate <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="DrRemovalRate"&
                                                                                   hcamr_specsresults_append[[i]]$Identifier==paste(HRU,DrSet,"1",sep=";"),
                                                                                 "Value"]) / 100
                      
                      rip_dr_set_rev <- paste0(rip_dr_set_rev," * (",DrRemovalRate," * DDrSet",DrSet,")")
                      
                    }
                  }
                  
                  lruadjup_set <- paste0(lruadjup_set,rip_dr_set_rev,")")
                  
                }
              }
            }
          }
        }
      }
    }
    
    STLRuAdjUp_df[counter+1,"Wmodel"] <- paste0(lruadjup_start, lruadjup_set1_rev, lruadjup_set, ";")
    STLRuAdjUp_df[counter+1,"rownum"] <- STLRuAdjUp + counter
    
    rm(rip_set1_rev, lruadjup_set1_rev, rip_set_rev, rip_dr_set_rev, lruadjup_set)
    
    camwrap_wmodel[[i]] <- rbind(STLRuAdjUp_pre, STLRuAdjUp_df, STLRuAdjUp_post)
    camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
    final_row_upd <- nrow(camwrap_wmodel[[i]])
    
    # LRuAdd equation --------------------------------------------------------------
    STLRuAdd <- camwrap_wmodel[[i]]$rownum[grep("subject to lruaddup",camwrap_wmodel[[i]]$Wmodel)]
    STLRuAdd_pre <- camwrap_wmodel[[i]][1:(STLRuAdd-1), ]
    STLRuAdd_post <- camwrap_wmodel[[i]][STLRuAdd+2:final_row_upd, ]
    
    counter = 0
    STLRuAdd_df <- data.frame()
    
    STLRuAdd_df[counter+1,"Wmodel"] <- "subject to lruadd1{t in time}:"
    STLRuAdd_df[counter+1,"rownum"] <- STLRuAdd + counter
    counter <- counter + 1
    
    lruadd_start <- "  LRuAdd1[t] = "
    
    Mgmt <- 1
        
    for (b in 1:NRipSet) {
      
      for (f in 1:NRipConv) {
        
        FromHRU <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="FromHRU"&
                                                               hcamr_specsresults_append[[i]]$Identifier==paste(b,f,sep=";"),
                                                             "Value"])
        ToHRU <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="ToHRU"&
                                                             hcamr_specsresults_append[[i]]$Identifier==paste(b,f,sep=";"),
                                                           "Value"])
        
        for (HRU in 1:length(hcam_HRUs)) {
          
          ALuBase <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ALuBase"&
                                                                   hcamr_specsresults_append[[i]]$Identifier==as.character(HRU)),"Value"])
          for (g in 1:NRipLoads) {
            
            ARip <- as.numeric(hcamr_specsresults_append[[i]][c(hcamr_specsresults_append[[i]]$Variable=="ARip"&
                                                                  hcamr_specsresults_append[[i]]$Identifier==paste(b,f,g,sep=";")),"Value"])
            if (ARip > 0) {
              
              if(FromHRU = HRU) {
                
                lruadd <- paste0("LRuU",HRU,Mgmt,"1[t] * (-ARip",b,f,g,") * DRipSet",b,f,g,"1")
                
                if (NDrSet > 0) {
                  
                  for(DrSet in 1:NDrSet) {
                    
                    DrRemovalRate <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="DrRemovalRate"&
                                                                                 hcamr_specsresults_append[[i]]$Identifier==paste(HRU,DrSet,"1",sep=";"),
                                                                               "Value"]) / 100
                    
                    dr_set_else <- paste0("(1 - (",DrRemovalRate," * DDrSet",DrSet,"))")
                    
                    lruadd <- paste0(lruadd," * ",dr_set_else)
                    
                  }
                }
              } else if (ToHRU = HRU) {
                
                lruadd <- paste0("LRuU",HRU,Mgmt,"1[t] * ARip",b,f,g,") * DRipSet",b,f,g,"1")
                
                if (NDrSet > 0) {
                  
                  for(DrSet in 1:NDrSet) {
                    
                    DrRemovalRate <- as.numeric(hcamr_specsresults_append[[i]][hcamr_specsresults_append[[i]]$Variable=="DrRemovalRate"&
                                                                                 hcamr_specsresults_append[[i]]$Identifier==paste(HRU,DrSet,"1",sep=";"),
                                                                               "Value"]) / 100
                    
                    dr_set_else <- paste0("(1 - (",DrRemovalRate," * DDrSet",DrSet,"))")
                    
                    lruadd <- paste0(lruadd," * ",dr_set_else)
                    
                  }
                }
              }
            }
            
            if (exists("lruadd_rev")) {
              lruadd_rev <- paste0(lruadd_rev, " + ", lruadd)
            } else {
              lruadd_rev <- lruadd
            }
          }
        }
      }
    }
    
    STLRuAdd_df[counter+1,"Wmodel"] <- paste0(lruadd_start, lruadd_rev, ";")
    STLRuAdd_df[counter+1,"rownum"] <- STLRuAdd + counter
    
    rm(lruadd_rev)
    
    camwrap_wmodel[[i]] <- rbind(STLRuAdd_pre, STLRuAdd_df, STLRuAdd_post)
    camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
    final_row_upd <- nrow(camwrap_wmodel[[i]])
    
  }
    
  # LRe equation --------------------------------------------------------------
  STLRe <- camwrap_wmodel[[i]]$rownum[grep("subject to lre1",camwrap_wmodel[[i]]$Wmodel)]
  STLRe_pre <- camwrap_wmodel[[i]][1:(STLRe-1), ]
  STLRe_post <- camwrap_wmodel[[i]][STLRe+2:final_row_upd, ]
  
  counter = 0
  STLRe_df <- data.frame()
  
  STLRe_df[counter+1,"Wmodel"] <- "subject to lre1{t in time}:"
  STLRe_df[counter+1,"rownum"] <- STLRe + counter
  counter <- counter + 1
  
  lre_start <- "  LRe1[t] = "
  
  for (Mgmt in 1:length(camwrap_mgmt)) {
    
    for (HRU in 1:length(hcam_HRUs)) {
      
      if (Mgmt==1) {
        lre <- paste0("LReU",HRU,Mgmt,"1[t]"," * DALu",HRU,Mgmt)
        
        # Bind together
        if (exists("lre_rev")) {
          lre_rev <- paste0(lre_rev," + ",lre)
        } else {
          lre_rev <- lre
        }
        
      } else { 
        lre_else <- paste0("(LReU",HRU,Mgmt,"1[t] - LReU",HRU,"11[t]) * DALu",HRU,Mgmt)  
        
        # Bind together
        if (exists("lre_rev_else")) {
          lre_rev_else <- paste0(lre_rev_else," + ",lre_else)
        } else {
          lre_rev_else <- lre_else
        }
        
      }
      
    }
    
  }
  
  STLRe_df[counter+1,"Wmodel"] <- paste0(lre_start, lre_rev, " + ",lre_rev_else, ";")
  STLRe_df[counter+1,"rownum"] <- STLRe + counter
  
  rm(lre_rev, lre_rev_else, lre)
  
  camwrap_wmodel[[i]] <- rbind(STLRe_pre, STLRe_df, STLRe_post)
  camwrap_wmodel[[i]]$rownum <- seq.int(nrow(camwrap_wmodel[[i]]))
  final_row_upd <- nrow(camwrap_wmodel[[i]])
  
  # GwSw equation--------------------------------------------------------------
  text_find_gw <- "subject to gw_sw_one"
  
  KGw <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable == "KGw"])
  VGwI_Tm3 <- as.numeric(hcamr_specsresults_append[[i]]$Value[hcamr_specsresults_append[[i]]$Variable == "VGwI"]) / 
    Tm3ToMG
  
  
  camwrap_wmodel[[i]][grep(text_find_gw, camwrap_wmodel[[i]]$Wmodel)+1,"Wmodel"] <- paste0("  QGwSw[first(time)] = ",VGwI_Tm3*KGw,";")
  
  camwrap_wmodel[[i]][grep(text_find_gw, camwrap_wmodel[[i]]$Wmodel)+4,"Wmodel"] <- paste0("  QGwSw[t] = ",KGw," * VGw[t-1];")
  
  # .mod files
  write(camwrap_wmodel[[i]][!is.na(camwrap_wmodel[[i]]$Wmodel),"Wmodel"], file = paste0(outpath,"Wmodel_",clim,".mod"))
  
  ##### ----- outFiles.csv ----- #####
  
  # Develop outFiles.csv contingent on the model mode
  if (hcamr_specsresults_append[[i]]$Identifier[hcamr_specsresults_append[[i]]$Variable == "ModelMode"] == "Hydrology & Loadings") {
    
    outFile <- data.frame(
      clim_Run_Abbrev  = c(clim, clim),
      clim_TS_Path = paste0(outpath_clim, "Climate_", clim, ".csv"),
      SpecsResults_Path = paste0(outpath_specs, "SpecsResults", "_", clim, ".csv")
    )
    
    # Bind together
    if (exists("outFile_bind")) {
      outFile_bind <- rbind(outFile_bind, outFile)
    } else {
      outFile_bind <- outFile
    }
    
    write.table(
      outFile_bind,
      paste0(outpath, "outFiles", ".csv"),
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      sep = ","
    )
    
  } else {
    
    outFile <- data.frame(
      clim_Run_Abbrev  = clim,
      clim_TS_Path = paste0(outpath_clim, "Climate_", clim, ".csv"),
      SpecsResults_Path = paste0(outpath_specs, "SpecsResults", "_", clim, ".csv")
    )
    
    # Bind together
    if (exists("outFile_bind")) {
      outFile_bind <- rbind(outFile_bind, outFile)
    } else {
      outFile_bind <- outFile
    }
    
    # Write to file
    write.table(
      outFile_bind,
      paste0(outpath, "outFiles", ".txt"),
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE,
      sep = ","
    )
    
  }
  
}

################################################
# PART 4: QUALITY ASSURANCE
################################################

## Confirm consistency of climate time series files from HCAM and HCAM-R

# Reformat HCAM climate scenario data to match HCAM-R Climate_[climate_scenario].csv files

# Initialize lists
hcam_clim_ref <- list()
hcam_clim_rev <- list()
hcamr_clim_rev <- list()
hcam_hcamr_clim <- list()
hcamr_clim_ref <- list()

for (i in seq(clim_vec)) {
  clim <- clim_vec[i]
  
  # Reformat TimeStamp column
  hcam_clim_ref[[i]] <- mutate(
    hcam_climate[[i]],
    MO = # For runoff and recharge variables, the first number in the identifier refers to the month; for all other variables, this number refers to the HRU
      sapply(strsplit(Timestamp, "/"), function(x)
        x[1]),
    
    DA =
      sapply(strsplit(Timestamp, "/"), function(x)
        x[2]),
    
    YR =
      substr(sapply(strsplit(Timestamp, "/"), function(x)
        x[3]), 1, 4)
  )
  
  hcam_clim_rev[[i]] <- hcam_clim_ref[[i]] %>%
    select(c("MO", "DA", "YR", "PREC", "TEMP")) %>%
    mutate(date = as.Date(paste(DA, MO, YR, sep = "-"), "%d-%m-%Y")) %>%
    group_by(date) %>%
    summarise(PREC = sum(PREC),
              TEMP = mean(TEMP)) 

  hcam_clim_rev[[i]] <-
    hcam_clim_rev[[i]][, c("date", "PREC", "TEMP")]
  
  names(hcam_clim_rev[[i]]) <-
    c("Timestamp", "PREC_HCAM", "TEMP_HCAM")
  
  # Reformat hcamr climate data
  hcamr_clim_rev[[i]] <- hcamr_climate[[i]]
  hcamr_clim_rev[[i]]$Timestamp <-
    as.Date(as.factor(hcamr_clim_rev[[i]]$Timestamp), format = "%Y-%m-%d")
  
  names(hcamr_clim_rev[[i]]) <-
    c("Timestamp", "PREC_HCAMR", "TEMP_HCAMR")
  
  # Round temperature for comparison
  hcamr_clim_rev[[i]]$TEMP_HCAMR <- round(hcamr_clim_rev[[i]]$TEMP_HCAMR,1)
  
  hcam_hcamr_clim[[i]] <-
    merge(hcam_clim_rev[[i]], hcamr_clim_rev[[i]])
  
  hcam_hcamr_clim[[i]]$PREC_DIFF <-
    with(hcam_hcamr_clim[[i]], PREC_HCAMR - PREC_HCAM)
  
  hcam_hcamr_clim[[i]]$TEMP_DIFF <-
    with(hcam_hcamr_clim[[i]], TEMP_HCAMR - TEMP_HCAM)

}

# Review differences (_DIFF columns)
View(hcam_hcamr_clim[[1]])
View(hcam_hcamr_clim[[2]])
View(hcam_hcamr_clim[[3]])

Message_5 <-
  paste0(
    "NOTE TO USER: If PREC_DIFF and TEMP_DIFF are substantial, consider reviewing the units of climate data and adjusting prior to comparison."
  )
Message_5



