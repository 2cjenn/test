# Jennifer Collister
# 30/09/20

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}




specs <- function() {
  # Source the variable maps
  source(file.path(config$scripts$cleaning, "Reorganise", "common_derivations.R"),
         local = if (sys.nframe() == 0L) {
           FALSE
           } else {
             TEUmaps <- new.env()
             }
         )
  if (exists("TEUmaps")) {
    attach(TEUmaps)
    on.exit(detach(TEUmaps))
  }
  
  # Dataset specifications
  
  TEUvars_common <- list(ID,
                         BaC_Sex,
                         TEU_BaC_DateOfBirth,
                         Rec_DateAssess,
                         TEU_BaC_AgeAtRec,
                         TEU_ethnicgrp,
                         TEU_Rec_Country
                         )
  
  TEUvars_BP <- list(TEU_BlP_SBP.0.0,
                     TEU_BlP_SBP.0.1,
                     TEU_BlP_DBP.0.0,
                     TEU_BlP_DBP.0.1,
                     TEU_BlP_nSBP,
                     TEU_BlP_nDBP,
                     TEU_BlP_SBP.avg,
                     TEU_BlP_DBP.avg
                     )
  
  HTN_control <- c(TEUvars_common,
                    TEUvars_BP,
                    list(VeI_PregnantNow,
                         TEU_BaC_AgeCat,
                         TEU_BlP_measuredHTN,
                         TEU_HMH_BowelCancerScreen,
                         TEU_Edu_HighestQual,
                         TEU_Edu_ISCED,
                         TEU_HoH_PreTaxInc,
                         TEU_TownsendDepInd_Quint,
                         TEU_HMH_Meds_BP,
                         TEU_Smo_Status,
                         TEU_Alc_Status,
                         TEU_Alc_WeeklyAlcUnits,
                         TEU_Alc_Binge,
                         TEU_Pha_METsover1200,
                         TEU_FaH_CVD,
                         TEU_BSM_BMIcat,
                         TEU_BSM_WaistCircCat,
                         TEU_SBP_PRS,
                         TEU_DBP_PRS,
                         TEU_BP_PRS,
                         TEU_BP_PRS_quintiles,
                         TEU_HMH_VascCond,
                         TEU_HMH_prevHTN,
                         TEU_HMH_prevstroke,
                         TEU_HMH_prevCVD,
                         HMH_IllDisab,
                         HMH_Diabetes,
                         HMH_HTNAge,
                         TEU_BlP_HTNseverity
                         
                         )
                    )
  return(environment())
}

TEU_SPECS <- specs()
