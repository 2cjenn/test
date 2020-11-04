# Jennifer Collister
# 30/09/20

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}


specs <- function() {
  
  # If you're here to write a new spec, you can run this source line interactively
  # to load all the variable derivation objects into your working environment
  # so you get autocomplete when typing them!
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
  
  TEUvars_common <- list(
    ID,
    BaC_Sex,
    TEU_BaC_DateOfBirth,
    Rec_DateAssess,
    TEU_BaC_AgeAtRec,
    TEU_ethnicgrp,
    TEU_Rec_AssessCentre,
    TEU_Rec_Country
  )
  
  TEUvars_BP <- list(
    TEU_BlP_SBP.0.0,
    TEU_BlP_SBP.0.1,
    TEU_BlP_DBP.0.0,
    TEU_BlP_DBP.0.1,
    TEU_BlP_nSBP,
    TEU_BlP_nDBP,
    TEU_BlP_SBP.avg,
    TEU_BlP_DBP.avg
  )
  
  HTN_control <- c(
    TEUvars_common,
    TEUvars_BP,
    TEU_VeI_HTN_prevalent(),
    list(
      TEU_selfrepHTN_dx,
      TEU_VeI_HTNmeds_rubric,
      TEU_selfrepHTN_meds,
      VeI_PregnantNow,
      TEU_BaC_AgeCat,
      TEU_BlP_measuredHTN,
      TEU_evidenceHTN,
      TEU_awareHTN,
      TEU_treatedHTN,
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
                   
  Cholesterol_PRS <- c(
    TEUvars_common,
    TEU_VeI_CVD_operation(dx_codes = c(1069, 1070, 1095, 1105)),
    list(
      TEU_BaC_AgeCat,
      TEU_HMH_BowelCancerScreen,
      TEU_Edu_HighestQual,
      TEU_Edu_ISCED,
      TEU_HoH_PreTaxInc,
      TEU_TownsendDepInd_Quint,
      TEU_HMH_Meds_Chol,
      TEU_Smo_Status,
      TEU_Alc_Status,
      TEU_Alc_WeeklyAlcUnits,
      TEU_Alc_Binge,
      TEU_Pha_METsover1200,
      TEU_FaH_CVD,
      TEU_BSM_BMIcat,
      TEU_BSM_WaistCircCat,
      TEU_LDL_C_PRS,
      TEU_LDL_C_PRS_deciles,
      GeP_Batch,
      TEU_HMH_VascCond,
      TEU_HMH_prevHTN,
      TEU_HMH_prevstroke,
      TEU_HMH_prevCVD,
      HMH_IllDisab,
      HMH_Diabetes,
      BBC_CHOL_Result,
      BBC_HDL_Result,
      BBC_LDL_Result,
      GeP_PC(pc=1),
      GeP_PC(pc=2),
      GeP_PC(pc=3),
      GeP_PC(pc=4),
      ADO_DateFirstMI,
      ADO_DateFirstIStroke
    )
  )
 
  return(environment())
}

TEU_SPECS <- specs()
