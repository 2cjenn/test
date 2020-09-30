# Jennifer Collister
# 30/09/20

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}




specs <- function() {
  # Source the variable maps
  source(
    file.path(
      config$scripts$cleaning,
      "Reorganise",
      "common_derivations.R"
    ),
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
                         TEU_BaC_AgeAtRec
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
  
  SPEC_Neo_HTN <- c(TEUvars_common,
                    TEUvars_BP,
                    list(TEU_BaC_AgeCat,
                         TEU_BlP_measuredHTN,
                         TEU_HMH_BowelCancerScreen,
                         TEU_Edu_HighestQual,
                         TEU_Edu_ISCED,
                         TEU_TownsendDepInd_Quint
                         )
                    )
  return(environment())
}

testSpecs <- specs()
