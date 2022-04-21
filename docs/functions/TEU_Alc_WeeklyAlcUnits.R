# This file has been automatically generated during the data derivation process.
# It contains the functions used to derive each data field, and should be viewed in conjunction with the data dictionary html.
# It will not run on its own.


TEU_Alc_WeeklyAlcUnits <- function(data) {
      alcservings <- data
      for (alc in c(
        "Alc_RedWineWk.0.0",
        "Alc_WhiteWineWk.0.0",
        "Alc_BeerCiderWk.0.0",
        "Alc_SpiritsWk.0.0",
        "Alc_FortWineWk.0.0",
        "Alc_OtherAlcWk.0.0"
      )) {
        alcservings[[alc]][alcservings[[alc]] < 0 | is.na(alcservings[[alc]])] <-  0
      }
      
      weekly_alcunits <-
        #	Red wine (1 glass, 125ml, ABV 12% = 1.5 units)
        (1.5 * alcservings$Alc_RedWineWk.0.0) +
        # White wine, champagne (1 glass, 125ml, ABV 12% = 1.5 units)
        (1.5 * alcservings$Alc_WhiteWineWk.0.0) +
        #	Fortified wines: e.g. sherry, port (1 measure, 50ml, ABV 20% = 1 unit)
        (1.0 * alcservings$Alc_FortWineWk.0.0) +
        #	Beer, cider including bitter, lager, stout, ale, Guinness (1 pint, 568ml, ABV 3.6% = 2 units)
        (2.0 * alcservings$Alc_BeerCiderWk.0.0) +
        #	Spirits, liquors (1 measure or shot, 25ml, ABV 40% = 1 unit)
        (1.0 * alcservings$Alc_SpiritsWk.0.0) +
        #	For "other" types of alcohol, will use alcopops as proxy ( 1 drink, 275ml, ABV 5.5% = 1.5 units)
        (1.5 * alcservings$Alc_OtherAlcWk.0.0)
      
      # Truncate alcohol consumption at upper 95th percentile
      upper95 <- quantile(weekly_alcunits, 0.95, na.rm = TRUE)
      weekly_alcunits[weekly_alcunits > upper95 & !is.na(weekly_alcunits)] <- upper95
      weekly_alcunits[is.na(weekly_alcunits)] <- 0
      
      return(weekly_alcunits)
    }


