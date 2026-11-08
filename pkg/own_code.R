# libraries
library(here)
library(dplyr)

# data 
AT_SAV <- file.path(here::here("pkg", "Altermann_LongtermExercise&Attention.sav"))
AT_RAW <- haven::read_sav(AT_SAV)
AT_D0 <- haven::zap_labels(AT_RAW)
AT_DONE <- !is.na(AT_D0$Concentration_KL_POST) & AT_D0$Concentration_KL_POST > 0
AT_D <- AT_D0[AT_DONE, ]

# Check pretest scores consistency
AT_D0$Concentration_KL_PRE_recalculated <- AT_D0$Speed_BZO_PRE - AT_D0$Ommisions_PRE - AT_D0$Commisions_PRE
AT_D0$ErrorScore_PRE_recalculated <- AT_D0$Concentration_KL_PRE_recalculated / AT_D0$Speed_BZO_PRE * 100
AT_D0$diff_Concentration_KL_PRE <- AT_D0$Concentration_KL_PRE - AT_D0$Concentration_KL_PRE_recalculated

# Check posttest scores consistency
AT_D0$Concentration_KL_POST_recalculated <- AT_D0$Speed_BZO_POST - AT_D0$Ommisions_POST - AT_D0$Commisions_POST
AT_D0$ErrorScore_POST_recalculated <- AT_D0$Concentration_KL_POST_recalculated / AT_D0$Speed_BZO_POST * 100
AT_D0$diff_Concentration_KL_POST <- AT_D0$Concentration_KL_POST - AT_D0$Concentration_KL_POST_recalculated

# Check age x gender strata per study group
df_aggregate <- AT_D |> dplyr::group_by(Intervention, Age, Gender) |> 
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last")

# Check average PE hours per study group
df_pe_hours <- AT_D |> dplyr::group_by(Intervention) |> 
  dplyr::summarise(mean_pe_hours = mean(PE_week))

# Check group count from initial sample
df_dist_init <- AT_D0 |> dplyr::group_by(Intervention) |> 
  dplyr::summarise(n = dplyr::n(), .groups = "drop_last")
