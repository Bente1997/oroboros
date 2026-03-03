# R/processing.R
# Core folder scan + DLD8 processing logic ported from the app.

# R/constants.R

# Cleaning/calibration/background protocols to exclude.
EXCLUDED_PROTOCOLS <- c(
  "O2_calibration_air",
  "Instrumental_high_O2_background_manual_injections",
  "Instrumental_high_O2_background_TIP2k",
  "Instrumental_O2_background_manual_injections_Dit",
  "Instrumental_O2_background_manual_injections_H2",
  "Instrumental_O2_background_manual_injections_sV",
  "Instrumental_O2_background_TIP2k",
  "Instrumental_O2_background_TIP2k_sV",
  "Instrumental_high_O2_background_and_zero_manual injections",
  "Instrumental_high_O2_background_and_zero_manual_injections",
  "Instrumental_high_O2_background_and_zero_TIP2k",
  "Instrumental_O2_background_and_zero_manual_injections",
  "Instrumental_O2_background_and_zero_manual_injections_sV",
  "Instrumental_O2_background_and_zero_TIP2k",
  "Instrumental_O2_background_and_zero_TIP2k_sV",
  "O2_calibration_air",
  "O2_calibration_air_and_zero",
  "Cleaning_AfterUse",
  "Cleaning_AfterUse_inhibitors",
  "Cleaning_AfterUse_instrumental",
  "Cleaning_AfterUse_stirrers",
  "Cleaning_BeforeUse",
  "O2k-cleaning_before_use",
  "MgG_before_use",
  "MgG_Calibration_and_Kd_determination_ADP_Mg",
  "MgG_Calibration_and_Kd_determination_ATP_Mg",
  "Fluo_calibration",
  "Saf_calibration",
  "AmR_background_fluorescence_slope",
  "AmR_calibration"
)

# Temperature checks
EXPECTED_OROBOROS_TEMPERATURE <- 37.0
