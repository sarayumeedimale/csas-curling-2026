library(tidyverse)

# ==============================================================================
# PART 1: EXTRACT MODEL SUMMARIES TO TEXT
# ==============================================================================
print("Starting Model Summary Extraction...")

# Check if model files exist
if (file.exists("wp_model_v2.rds")) {
  
  # Load the models
  # (Wrap in tryCatch in case v1 is missing, but v2 is the important one)
  model_v2 <- tryCatch(readRDS("wp_model_v2.rds"), error = function(e) NULL)
  model_v1 <- tryCatch(readRDS("wp_model.rds"), error = function(e) NULL)
  
  # Start writing to a text file
  sink("model_results_for_claude.txt")
  
  print("=========================================")
  print("PROJECT: CURLING POWER PLAY ANALYSIS")
  print("MODEL SUMMARY OUTPUT")
  print("=========================================")
  
  if (!is.null(model_v2)) {
    print("\n\n=========================================")
    print("FINAL MODEL (V2) SUMMARY")
    print("=========================================")
    print(summary(model_v2))
    
    print("\n\n=========================================")
    print("ODDS RATIOS (Effect Sizes)")
    print("Interpretable impact of variables on Win Prob")
    print("=========================================")
    # Exponentiate coefficients to get Odds Ratios
    print(exp(coef(model_v2)))
  }
  
  if (!is.null(model_v1)) {
    print("\n\n=========================================")
    print("BASELINE MODEL (V1) SUMMARY")
    print("=========================================")
    print(summary(model_v1))
  }
  
  sink() # Stop writing to file
  print("✅ SUCCESS: Created 'model_results_for_claude.txt'")
  
} else {
  print("⚠️ WARNING: Could not find 'wp_model_v2.rds'. Skipping model summary.")
}

# ==============================================================================
# PART 2: CONVERT RDS DATA TABLES TO CSV
# ==============================================================================
print("Starting CSV Conversion...")

# List of files you want to convert
# (Excluding the actual model objects since they can't be CSVs)
files_to_convert <- c(
  "pp_analysis_ready.rds",       # Note: likely too big to upload to Claude, but good to have
  "pp_pressure_step1.rds",
  "pp_pressure_step2.rds",
  "scoring_summary_table.rds",
  "shot_sequences_summary.rds",
  "shot_strategy_summary.rds",
  "team_profiles.rds",
  "execution_quality_summary.rds",
  "wp_predictions_v2.rds"
)

for (f in files_to_convert) {
  if (file.exists(f)) {
    tryCatch({
      # 1. Read the RDS
      data <- readRDS(f)
      
      # 2. Check if it's a dataframe/tibble (skippable if it's a list or model)
      if (is.data.frame(data) || is_tibble(data)) {
        
        # 3. Create new filename
        csv_name <- gsub("\\.rds$", ".csv", f)
        
        # 4. Save as CSV
        write_csv(data, csv_name)
        print(paste("✅ CONVERTED:", f, "->", csv_name))
        
      } else {
        print(paste("⚠️ SKIPPED:", f, "(Not a data table)"))
      }
    }, error = function(e) {
      print(paste("❌ ERROR converting", f, ":", e$message))
    })
  } else {
    print(paste("ℹ️ MISSING:", f))
  }
}

print("--- ALL OPERATIONS COMPLETE ---")