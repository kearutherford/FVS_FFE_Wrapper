
createCSV <- function(db_list, output_path) {

  # create empty dataframes to fill
  output_fire <- data.frame(
    CaseID = as.character("create_df"),
    StandID = as.character(NA),
    Year = as.integer(NA),
    Surf_Flame_Sev = as.double(NA),
    Surf_Flame_Mod = as.double(NA),
    Tot_Flame_Sev = as.double(NA),
    Tot_Flame_Mod = as.double(NA),
    Fire_Type_Sev = as.character(NA),
    Fire_Type_Mod = as.character(NA),
    PTorch_Sev = as.double(NA),
    PTorch_Mod = as.double(NA),
    Torch_Index = as.double(NA),
    Crown_Index = as.double(NA),
    Canopy_Ht = as.integer(NA),
    Canopy_Density = as.double(NA),
    Mortality_BA_Sev = as.double(NA),
    Mortality_BA_Mod = as.double(NA),
    Mortality_VOL_Sev = as.double(NA),
    Mortality_VOL_Mod = as.double(NA),
    Pot_Smoke_Sev = as.double(NA),
    Pot_Smoke_Mod = as.double(NA),
    Fuel_Mod1 = as.integer(NA),
    Fuel_Mod2 = as.integer(NA),
    Fuel_Mod3 = as.integer(NA),
    Fuel_Mod4 = as.integer(NA),
    Fuel_Wt1 = as.double(NA),
    Fuel_Wt2 = as.double(NA),
    Fuel_Wt3 = as.double(NA),
    Fuel_Wt4 = as.double(NA))

  output_tree <- data.frame(
    CaseID = as.character("create_df"),
    StandID = as.character(NA),
    Year = as.integer(NA),
    PrdLen = as.integer(NA),
    TreeId = as.character(NA),
    TreeIndex = as.integer(NA),
    SpeciesFVS = as.character(NA),
    SpeciesPLANTS = as.character(NA),
    SpeciesFIA = as.character(NA),
    TreeVal = as.integer(NA),
    SSCD = as.integer(NA),
    PtIndex = as.integer(NA),
    TPH = as.double(NA),
    MortPH = as.double(NA),
    DBH = as.double(NA),
    DG = as.double(NA),
    Ht = as.double(NA),
    HtG = as.double(NA),
    PctCr = as.integer(NA),
    CrWidth = as.double(NA),
    MistCD = as.integer(NA),
    BAPctile = as.double(NA),
    PtBAL = as.double(NA),
    TCuM = as.double(NA),
    MCuM = as.double(NA),
    CCum = as.double(NA),
    MDefect = as.integer(NA),
    BDefect = as.integer(NA),
    TruncHt = as.integer(NA),
    EstHt = as.double(NA),
    ActPt = as.integer(NA),
    Ht2TDCM = as.double(NA),
    Ht2TDBM = as.double(NA),
    TreeAge = as.double(NA))

  output_mort <- data.frame(
    CaseID = as.character("create_df"),
    StandID = as.character(NA),
    Year = as.integer(NA),
    SpeciesFVS = as.character(NA),
    SpeciesPLANTS = as.character(NA),
    SpeciesFIA = as.character(NA),
    Killed_class1 = as.double(NA),
    Total_class1 = as.double(NA),
    Killed_class2 = as.double(NA),
    Total_class2 = as.double(NA),
    Killed_class3 = as.double(NA),
    Total_class3 = as.double(NA),
    Killed_class4 = as.double(NA),
    Total_class4 = as.double(NA),
    Killed_class5 = as.double(NA),
    Total_class5 = as.double(NA),
    Killed_class6 = as.double(NA),
    Total_class6 = as.double(NA),
    Killed_class7 = as.double(NA),
    Total_class7 = as.double(NA),
    Bakill = as.double(NA),
    Volkill = as.double(NA))

  # loop through each fvs output database
  n <- length(db_list$potf)

  for(i in 1:n) {

    # potential fire --------------------------------------
    # pull db file name
    potf_run_id <- db_list$potf[i]
    potf_file_path <- paste0(output_path,'/',potf_run_id)

    if(str_detect(potf_run_id, "pre")) {
      potf_time <- 2021
    } else if(str_detect(potf_run_id, "post")) {
      potf_time <- 2022
    }

    # grab necessary tables from the db
    potf_run_db <- dbConnect(RSQLite::SQLite(), potf_file_path)
    fire_table <- dbReadTable(potf_run_db, "FVS_PotFire_Metric")
    tree_table <- dbReadTable(potf_run_db, "FVS_TreeList_Metric")
    tree_table_sub <- tree_table %>% filter(Year == potf_time)
    dbDisconnect(potf_run_db) # must disconnect

    # row bind to existing dataframe
    output_fire <- rbind(output_fire, fire_table)
    output_tree <- rbind(output_tree, tree_table_sub)

    # potential fire --------------------------------------
    # pull db file name
    sim_run_id <- db_list$sim[i]
    sim_file_path <- paste0(output_path,'/',sim_run_id)

    # grab necessary tables from the db
    sim_run_db <- dbConnect(RSQLite::SQLite(), sim_file_path)
    mort_table <- dbReadTable(sim_run_db, "FVS_Mortality_Metric")
    dbDisconnect(sim_run_db) # must disconnect

    # row bind to existing dataframe
    if(nrow(mort_table > 0)) {
      output_mort <- rbind(output_mort, mort_table)
    }

  }

  # write csv files
  write.csv(output_fire, here::here("output_fvs/fvs_fire.csv"))
  write.csv(output_tree, here::here("output_fvs/fvs_tree.csv"))
  write.csv(output_mort, here::here("output_fvs/fvs_mort.csv"))

  print("DONE")

}
