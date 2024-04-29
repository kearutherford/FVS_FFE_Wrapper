
runFVS <- function(tree_df, plot_df, extra_input, output_path) {

  # check that all plots are in the input tree
  if(!all(is.element(extra_input$StandPlot_ID, tree_df$StandPlot_ID))) {

    missing_plots <- paste0(unique(extra_input[!is.element(extra_input$StandPlot_ID, tree_df$StandPlot_ID), "StandPlot_ID"]), sep = "  ")

    stop('Plots missing in the input tree list: ', missing_plots)

  }

  # change working directory just for this code chunk
  setwd(output_path)

  # loop through ForSys (n = number of plots)
  n <- nrow(extra_input)
  potf_db_names <- c()
  sim_db_names <- c()

  for(i in 1:n) {

    # get unique forest/unit/plot ID
    plot_id <- extra_input$StandPlot_ID[i]

    # subset dataframes
    extra_pull <- subset(extra_input, StandPlot_ID == plot_id)
    plot_pull <- subset(plot_df, StandPlot_ID == plot_id)
    tree_pull <- subset(tree_df, StandPlot_ID == plot_id)

    # create db file
    output_path_db <- paste0(output_path,'/',plot_id,'.db')
    run_db <- dbConnect(RSQLite::SQLite(), output_path_db)
    dbWriteTable(run_db, "FVS_PlotInit", plot_pull, overwrite = TRUE)
    dbWriteTable(run_db, "FVS_TreeInit", tree_pull, overwrite = TRUE)
    dbDisconnect(run_db) # must disconnect

    # create keyword files
    # formatting (including spaces) is very specific
    fm1_raw <- extra_pull$FM1
    fm2_raw <- extra_pull$FM2
    wind <- extra_pull$wind
    temp <- extra_pull$temp

    n1 <- nchar(fm1_raw)
    n2 <- nchar(fm2_raw)

    if(n1 == 1) {
      fm1 <- paste0('  ',fm1_raw)
    } else if (n1 == 2) {
      fm1 <- paste0(' ',fm1_raw)
    } else if (n1 == 3) {
      fm1 <- paste0(fm1_raw)
    }

    if(n2 == 1) {
      fm2 <- paste0('  ',fm2_raw)
    } else if (n2 == 2) {
      fm2 <- paste0(' ',fm2_raw)
    } else if (n2 == 3) {
      fm2 <- paste0(fm2_raw)
    }

    # keyword file for potential fire
    a <- paste0('STDIDENT\n',plot_id)
    # break
    b <- paste0("DATABASE\nDSNIN\n",plot_id,".db\nStandSQL\nSELECT * FROM FVS_PlotInit WHERE StandPlot_ID = '",plot_id,"'\nEndSQL\nTreeSQL\nSELECT * FROM FVS_TreeInit WHERE StandPlot_ID = '",plot_id,"'\nEndSQL\nEND")
    # break
    c1 <- "TIMEINT                      1"
    c2 <- "NUMCYCLE           1"
    c3 <- "TREELIST"
    # break
    d1 <- "FMIN"
    d2 <- paste0('POTFWIND          ',wind,'        ',wind)
    d3 <- paste0('POTFTEMP          ',temp,'        ',temp)
    d4 <- "POTFSEAS           3         3"
    d5 <- "POTFMOIS           1         4         4         5        10        15        70        70"
    d6 <- "POTFMOIS           2         4         4         5        10        15        70        70"
    d7 <- paste0('FUELMODL           1       ',fm1,'       0.5       ',fm2,'       0.5\n') # there must be a blank line after this keyword
    d8 <- "CANCALC                    1.3                    "
    d9 <- "POTFIRE"
    d10 <- "END"
    # break
    e1 <- paste0('DATABASE\nDSNOUT\n',plot_id,'-potf_out.db\nSUMMARY')
    e2 <- "POTFIRDB           2"
    e3 <- "TREELIDB           2"
    e4 <- "END"
    # break
    f <- "PROCESS\nSTOP"

    potf_kw <- paste0(a,'\n\n',b,'\n\n',c1,'\n',c2,'\n',c3,'\n\n',d1,'\n',d2,'\n',d3,'\n',d4,'\n',d5,'\n',d6,'\n',d7,'\n',d8,'\n',d9,'\n',d10,'\n\n',e1,'\n',e2,'\n',e3,'\n',e4,'\n\n',f)
    potf_kw_out_path <- paste0(output_path,'/',plot_id,'-potf_kw.txt')
    writeLines(potf_kw, potf_kw_out_path)

    # keyword file for simulated fire
    g1 <- "FMIN"
    g2 <- paste0('SIMFIRE            1        ',wind,'         1        ',temp,'         1       100         3') # in km/h and deg. C
    g3 <- paste0('FUELMODL           1       ',fm1,'       0.5       ',fm2,'       0.5\n') # there must be a blank line after this keyword
    g4 <- "MORTCLAS           0       7.5      17.5       100       101       102       103" # in cm
    g5 <- "CANCALC                    1.3                    "
    g6 <- "MORTREPT\nBURNREPT"
    g7 <- "END"
    # break
    h1 <- paste0('DATABASE\nDSNOUT\n',plot_id,'-sim_out.db\nSUMMARY')
    h2 <- "MORTREDB           2"
    h3 <- "BURNREDB           2"
    h4 <- "END"

    sim_kw <- paste0(a,'\n\n',b,'\n\n',c1,'\n',c2,'\n\n',g1,'\n',g2,'\n',g3,'\n',g4,'\n',g5,'\n',g6,'\n',g7,'\n\n',h1,'\n',h2,'\n',h3,'\n',h4,'\n\n',f)
    sim_kw_out_path <- paste0(output_path,'/',plot_id,'-sim_kw.txt')
    writeLines(sim_kw, sim_kw_out_path)

    # run through ForSys (potential fire)
    potf_kw_file <- paste0(plot_id,'-potf_kw.txt')
    potf_out_txt_file <- paste0(plot_id,'-potf_out.txt')
    db_file <- paste0(plot_id,'.db')

    potf_command <- c(potf_kw_file, db_file, potf_out_txt_file, "", "", "")
    system2("FVSbc.exe", input = potf_command)

    # run through ForSys (simulated fire)
    sim_kw_file <- paste0(plot_id,'-sim_kw.txt')
    sim_out_txt_file <- paste0(plot_id,'-sim_out.txt')

    sim_command <- c(sim_kw_file, db_file, sim_out_txt_file, "", "", "")
    system2("FVSbc.exe", input = sim_command)

    # add db names to output vectors
    potf_out_db <- paste0(plot_id,'-potf_out.db')
    potf_db_names <- c(potf_db_names, potf_out_db)

    sim_out_db <- paste0(plot_id,'-sim_out.db')
    sim_db_names <- c(sim_db_names, sim_out_db)

  }

  out_list <- list("potf" = potf_db_names, "sim" = sim_db_names)
  return(out_list)

}
