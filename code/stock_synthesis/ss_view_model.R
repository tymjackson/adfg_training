# load ----

source("./code/stock_synthesis/ss_output_functions.R")

# view outputs ----

## directory with ssoutput files
dir <- "./code/stock_synthesis/23.3"
## report file
report <- SS_output(dir)
## standard SS plots
SS_plots(report, btarg = F, sprtarg = F)

# retrospective analysis, peel 5 yr ----

# create directory for retrospective runs
retro_dir <- file.path(getwd(), "code/stock_synthesis/23.3/retro")
copy_SS_inputs(dir.old = dir,
               dir.new = retro_dir)
# copy executable file
file.copy("./code/stock_synthesis/23.3/ss.exe",
          "./code/stock_synthesis/23.3/retro/ss.exe")

## do analysis
SS_doRetro(masterdir = retro_dir, oldsubdir = "", newsubdir = "retrospectives",
           years = 0:-5, exefile = "ss.exe")

## view retrospective output
retro_models <- SSgetoutput(dirvec = file.path(retro_dir, "retrospectives",
                                               paste("retro", 0:-5, sep = "")))
retro_summary <- SSsummarize(retro_models)
endyrvec <- retro_summary[["endyrs"]] + 0:-5

dir.create(file.path(retro_dir, "plots"))
SSplotComparisons(retro_summary, endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"), btarg = F, sprtarg = F,
                  plotdir =  "./code//stock_synthesis/23.3/retro/plots", print = T)
