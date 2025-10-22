## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "bird_modelPredict",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Sourav", ""), family = "Das", role = c("aut", "cre"), email = "souravdron@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(bird_modelPredict = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "bird_modelPredict.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2","SpaDES.core", "tidyterra", "viridis", "gbm","crayon"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "png", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("predictStartYear", "numeric", NA, NA, NA, 
                    "First year for prediction"),
    defineParameter("predictEndYear", "numeric", NA, NA, NA, 
                    "Last year for prediction"),
    defineParameter("predictInterval", "numeric", 1, NA, NA, 
                    "Prediction year interval (e.g., every 5 years)")
    
  ),
  inputObjects = bindrows(
    expectsInput("sim", "list", "Output list from bird_dataPrep module, containing stack_list"),
    expectsInput("studyArea", "SpatVector", "Study area polygon with subUnit attribute"),
    expectsInput("modelFolder", "character", "Path to folder where species models are downloaded")
  ),
  outputObjects = bindrows(
    createsOutput("predictedList", "list", "Nested list of prediction raster stacks: predictedList[[species]][[year]]"),
    createsOutput("summaries", "list", "List of raster summary maps (mean and SD) for each species-year combination")
  )
))

doEvent.bird_modelPredict = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "bird_modelPredict", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "bird_modelPredict", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "bird_modelPredict", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "bird_modelPredict", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "bird_modelPredict", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "bird_modelPredict", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
   
  years <- seq(P(sim)$predictStartYear, P(sim)$predictEndYear, by = P(sim)$predictInterval)
  bcr_code <- unique(sim$studyArea$subUnit)
  

  sim$predictedList <- list()
  sim$summaries <- list()
  # Predict species models  

  
  # ## Call prediction & summaries
  # sim <- PredictAllSpecies_blist(sim, year = "2010", nBoot = 10, modelFolder = file.path(inputPath(sim)), outFolder= "predictions") #modelFolder = sim$modelFolder, year = P(sim)$predictionYear,nBoot = P(sim)$nBoot
  # ## currently, the model is predicted for a single year and XX boot. we need to connect this with the year from setupPorject call (may be not? its better to keep them seperate)
  # bcr_code <- unique(sim$studyArea$subUnit)
  # for (speciesName in names(sim$predictedList)) {
  #   sim$summaries[[speciesName]] <- SummarizeAndSaveBootstrapStack(sim, speciesName, bcr_code)
  # }
  for (yr in years) {
    message(green("Running predictions for year: "), yellow(yr))
    
    sim <- PredictAllSpecies_blist(sim, year = as.character(yr), nBoot = 2,  #10
                                   modelFolder = file.path(inputPath(sim)), 
                                   outFolder = file.path(outputPath(sim), "predictions"))
    
    # for (speciesName in names(sim$predictedList)) {
    #   sim$summaries[[paste0(speciesName, "_", yr)]] <- 
    #     SummarizeAndSaveBootstrapStack(sim, speciesName, bcr_code,outFolder = file.path(outputPath(sim), "predictions"))
    # }
    for (speciesName in names(sim$predictedList)) {
      for (yr in names(sim$predictedList[[speciesName]])) {
        sim$summaries[[paste0(speciesName, "_", yr)]] <- 
          SummarizeAndSaveBootstrapStack(sim, speciesName, bcr_code,
                                         year = yr,
                                         outFolder = file.path(outputPath(sim), "predictions"))
      }
    }
    
  }
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}
PredictAllSpecies_blist <- function(sim, year, nBoot = NULL, modelFolder, outFolder ) {
  bcr_code <- unique(sim$studyArea$subUnit)
  if (length(bcr_code) != 1) stop("studyArea must have a single BCR subUnit.")
  
  if (!(year %in% names(sim$stack_list))) {
    warning("Year ", year, " not found in stack_list. Skipping prediction for this year.")
    return(invisible(sim))
    }
  
  modelFiles <- list.files(modelFolder, pattern = "\\.Rdata$", full.names = TRUE) ## also need to add BCR_code, incase more than one BCR are stored in the folder 
  dir.create(outFolder, recursive = TRUE, showWarnings = FALSE)
  
  for (modelPath in modelFiles) {
    message("Processing model file: ", modelPath)
    load(modelPath)  # loads b.list, this needs to be aligned with the model objects, when single model object contains all bootstrap, they are contained under b.list. 
    
    speciesName <- sub("_can.*$", "", basename(modelPath))
    
    boot_indices <- seq_along(b.list)
    if (!is.null(nBoot)) {
      boot_indices <- boot_indices[1:min(nBoot, length(boot_indices))]
    }
    
    boot_rasters <- list()
    for (bootNum in boot_indices) {
      message(" - Bootstrap: ", bootNum)
      
      ras_stack <- sim$stack_list[[year]]
      
      model_vars <- b.list[[bootNum]][["var.names"]]
      ras_vars <- names(ras_stack)
      missing_vars <- setdiff(model_vars, ras_vars)
      
      if (length(missing_vars) > 0) {
        warning("Missing vars for ", speciesName, " bootstrap ", bootNum, ": ", paste(missing_vars, collapse=", "))
        next
      }
      
      ras_subset <- terra::subset(ras_stack, model_vars)
      
      pred_raster <- terra::predict(ras_subset, b.list[[bootNum]], type = "response")
      
      boot_rasters[[bootNum]] <- pred_raster
    }
    
    boot_stack <- rast(boot_rasters)
    names(boot_stack) <- paste0("b", seq_len(nlyr(boot_stack)), "_",year)
    #sim$predictedList[[speciesName]] <- boot_stack
    sim$predictedList[[speciesName]][[as.character(year)]] <- boot_stack
    # outPath <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code, "_prediction.tif"))
    outPath <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code, "_prediction_", year, ".tif"))
    
    terra::writeRaster(boot_stack, outPath, overwrite = TRUE)
    
    message("Prediction stack saved for ", speciesName)
  }

  #return(invisible(sim))
}


# Summarize Bootstrap 
# SummarizeAndSaveBootstrapStack <- function(sim, speciesName, bcr_code, outFolder ) {
#   boot_stack <- sim$predictedList[[speciesName]]
#   mean_r <- terra::app(boot_stack, mean, na.rm = TRUE)
#   sd_r <- terra::app(boot_stack, sd, na.rm = TRUE)
#   mean_path <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code, "_mean.tif"))
#   sd_path <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code, "_sd.tif"))
#   terra::writeRaster(mean_r, mean_path, overwrite = TRUE)
#   terra::writeRaster(sd_r, sd_path, overwrite = TRUE)
#   return(list(mean = mean_r, sd = sd_r))
# }

# SummarizeAndSaveBootstrapStack <- function(sim, speciesName, bcr_code, outFolder = "predictions") {
SummarizeAndSaveBootstrapStack <- function(sim, speciesName, bcr_code, year, outFolder = "predictions") {

  for (year in names(sim$predictedList[[speciesName]])) {
    boot_stack <- sim$predictedList[[speciesName]][[year]]
    mean_r <- terra::app(boot_stack, mean, na.rm = TRUE)
    sd_r <- terra::app(boot_stack, sd, na.rm = TRUE)
    
    mean_path <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code, "_", year, "_mean.tif"))
    sd_path <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code, "_", year, "_sd.tif"))
    
    terra::writeRaster(mean_r, mean_path, overwrite = TRUE)
    terra::writeRaster(sd_r, sd_path, overwrite = TRUE)
    
    sim$summaries[[paste0(speciesName, "_", year)]] <- list(mean = mean_r, sd = sd_r)
  }
  #return(invisible(sim))
}


gg_predMap <- function(x, title = "") {
  if (terra::is.factor(x)) {
    ggplot() +
      tidyterra::geom_spatraster(data = x) +
      tidyterra::scale_fill_coltab(data = x, na.value = "transparent") +
      ggtitle(title)
  } else {
    ggplot() +
      tidyterra::geom_spatraster(data = x * 1.0) +
      viridis::scale_fill_viridis(na.value = "transparent") +
      ggtitle(title)
  }
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
# plotFun <- function(sim) {
#   for (speciesName in names(sim$summaries)) {
#     summary <- sim$summaries[[speciesName]]
#     
#     Plots(summary$mean,
#           fn = gg_predMap,
#           types = P(sim)$.plots,
#           filename = paste0(speciesName, "_mean_yr_", time(sim)),
#           title = paste(speciesName, "Mean", time(sim)),
#           path = file.path(figurePath(sim)))
#     
#     Plots(summary$sd,
#           fn = gg_predMap,
#           types = P(sim)$.plots,
#           filename = paste0(speciesName, "_sd_yr_", time(sim)),
#           title = paste(speciesName, "SD", time(sim)),
#           path = file.path(figurePath(sim)))
#   }
#   
#   return(invisible(sim))
# }
plotFun <- function(sim) {
  for (key in names(sim$summaries)) {
    summary <- sim$summaries[[key]]
    parts <- strsplit(key, "_")[[1]]
    speciesName <- paste(parts[-length(parts)], collapse = "_")
    yr <- parts[length(parts)]
    
    Plots(summary$mean,
          fn = gg_predMap,
          types = P(sim)$.plots,
          filename = paste0(speciesName, "_mean_yr_", yr),
          title = paste(speciesName, "Mean", yr),
          path = file.path(figurePath(sim)))
    
    Plots(summary$sd,
          fn = gg_predMap,
          types = P(sim)$.plots,
          filename = paste0(speciesName, "_sd_yr_", yr),
          title = paste(speciesName, "SD", yr),
          path = file.path(figurePath(sim)))
  }
  
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
# 
# ggplotFn <- function(data, ...) {
#   ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
#     ggplot2::geom_histogram(...)
# }

