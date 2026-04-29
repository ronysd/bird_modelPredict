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
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9003)", "ggplot2","SpaDES.core", "tidyterra", "viridis", "gbm"),
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
                    desc = "First year to predict the model"),
    defineParameter("predictEndYear", "numeric", NA, NA, NA,
                    desc = "Last year to predict the model"),
    defineParameter("predictInterval", "numeric", 1, NA, NA,
                    desc = "Interval between years to predict")
    
  ),
  inputObjects = bindrows(
    expectsInput("stack_list", "list", "Raster stacks created in DataPrep or translator module"),
    expectsInput("studyArea", "SpatVector", "Study area polygon"),
    expectsInput("modelFolder", "character", "Local path to downloaded models objects per species per BCR"),
    expectsInput("climateYear", "numeric", "Optional year override for dynamic predictions; used when running NRV-style single-year predictions")
  ),
  outputObjects = bindrows(
    createsOutput("predictedList", "list", "Prediction raster stacks for all species")
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
      # sim <- scheduleEvent(sim, P(sim)$predictStartYear, "bird_modelPredict", "predict")
      
      ## for situations when translator is needed before predict module
      if (!is.null(sim$stack_list[[as.character(P(sim)$predictStartYear)]])) {
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "bird_modelPredict", "predict", eventPriority = 7)
      }
      # sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "bird_modelPredict", "plot")
      # sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "bird_modelPredict", "save")
      
    },
    predict = {
      sim <- Predict(sim)
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
  
  message("Initializing bird_modelPredict...")
  
  # required objects from dataPrep / translator
  if (is.null(sim$stack_list))
    stop("stack_list must be supplied to bird_modelPredict module.")
  if (is.null(sim$studyArea))
    stop("studyArea must be supplied to bird_modelPredict module.")
  if (is.null(P(sim)$modelFolder))
    stop("modelFolder must be supplied to bird_modelPredict module.")
  
  # initialize outputs
  if (is.null(sim$predictedList)) sim$predictedList <- list()
  if (is.null(sim$summaries))     sim$summaries     <- list()
  
  return(invisible(sim))
}

Predict <- function(sim) {
  #browser()
  #year <- time(sim)
  
  ## MODIFICATION FOR NEW climateYear Module
  # year <- if (!is.null(sim$climateYear)) sim$climateYear else time(sim)
  year <- time(sim)
  climate_year <- if (!is.null(sim$climateYear)) sim$climateYear else year
  
  message(bold$green("Running bird_modelPredict for year: "), bold$yellow(year))
  #browser()
  ##ensure co-variate list exists
  # if (is.null(sim$stack_list[[as.character(year)]])) {
  #   # Trigger translator to build it
  #   if ("bird_covariateTranslator" %in% modules(sim)) {
  #     message("Scheduling translator to build covariates for ", year)
  #     sim <- scheduleEvent(sim, time(sim), "bird_covariateTranslator", "buildCovariates", eventPriority = 1)
  #     
  #     sim <- scheduleEvent(sim, time(sim), "bird_modelPredict", "predict", eventPriority = 2)
  #     return(invisible(sim))
  #   } else {
  #     stop("Translator module not loaded but covariates for ", year, " are missing.")
  #   }
  # }
  # if (is.null(sim$stack_list[[as.character(year)]])) {
  #   stop(
  #     "Covariates missing for year ", year,
  #     ". Translator should have been scheduled earlier."
  #   )
  # }
  # 
  # 
  # if (!year %in% names(sim$stack_list)) {
  #   stop("stack_list has no entry for year ", year,
  #        ". Translator module did not generate")
  # }
  # run predictions for this year, need to cache
  # sim <- PredictAllSpecies_blist(
  #   sim         = sim,
  #   year        = as.character(year),   # stack_list stored with string names
  #   climate_year = climate_year,
  #   modelFolder = P(sim)$modelFolder,
  #   nBoot = 2,
  #   outFolder   = "predictions"
  # )
  # 
  ## ADDED 29th March
  if (is.null(sim$stack_list[[as.character(year)]])) {
    if ("bird_covariateTranslator" %in% modules(sim)) {
      message("Missing covariates → scheduling translator for year ", year)
      
      sim <- scheduleEvent(sim, time(sim),
                           "bird_covariateTranslator", "buildCovariates",
                           eventPriority = 1)
      sim <- scheduleEvent(sim, time(sim),
                           "bird_modelPredict", "predict",
                           eventPriority = 7)
      return(invisible(sim))
      
    } else {
      stop("Covariates missing and no translator available.")
    }
  }
  
  
  
  ## Multi BCR prediction
  for (bcr in sim$BCR_codes) {
    bcr_poly <- sim$BCR_parts[sim$BCR_parts$subUnit == bcr]
    sim <- PredictAllSpecies_blist(
      sim         = sim,
      year        = as.character(year),
      climate_year = climate_year,
      modelFolder = P(sim)$modelFolder,
      bcr_code    = bcr,
      bcr_poly    = bcr_poly,
      nBoot = 2,
      outFolder   = "predictions"
    )
  }
  
  # summarizing bootstrap outputs
  #bcr_code <- unique(sim$studyArea$subUnit)
  for (speciesName in names(sim$predictedList)) {
    # sim$summaries[[speciesName]][[as.character(year)]] <- 
    #   SummarizeAndSaveBootstrapStack(sim, speciesName, bcr_code)
    sim$summaries[[speciesName]][[as.character(year)]] <-
      SummarizeAndSaveBootstrapStack(sim, speciesName, year)
  }
  # Always schedule plot for current prediction year
  if (P(sim)$.plots != "none") {
    sim <- scheduleEvent(sim, time(sim), "bird_modelPredict", "plot")
  }
  # schedule next year
  # Bound to LandR simulation years
  if (P(sim)$predictEndYear > end(sim)) {
    warning(sprintf(
      "predictEndYear (%s) exceeds simulation end (%s). Adjusting to match end of simulation year",
      P(sim)$predictEndYear, end(sim)
    ))
    params(sim)$bird_modelPredict$predictEndYear <- end(sim)
  }
  
  ## adjust if theres no covariate
  # if (P(sim)$predictEndYear > end(sim)) {
  #   warning(sprintf(
  #     "predictEndYear (%s) exceeds simulation end (%s). Adjusting.",
  #     P(sim)$predictEndYear, end(sim)
  #   ))
  #   params(sim)$bird_modelPredict$predictEndYear <- end(sim)
  # }
  
  # schedule next year
  # next_year <- year + P(sim)$predictInterval
  # if (next_year <= P(sim)$predictEndYear) {
  #   ## this one schedules prediciton
  #   sim <- scheduleEvent(sim, next_year, "bird_modelPredict", "predict",eventPriority = 5)
  #   ## this one schedules plot for every year, mean and SD
  #   sim <- scheduleEvent(sim, next_year, "bird_modelPredict", "plot")
  # }
  
  ## NEW ADDITION FOR climateYear approach
  if (is.null(sim$climateYear)) {
    next_year <- year + P(sim)$predictInterval
    if (next_year <= P(sim)$predictEndYear) {
      sim <- scheduleEvent(sim, next_year, "bird_modelPredict", "predict", eventPriority = 7)
      sim <- scheduleEvent(sim, next_year, "bird_modelPredict", "plot")
    }
  }
  return(invisible(sim))
}


# PredictAllSpecies_blist <- function(sim, year, nBoot = NULL, climate_year,modelFolder, outFolder ) {
PredictAllSpecies_blist <- function(
    sim,
    year,
    bcr_code,
    bcr_poly,
    nBoot = NULL,
    climate_year,
    modelFolder,
    outFolder
) {
    #bcr_code <- unique(sim$studyArea$subUnit)
  #if (length(bcr_code) != 1) stop("studyArea must have a single BCR subUnit.")
  #browser()
  #modelFiles <- list.files(modelFolder, pattern = "\\.Rdata$", full.names = TRUE) ## also need to add BCR_code, incase more than one BCR are stored in the folder 
  modelFiles <- list.files(modelFolder, pattern = paste0("_can", bcr_code, "\\.Rdata$"), full.names = TRUE)
    dir.create(outFolder, recursive = TRUE, showWarnings = FALSE)
    ## FOr multi BCR
    
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

      if (is.null(ras_stack))
        stop("No raster stack found for year ", year, 
             ". Check stack_list names or translator module output.")
      model_vars <- b.list[[bootNum]][["var.names"]]
      
      ## temp solution for 2 VLCE 1km folder
      names(ras_stack) <- make.unique(names(ras_stack))
      ras_vars <- names(ras_stack)
      missing_vars <- setdiff(model_vars, ras_vars)
      
      if (length(missing_vars) > 0) {
        warning("Missing vars for ", speciesName, " bootstrap ", bootNum, ": ", paste(missing_vars, collapse=", "))
        next
      }
      
      # ras_subset <- terra::subset(ras_stack, model_vars)
      # 
      # pred_raster <- terra::predict(ras_subset, b.list[[bootNum]], type = "response")
      # 
      # boot_rasters[[bootNum]] <- pred_raster
      # predict_bootstrap_raster <- function(ras_stack, model_vars, model) {
      #   ras_subset <- terra::subset(ras_stack, model_vars)
      #   terra::predict(ras_subset, model, type = "response")
      # }
      
      # ## adding year as an input, for proper cache ID
      # predict_bootstrap_raster <- function(ras_stack, model_vars, model, year) {
      #   ras_subset <- terra::subset(ras_stack, model_vars)
      #   terra::predict(ras_subset, model, type = "response")
      # }
      ## adding year and climateYear as an input, for proper caching
      # predict_bootstrap_raster <- function(
      #   ras_stack,
      #   model_vars,
      #   model,
      #   year,
      #   climate_year
      #   ) {
      #   ras_subset <- terra::subset(ras_stack, model_vars)
      #   terra::predict(ras_subset, model, type = "response")
      # }
      predict_bootstrap_raster <- function(
    ras_stack,
    model_vars,
    model,
    bcr_poly,
    year,
    climate_year
      ) {
        ras_subset <- terra::subset(ras_stack, model_vars)
        pred <- terra::predict(ras_subset, model, type = "response")
        pred <- terra::mask(pred, bcr_poly)
        return(pred)
      }
      
      # boot_rasters[[bootNum]] <- Cache(
      #   predict_bootstrap_raster,
      #   ras_stack,
      #   model_vars,
      #   b.list[[bootNum]], year, ## added year input
      #   userTags = c("bootstrap", speciesName, year, paste0("boot_", bootNum))
      # )
      
      #browser()
      boot_rasters[[bootNum]] <- Cache(
        predict_bootstrap_raster,
        ras_stack,
        model_vars,
        b.list[[bootNum]],
        bcr_poly,
        year,
        climate_year,
        userTags = c(
          "bootstrap",
          speciesName,
          paste0("sim_", year),
          paste0("clim_", climate_year),
          paste0("boot_", bootNum),
          paste0("bcr_", bcr_code),
          paste0("species_", speciesName)
        )
      )
      
    }
    
    boot_stack <- rast(boot_rasters)
    names(boot_stack) <- paste0("b", seq_len(nlyr(boot_stack)), "_",year)
    # sim$predictedList[[speciesName]] <- boot_stack
    #sim$predictedList[[speciesName]][[as.character(year)]] <- boot_stack
    sim$predictedList[[speciesName]][[as.character(year)]][[as.character(bcr_code)]] <- boot_stack
    outPath <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code,"_",year, "_prediction.tif"))
    terra::writeRaster(boot_stack, outPath, overwrite = TRUE)
    
    message("Prediction stack saved for ", speciesName)
  }
  
  message("all predictions complete!")
  return(sim)
}


# Summarize Bootstrap 
# SummarizeAndSaveBootstrapStack <- function(sim, speciesName, bcr_code, outFolder = "predictions") {
#   # boot_stack <- sim$predictedList[[speciesName]]
#   boot_stack <- sim$predictedList[[speciesName]][[as.character(year)]]
#   mean_r <- terra::app(boot_stack, mean, na.rm = TRUE)
#   sd_r <- terra::app(boot_stack, sd, na.rm = TRUE)
#   mean_path <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code, "_mean.tif"))
#   sd_path <- file.path(outFolder, paste0(speciesName, "_BCR_", bcr_code, "_sd.tif"))
#   terra::writeRaster(mean_r, mean_path, overwrite = TRUE)
#   terra::writeRaster(sd_r, sd_path, overwrite = TRUE)
#   return(list(mean = mean_r, sd = sd_r))
# }
SummarizeAndSaveBootstrapStack <- function(sim, speciesName, year, outFolder = "predictions") {
  
  # get the single-year SpatRaster bootstrap stack
  #boot_stack <- sim$predictedList[[speciesName]][[as.character(year)]]
  speciesStacks <- sim$predictedList[[speciesName]][[as.character(year)]]
  # boot_stack <- Reduce(terra::cover, speciesStacks)
  nboot <- nlyr(speciesStacks[[1]])
  merged_layers <- lapply(seq_len(nboot), function(i) {
    rasters_i <- lapply(speciesStacks, function(x) x[[i]])
    Reduce(terra::cover, rasters_i)
  })
  #browser()
  boot_stack <- terra::rast(merged_layers)
  if (is.null(boot_stack))
    stop("No bootstrap stack found for species ", speciesName, " and year ", year)
  
  # summary rasters
  mean_r <- terra::app(boot_stack, mean, na.rm = TRUE)
  sd_r   <- terra::app(boot_stack, sd,   na.rm = TRUE)
  
  # save with year suffix
  mean_path <- file.path(outFolder, paste0(speciesName, "_",  year, "_mean.tif"))
  sd_path   <- file.path(outFolder, paste0(speciesName, "_",  year, "_sd.tif"))
  
  terra::writeRaster(mean_r, mean_path, overwrite = TRUE)
  terra::writeRaster(sd_r, sd_path, overwrite = TRUE)
  
  return(list(mean = mean_r, sd = sd_r))
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
# Save <- function(sim) {
#   # ! ----- EDIT BELOW ----- ! #
#   # do stuff for this event
#   sim <- saveFiles(sim)
#   
#   # ! ----- STOP EDITING ----- ! #
#   return(invisible(sim))
# }

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
  currentYear <- time(sim)
  
  for (speciesName in names(sim$summaries)) {
    
    # extract summaries for THIS year only
    yearSummary <- sim$summaries[[speciesName]][[as.character(currentYear)]]
    
    if (is.null(yearSummary)) {
      warning("No summary found for species ", speciesName, 
              " for year ", currentYear)
      next
    }
    
    # plot mean
    Plots(
      yearSummary$mean,
      fn       = gg_predMap,
      types    = P(sim)$.plots,
      filename = paste0(speciesName, "_mean_yr_", currentYear),
      title    = paste(speciesName, "Mean", currentYear),
      path     = file.path(figurePath(sim))
    )
    
    # plot sd
    Plots(
      yearSummary$sd,
      fn       = gg_predMap,
      types    = P(sim)$.plots,
      filename = paste0(speciesName, "_sd_yr_", currentYear),
      title    = paste(speciesName, "SD", currentYear),
      path     = file.path(figurePath(sim))
    )
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
  # if (!suppliedElsewhere("stack_list", sim)) {
  #   stop("stack_list must be provided by an upstream module like bird_covariateTranslator")
  # }
  # 
  # if (!suppliedElsewhere("studyArea", sim)) {
  #   stop("studyArea must be provided by the setupProject or upstream module")
  # }
  # 
  # if (!suppliedElsewhere("modelFolder", sim)) {
  #   stop("modelFolder path must be provided in params")
  # }
  # 
  # if (!suppliedElsewhere("climateYear", sim)) {
  #   sim$climateYear <- NULL  # allow fallback to time(sim)
  # }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
# 
# ggplotFn <- function(data, ...) {
#   ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
#     ggplot2::geom_histogram(...)
# }
