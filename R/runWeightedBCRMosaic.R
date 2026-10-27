runWeightedBCRMosaic <- function(
    sim,
    species,
    year,
    weightFolderURL,
    destinationPath = tempdir()
){
  
  require(terra)
  require(googledrive)
  require(reproducible)
  
  bcr_codes <- sim$BCR_codes
  #message("Detected BCRs: ", paste(bcr_codes, collapse=", "))
  message(bold(
    "Creating weighted BCR mosaic | species = ", species,
    " | year = ", year,
    " | BCRs = ", paste(bcr_codes, collapse = ", ")
  ))
  #-----------------------------
  # 1. DOWNLOAD WEIGHT RASTERS
  #-----------------------------
  folder_id <- googledrive::as_id(weightFolderURL)
  files <- googledrive::drive_ls(folder_id)
  
  weight_files <- files[
    grepl(paste0("(", paste(bcr_codes, collapse="|"), ")"), files$name),
  ]
  
  weights <- lapply(seq_len(nrow(weight_files)), function(i){
    
    local_path <- Cache(
      prepInputs,
      .functionName = "prep_BCR_weights",
      url = weight_files$id[i],
      destinationPath = destinationPath,
      targetFile = weight_files$name[i],
      fun = NA,
      userTags = c("BCRweight", weight_files$name[i])
    )
    
    rast(local_path)
    
  })
  names(weights) <- weight_files$name
  # names(weights) <- gsub("[^0-9]", "", weight_files$name)
  #--------------------------------
  # CLEAN + MATCH WEIGHTS (CAN only)
  #--------------------------------
  
  # Parse metadata
  meta <- data.frame(
    file = weight_files$name,
    country = substr(weight_files$name, 1, 3),
    bcr = sub(".*?(\\d+).*", "\\1", weight_files$name),
    stringsAsFactors = FALSE
  )
  
  # Keep only CAN weights
  meta_can <- meta[meta$country == "can", ]
  
  # Convert BCR codes to character
  bcr_codes <- as.character(bcr_codes)
  
  # Keep only needed BCRs
  meta_can <- meta_can[meta_can$bcr %in% bcr_codes, ]
  
  # Build clean weights list
  weights_clean <- lapply(seq_len(nrow(meta_can)), function(i){
    weights[[meta_can$file[i]]]
  })
  
  names(weights_clean) <- meta_can$bcr
  weights <- weights_clean
  
  # Sanity check (critical)
  if (!setequal(names(weights), bcr_codes)) {
    stop("Mismatch between weights and BCR codes")
  }
  
  #browser()
  #--------------------------------
  # 2. GET PREDICTION STACKS
  #--------------------------------
  pred_list <- lapply(bcr_codes, function(bcr){
    
    sim$predictedList[[species]][[as.character(year)]][[as.character(bcr)]]
    
  })
  
  names(pred_list) <- bcr_codes
  
  #pred_template <- pred_list[[1]][[1]]
  pred_list <- Filter(Negate(is.null), pred_list)
  
  if (length(pred_list) == 0) {
    stop("No BCR prediction rasters found for species ", species, " year ", year)
  }
  
  pred_template <- pred_list[[1]][[1]]
  
  #--------------------------------
  # 3. ALIGN WEIGHTS
  #--------------------------------
  # weights <- lapply(weights, function(w){
  # 
  #   if(!compareGeom(w, pred_template, stopOnError=FALSE)){
  #     # w <- crop(w, pred_template)
  #     w <- terra::resample(w, pred_template, method="bilinear")
  #   }
  # 
  #   w
  # })
  
  weights <- lapply(weights, function(w){
    
    # STEP 1: reproject FIRST (critical)
    if (!compareGeom(w, pred_template, stopOnError=FALSE, crs=TRUE)) {
      w <- terra::project(w, pred_template)
    }
    
    # STEP 2: now check overlap
    if (!relate(ext(w), ext(pred_template), "intersects")) {
      return(NULL)
    }
    
    # STEP 3: align grid
    if(!compareGeom(w, pred_template, stopOnError=FALSE)){
      w <- crop(w, pred_template)
      w <- terra::resample(w, pred_template, method="bilinear")
    }
    
    w
  })
  #--------------------------------
  # 4. MOSAIC BOOTSTRAPS
  #--------------------------------
  nboot <- nlyr(pred_list[[1]])
  boot_results <- vector("list", nboot)
  
  for(i in seq_len(nboot)){
    
    preds <- lapply(names(pred_list), function(bcr){
      
      pred_list[[bcr]][[i]] * weights[[bcr]]
      
    })
    
    ws <- lapply(names(pred_list), function(bcr){
      
      weights[[bcr]]
      
    })
    
    pred_sum <- mosaic(sprc(preds), fun="sum")
    weight_sum <- mosaic(sprc(ws), fun="sum")
    
    out <- pred_sum / weight_sum
    out[weight_sum==0] <- NA
    
    boot_results[[i]] <- out
  }
  
  final <- rast(boot_results)
  names(final) <- names(pred_list[[1]])
  
  message(bold(cyan(
    "Finished weighted BCR mosaic | species = ", species,
    " | year = ", year,
    " | layers = ", terra::nlyr(final)
  )))
  return(final)
}