#' @export
set_soil <- function(data, id=NULL, depth=NULL, saturated_wtr_cont=NULL, organic_N_conc=NULL,
                     layer_depth = NULL, layer_water_field_cap=NULL, layer_water_wilting_pt=NULL,
                     layer_bulk_density_moist=NULL, layer_saturated_wtr_cont=NULL, verbose=FALSE,
                     data_format = NULL){

  # get dictionnary from function argument values
  dict <- get_argValues()
  dict[c("data", "verbose", "data_format")] <- NULL
  # seperate dictionary from units
  dict_hasUnit <- sapply(dict, is.list)
  unitsParam <- sapply(dict[dict_hasUnit], `[[`, 2)
  dict[dict_hasUnit] <- sapply(dict[dict_hasUnit], `[[`, 1)

  # determine data format
  is_tibbleWithList <- FALSE
  is_dataFrameLarge <- FALSE
  is_dataFrameLong <- FALSE
  if(is.null(data_format)){
    # automatic data format selection
    is_tibbleWithList <- detect_tibbleWithList(data, dict)
    is_dataFrameLarge <- detect_dataFrameLarge(data, dict) & !is_tibbleWithList
    is_dataFrameLong <- detect_dataFrameLong(data, dict)
    if(!is_tibbleWithList & !is_dataFrameLarge & !is_dataFrameLong){
      stop("Automatic data format detection failed. See documentation of the `data_format` argument to select the data format manually.")
    }
  } else{
    # manual data format selection
    format <- as.list(data_format)[[1]]
    format <- match.arg(format, c("tibble", "large", "long"))
    is_tibbleOfLists <- format == "tibble"
    is_dataFrameLarge <- format == "large"
    is_dataFrameLong <- format == "long"
  }

  # case: DSSAT-style tibble
  if(is_tibbleWithList){
    if(verbose)
      cli::cli_alert_info("Soil data format detected: Tibble containing lists for soil layer information")

    # check for name typos
    is_present <- dict %in% names(data)
    if(sum(!is_present) > 0){
      stop("Could not find parameter names: `",
           paste(dict[!is_present], collapse = "`, `"),
           "`."
      )
    }

    # seperate and unnest layer parameters
    # find layer parameters
    is_layerParameter <- sapply(data, is.list)

    # seperate layer parameters
    data_byLayer <- data[is_layerParameter | names(data) == dict["id"]]
    data <- data[!is_layerParameter]

    # find and check number of layers for each observation
    ind_id <- which(names(data_byLayer) == dict["id"])
    nb_layers <- sapply(data_byLayer[-ind_id], sapply, length)
    equal_length <- apply(nb_layers, 1, function(x) all(x==x[[1]]))
    if(any(!equal_length)){
      stop("For every observation, the soil parameters must possess the same number of layers: the observation/s `",
           paste(data_byLayer[!equal_length, ind_id], collapse = "`, `"),
           "` has/have a varying number of layers."
      )
    }

    # unnest layer parameters
    data_byLayer <- tidyr::unnest(data_byLayer, names(data_byLayer))
    # add layer number
    data_byLayer$layer <- as.vector(sapply(nb_layers[,1], function(x) 1:x))

    # update names using dictionary
    names(data) <- get_dict(names(data), dict)
    names(data_byLayer) <- get_dict(names(data_byLayer), dict)
  }

  # case: large data frame
  if(is_dataFrameLarge){
    if(verbose){
      cli::cli_alert_info("Soil data format detected: large data frame")
    }

    dict_isDataName <- lapply(dict, is_prefixOf, names(data))
    dict_isAnyDataName <- sapply(dict_isDataName, any)

    # check for name typos
    if(sum(dict_isAnyDataName) < length(dict)){
      stop(
        paste0("Could not find parameter names: `",
               paste(dict[!dict_isAnyDataName], collapse = "`, `"),
               "`."
        )
      )
    }

    # find and sort names corresponding to every dictionary entry
    names_byLayer <- is_prefixOf("layer_", names(dict))
    names_byLayer <- lapply(dict_isDataName[names_byLayer], function(x, dataNames) dataNames[x], names(data))
    names_byLayer <- lapply(names_byLayer, stringr::str_sort)

    # seperate layer parameters
    data_byLayer <- data %>% dplyr::select(dict[["id"]] | !!unlist(names_byLayer, use.names=FALSE))
    data <- data %>% dplyr::select(!unlist(names_byLayer, use.names=FALSE))

    # transform layer data to have on column per parameter
    spec <- data.frame(
      .name = unlist(names_byLayer),
      .value = sapply(unlist(names_byLayer), find_nameInList, names_byLayer),
      layer = sapply(unlist(names_byLayer), find_indexInList, names_byLayer)
    )
    data_byLayer <- tidyr::pivot_longer_spec(data_byLayer, spec)

    # update names according to dictionnary
    names(data) <- get_dict(names(data), dict)
    names(data_byLayer) <- get_dict(names(data_byLayer), dict)
  }

  # case: long data frame
  # find param column indice in long data frame

  if(is_dataFrameLong){
    indList <- lapply(1:4, function(x) character(0))
    names(indList) <- list("id", "layer", "param", "value")
    # find column indices for id, layer and value
    # if specified manually
    if(length(data_format) == 2){
      # add specified indices to indices
      indList <- combine.lists(as.list(data_format)[[2]], indList)

      # check validity of column specification : has to be either integer or character
      ind_isInteger <- sapply(indList, class) == "numeric"
      ind_isInteger[ind_isInteger] <- sapply(indList[ind_isInteger], function(x) x %% 1 == 0)
      indList[ind_isInteger] <- sapply(indList[ind_isInteger], as.integer)
      is_formatCorrect <- sapply(indList, class) %in% c("character", "integer")
      if(any(!is_formatCorrect)){
        stop("Column indices/names given in the argument `data_format` have to be integers or characters.")
      }

      # extract indices, if column given by name then convert to index
      indList <- lapply(indList, function(x) {
        if(is.integer(x))
          return(x)
        else
          return(which(names(data) == x))
      })
      # for the id column, take the usual `id` parameter if not further specified
      if(rlang::is_empty(indList$id))
        indList$id <- which(names(data) == dict[["id"]])
      # indList$id <- if(is.integer(indices$id)) indices$id
      #   else if(!is.character(indices$id)) which(names(data) == dict[["id"]])
      #   else which(names(data) == indices$id)
      # indList$param <- if(is.integer(indices$param)) indices$param else which(names(data) == indices$param)
      # indList$layer <- if(is.integer(indices$layer)) indices$layer else which(names(data) == indices$layer)
      # indList$value <- if(is.integer(indices$value)) indices$value else which(names(data) == indices$value)

      # check if any one of them is missing
      ind_isInvalid <- sapply(indList, rlang::is_empty)
      if(any(ind_isInvalid)){
        stop("Column index for `", paste(names(indList)[ind_isInvalid], collapse="`, `"), "` is missing.")
      }
    }
    # if not specified manually, detect columns automatically
    else{
      # detect columns automatically
      indList$id <- which(names(data) == id)
      indList$layer <- which(sapply(data, is.integer))
      indList$param <- get_indParam(data, dict)
      indList$value <- which(sapply(data, is.numeric) & !sapply(data, is.integer))

      # throw warning if any of the detections failed
      ind_detectionFailed <- sapply(indList, length) == 0
      if(sum(ind_detectionFailed) > 0){
        stop("No candidate for column(s) `", paste(names(indList)[ind_detectionFailed], collapse="`, `"), "` was found. Use argument `data_format` to select the data columns manually.")
      }

      # throw warning if any of the detections is ambiguous
      ind_isAmbiguous <- sapply(indList, length) > 1
      indList[ind_isAmbiguous] <- lapply(indList[ind_isAmbiguous], `[[`, 1)

      if(sum(ind_isAmbiguous) > 0){
        warning(paste0("Automatic column detection for long data format was ambiguous. Selected `",
                       paste(
                         mapply(function(nameSelected, nameCol) paste0(nameSelected, "` for `", nameCol),
                                names(data)[unlist(indList[ind_isAmbiguous])], names(indList)[ind_isAmbiguous]),
                         collapse = "`, `"),
                       "`. Specify columns using the argument `data_format` to suppress this warning."
        ))
      }
    }

    if(verbose){
      cli::cli_alert_info("Soil data format detected: long data frame")
      cli::cli_alert_info(paste0("Long data frame column detected for soil identification: `", names(data)[[indList$id]], "`"))
      cli::cli_alert_info(paste0("Long data frame column detected for soil parameter names: `", names(data)[[indList$param]], "`"))
      cli::cli_alert_info(paste0("Long data frame column detected for soil layers: `", names(data)[[indList$layer]], "`"))
      cli::cli_alert_info(paste0("Long data frame column detected for parameter values: `", names(data)[[indList$value]], "`"))
    }

    # check for missing params
    # get indices of params that should not be checked
    indNoCheck <- which(names(dict) %in% c("id"))

    # check for missing param names
    indMissingParams <- which(!(dict %in% data[,indList$param]))
    indMissingParams <- dplyr::setdiff(indMissingParams, indNoCheck)
    if(length(indMissingParams) > 0)
      stop(paste0("Could not find parameter names: `",
                  paste(dict[indMissingParams], collapse= '`, `'),
                  "`.")
      )

    # add layer to dictionary
    dict <- c(dict, list(layer = as.symbol(names(data)[[indList$layer]])))

    # extract relevent columns
    data <- data[unlist(indList)]
    # update indices
    indList <- setNames(as.list(seq_along(indList)), names(indList))
    # seperate layer and non-layer parameters
    data_byLayer <- data[!is.na(data[indList$layer]),]
    data <- data[is.na(data[indList$layer]),]

    # transform data in wider data frame format
    data <- data %>%
      dplyr::select(-indList$layer) %>%
      tidyr::pivot_wider(names_from = names(data)[[indList$param]], values_from = names(data)[[indList$value]])
    data_byLayer <- tidyr::pivot_wider(data_byLayer, names_from = indList$param, values_from = indList$value)

    # update data names according to dictionary
    names(data_byLayer) <- get_dict(names(data_byLayer), dict)
    names(data) <- get_dict(names(data), dict)
  }

  # apply units to respective columns
  data <- data_setUnits(data, unitsParam)
  data_byLayer <- data_setUnits(data_byLayer, unitsParam)

  # build soil data object
  soil <- list(data = data, data_byLayer = data_byLayer, dict = dict) %>%
    structure(class = "cropr_input")

  return(invisible(soil))
  # ToDo: verify coherence of input data (same number of observations, ...)
  # ToDo: check that ... contains only named arguments
}
# copy charactersitcs automatically from function agruments
# glob.chars$soil <- data_soil %>% formals() %>% names() %>% utils::tail(-1)

find_nameInList <- function(value, list){
  found <- sapply(list, function(x) value %in% x)
  if(any(found))
    return(names(list)[found])
  return(NA)
}

find_indexInList <- function(value, list){
  found <- sapply(list, function(x) value %in% x)
  if(any(found) && length(list[[which(found)]]) > 1)
    return(as.character(match(value, list[[which(found)]])))
  return(NA)
}

fill_layerColumn <- function(data, dict, dict_isPresentInDataNames){
  for(param in dict){
    row <- dict_isPresentInDataNames[param]
    data[,row] <- data[,row][order(names(data[row]))]
  }
  data[row] <- data[row][order(names(data[row]))]
}

is_prefixOf <- function(x, vec_names){
  return(
    startsWith(
      vec_names,
      as.character(x)
    )
  )
}

get_indParam <- function(data, dict){
  # reveal supplied parameter names in data
  data_isParamName <- sapply(data, `%in%`, dict)
  # get column that contains the most parameter names
  indParam <- which.max(colSums(data_isParamName))
  # reveal supplied param names that are in the found data column
  dict_WithoutId <- if(!"id" %in% names(dict)) dict else dict[-which(names(dict)=="id")]
  dict_isInParamCol <- sapply(as.character(dict_WithoutId), `%in%`, data[[indParam]])
  # accept if at least half of supplied parameters appear in this column
  if(sum(dict_isInParamCol) >= length(dict_WithoutId)/2)
    return(indParam)
  return(NULL)
}

detect_dataFrameLarge <- function(data, dict){
  nb_present <- sapply(dict, is_prefixOf, names(data)) %>%
    apply(2, any) %>%
    sum()
  return(nb_present >= length(dict) / 2)
}

detect_tibbleWithList <- function(data, dict){
  return(detect_dataFrameLarge(data, dict) & "list" %in% sapply(data, class))
}

detect_dataFrameLong <- function(data, dict){
  return(!is.null(get_indParam(data, dict)))
}


