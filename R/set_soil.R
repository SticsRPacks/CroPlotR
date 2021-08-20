#' Collect data on soil variables for further use in plot-functions
#'
#' Transforms user-provided soil data into an object that can be read by other CroPlotR
#' functions, most notably by the plot functions. The data on all variables has to be given through
#' a single data frame. The names of every variable and its unit have to be specified.
#'
#' @param data A data.frame containing soil data
#' @param id Soil identification
#' @param thickness Soil thickness
#' @param soil_max_wtr_cap Soil maximal water capacity
#' @param organic_N_conc Soil organic nitrogen content
#' @param layer_thickness Soil thickness by layer
#' @param layer_water_field_cap Soil water field capacity by layer
#' @param layer_water_wilting_pt Soil water wilting point by layer
#' @param layer_bulk_density Soil bulk density of fine earth fraction by layer
#' @param layer_max_wtr_cap Soil maximal water capacity by layer
#' @param data_format Specify format of data argument, see `Details` for usage
#' @param verbose Provide extra information about the function's inner procedures?
#' @return A list of class `cropr_input` containing all necessary information for plotting.
#' @details
#' The column name in `data` and the unit of every variable have to be given as a list of two characters.
#'
#' Possible values of the `data_format` argument are "tibble", "wide" and "long", see vignette (link?) for details.
#' If no data format is specified, an automatic detection is attempted. In the case of the *long* data frame format, the
#' `data_format` argument can be a list where the first element determines the format and the second element is a named list.
#' The elements in this named list must be called 'id', 'variable', 'layer' and 'value' and they must be either integers or characters.
#' They specify by index or by names in which column of the long data frame the identifications, variables, layers and values
#' are to be found. If the element 'id' is not specified, the regular 'id' argument of `set_soil` is used.
#' @export
#' @examples
#' # load example data in different formats
#' workspace <- system.file(file.path("extdata", "example_input"), package = "CroPlotR")
#' soil_data_wide <- readRDS(file.path(workspace, "soil_data_wide.rds"))
#' soil_data_long <- readRDS(file.path(workspace, "soil_data_long.rds"))
#' soil_data_tibble <- readRDS(file.path(workspace, "soil_data_tibble.rds"))
#'
#' # usage with automatic format detection
#' soil <- set_soil(
#'   soil_data_wide,
#'   id = "name",
#'   organic_N_conc = list("norg", "g/g"),
#'   layer_thickness = list("epc", "cm")
#' )
#'
#' # usage with specific format
#' soil <- set_soil(
#'   soil_data_wide,
#'   id = "name",
#'   organic_N_conc = list("norg", "g/g"),
#'   layer_thickness = list("epc", "cm"),
#'   data_format = "wide"
#' )
#'
#' # usage in long format while specifying columns
#' soil <- set_soil(
#'   soil_data_long,
#'   id = "name",
#'   organic_N_conc = list("norg", "g/g"),
#'   layer_thickness = list("epc", "cm"),
#'   data_format = list("long", list(id = "name", variable = "variable", layer = 3, value = "value"))
#' )
#'
set_soil <- function(data, id, thickness=NULL, soil_max_wtr_cap=NULL, organic_N_conc=NULL,
                     layer_thickness = NULL, layer_water_field_cap=NULL, layer_water_wilting_pt=NULL,
                     layer_bulk_density=NULL, layer_max_wtr_cap=NULL, data_format = NULL,
                     verbose=FALSE){

  # get dictionnary from function argument values
  dict <- get_argValues()
  dict[c("data", "verbose", "data_format")] <- NULL
  # seperate dictionary from units
  dict_hasUnit <- sapply(dict, is.list)
  unitsParam <- sapply(dict[dict_hasUnit], `[[`, 2)
  dict[dict_hasUnit] <- sapply(dict[dict_hasUnit], `[[`, 1)

  # determine data format
  is_tibbleWithList <- FALSE
  is_dataFrameWide <- FALSE
  is_dataFrameLong <- FALSE
  if(is.null(data_format)){
    # automatic data format selection
    is_tibbleWithList <- detect_tibbleWithList(data, dict)
    is_dataFrameWide <- detect_dataFrameWide(data, dict) & !is_tibbleWithList
    is_dataFrameLong <- detect_dataFrameLong(data, dict)
    if(!is_tibbleWithList & !is_dataFrameWide & !is_dataFrameLong){
      stop("Automatic data format detection failed. See documentation of the `data_format` argument to select the data format manually.")
    }
  } else{
    # manual data format selection
    format <- as.list(data_format)[[1]]
    format <- match.arg(format, c("tibble", "wide", "long"))
    is_tibbleWithList <- format == "tibble"
    is_dataFrameWide <- format == "wide"
    is_dataFrameLong <- format == "long"
  }

  # case: DSSAT-style tibble
  if(is_tibbleWithList){
    if(verbose)
      cli::cli_alert_info("Soil data format detected: Tibble containing lists for soil layer information")

    # check for name typos
    is_present <- dict %in% names(data)
    if(sum(!is_present) > 0){
      stop("Could not find column names: `",
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

    if(sum(is_layerParameter) > 0){
      # find and check number of layers for each observation
      ind_id <- which(names(data_byLayer) == dict["id"])
      nb_layers <- sapply(data_byLayer[-ind_id], sapply, length)
      equal_length <- apply(nb_layers, 1, function(x) all(x==x[[1]]))
      if(any(!equal_length)){
        stop("For every observation, the soil variables must possess the same number of layers: the observation/s `",
             paste(data_byLayer[!equal_length, ind_id], collapse = "`, `"),
             "` has/have a varying number of layers."
        )
      }

      # unnest layer parameters
      data_byLayer <- tidyr::unnest(data_byLayer, names(data_byLayer))

      # add layer number
      data_byLayer$layer <- unlist(lapply(nb_layers[,1], function(x) 1:x))
    }

    # update names using dictionary
    names(data) <- get_dict(names(data), dict)
    names(data_byLayer) <- get_dict(names(data_byLayer), dict)
  }

  # case: wide data frame
  if(is_dataFrameWide){
    if(verbose){
      cli::cli_alert_info("Soil data format detected: wide data frame")
    }

    dict_isDataName <- lapply(dict, is_prefixOf, names(data))
    dict_isAnyDataName <- sapply(dict_isDataName, any)

    # check for name typos
    if(sum(dict_isAnyDataName) < length(dict)){
      stop(
        paste0("Could not find column names: `",
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
    names(indList) <- list("id", "layer", "variable", "value")
    # find column indexes for id, layer and value
    # if specified manually
    if(length(data_format) == 2){
      tryCatch({
        names(data_format[[2]]) <- sapply(names(data_format[[2]]), match.arg, c("id", "layer", "variable", "value"))
        },
        error = function(cond){
          stop("Long data frame format column names should be one of 'id', 'layer', 'variable', 'value'.")
        })
      # add specified indexes to indexes
      indList <- combine.lists(as.list(data_format)[[2]], indList)

      # check validity of column specification : has to be either integer or character
      ind_isInteger <- sapply(indList, class) == "numeric"
      ind_isInteger[ind_isInteger] <- sapply(indList[ind_isInteger], function(x) x %% 1 == 0)
      indList[ind_isInteger] <- sapply(indList[ind_isInteger], as.integer)
      is_formatCorrect <- sapply(indList, class) %in% c("character", "integer")
      if(any(!is_formatCorrect)){
        stop("Column indices/names given in the argument `data_format` have to be integers or characters.")
      }

      # extract indixes, if column given by name then convert to index
      indList <- lapply(indList, function(x) {
        if(is.integer(x))
          return(x)
        else
          return(which(names(data) == x))
      })
      # for the id column, take the usual `id` parameter if not further specified
      if(rlang::is_empty(indList$id))
        indList$id <- which(names(data) == dict[["id"]])

      # check if any one of them is missing
      ind_isInvalid <- sapply(indList, rlang::is_empty)
      if(any(ind_isInvalid)){
        stop("Long data frame column(s) not specified or specification(s) could not be found: `", paste(names(indList)[ind_isInvalid], collapse="`, `"), "`.")
      }
    }
    # if not specified manually, detect columns automatically
    else{
      # detect columns automatically
      indList$id <- which(names(data) == id)
      indList$layer <- which(sapply(data, is.integer))
      indList$variable <- get_indVar(data, dict)
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
        warning(paste0("Automatic column detection for long data format was ambiguous. Selected the column(s) `",
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
      cli::cli_alert_info(paste0("Long data frame column detected for soil variable names: `", names(data)[[indList$variable]], "`"))
      cli::cli_alert_info(paste0("Long data frame column detected for soil layers: `", names(data)[[indList$layer]], "`"))
      cli::cli_alert_info(paste0("Long data frame column detected for values: `", names(data)[[indList$value]], "`"))
    }

    # check for missing params
    # get indices of params that should not be checked
    indNoCheck <- which(names(dict) %in% c("id"))

    # check for missing param names
    indMissingParams <- which(!(dict %in% data[,indList$variable]))
    indMissingParams <- dplyr::setdiff(indMissingParams, indNoCheck)
    if(length(indMissingParams) > 0)
      stop(paste0("Could not find variable names: `",
                  paste(dict[indMissingParams], collapse= '`, `'),
                  "`.")
      )

    # add layer to dictionary
    dict <- c(dict, list(layer = as.symbol(names(data)[[indList$layer]])))

    # extract relevent columns
    data <- data[unlist(indList)]
    # update indices
    indList <- stats::setNames(as.list(seq_along(indList)), names(indList))
    # seperate layer and non-layer parameters
    data_byLayer <- data[!is.na(data[indList$layer]),]
    data <- data[is.na(data[indList$layer]),]

    # transform data in wider data frame format
    data <- data %>%
      dplyr::select(-indList$layer) %>%
      tidyr::pivot_wider(names_from = names(data)[[indList$variable]], values_from = names(data)[[indList$value]])
    data_byLayer <- tidyr::pivot_wider(data_byLayer, names_from = indList$variable, values_from = indList$value)

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

#' Get the names of all elements of a list that contain a certain value
#'
#' @param value A value
#' @param list A list of lists
#' @return The names all elements of `list` that contain value
find_nameInList <- function(value, list){
  found <- sapply(list, function(x) value %in% x)
  if(any(found))
    return(names(list)[found])
  return(NA)
}

#' Get the indexes of all elements of a list that contain a certain value
#'
#' @param value A value
#' @param list A list of lists
#' @return The indexes all elements of `list` that contain value
find_indexInList <- function(value, list){
  found <- sapply(list, function(x) value %in% x)
  if(any(found) && length(list[[which(found)]]) > 1)
    return(as.character(match(value, list[[which(found)]])))
  return(NA)
}

# fill_layerColumn <- function(data, dict, dict_isPresentInDataNames){
#   for(param in dict){
#     row <- dict_isPresentInDataNames[param]
#     data[,row] <- data[,row][order(names(data[row]))]
#   }
#   data[row] <- data[row][order(names(data[row]))]
# }

#' Check if one character is a prefix of another
#'
#' @param x character
#' @param vec_names A vector of characters
#' @return A logic vector indicating whether `x` is a prefix of every element in `vec_names`
is_prefixOf <- function(x, vec_names){
  return(
    startsWith(
      vec_names,
      as.character(x)
    )
  )
}

#' Get the column of a data frame that contains at least half of the names of a list.
#'
#' @param data A data frame
#' @param dict A named list
#' @return The index of the column in `data` that contains most of the names of `dict`. Returns `character(0)` if
#' to column contains more than half of the names.
get_indVar <- function(data, dict){
  # reveal supplied parameter names in data
  data_isParamName <- sapply(data, `%in%`, dict)
  # get column that contains the most parameter names
  indParam <- which.max(colSums(data_isParamName))[[1]]
  # reveal supplied param names that are in the found data column
  dict_WithoutId <- if(!"id" %in% names(dict)) dict else dict[-which(names(dict)=="id")]
  dict_isInParamCol <- sapply(as.character(dict_WithoutId), `%in%`, data[[indParam]])
  # accept if at least half of supplied parameters appear in this column
  if(sum(dict_isInParamCol) >= length(dict_WithoutId)/2)
    return(indParam)
  return(character(0))
}

#' Detect whether a data frame is in wide format
#'
#' @param data A data frame
#' @param dict A named list
#' @return A logic value indicating whether at least half of the names of `list` are prefixes of column names of `data`.
detect_dataFrameWide <- function(data, dict){
  nb_present <- sapply(dict, is_prefixOf, names(data)) %>%
    apply(2, any) %>%
    sum()
  return(nb_present >= length(dict) / 2)
}

#' Detect whether a data frame is in tibble format
#'
#' @param data A data frame
#' @param dict A named list
#' @return A logic value indicating whether the `data` is in wide format and also contains at least on element of type `list`
detect_tibbleWithList <- function(data, dict){
  return(detect_dataFrameWide(data, dict) & "list" %in% sapply(data, class))
}

#' Detect whether a data frame is in long format
#'
#' @param data A data frame
#' @param dict A named list
#' @return A logic value indicating whether one column of `data` contains at least half the names of `dict`
detect_dataFrameLong <- function(data, dict){
  return(!rlang::is_empty(get_indVar(data, dict)))
}


