#'
#' @importFrom dplyr across arrange as_tibble bind_rows everything filter
#'             mutate rename select
#' @importFrom rlang .data
#' @importFrom stringr str_remove
#' @importFrom tidyr separate_wider_delim unite
#' @importFrom foundr bestcor eigen_contrast eigen_traits
#' @importFrom utils combn
#' @importFrom shiny hr

#' @return HTML
#' @rdname founder_helpers
border_line <- function() {
  shiny::hr(style="border-width:5px;color:black;background-color:black")
}
#' Turn `conditionContrasts` object into a `traitSignal` object.
#' @param contrasts data frame
#' @return data frame
#' @rdname founder_helpers
contrast_signal <- function(contrasts) {
  if(is.null(contrasts))
  return(NULL)

  dplyr::mutate(
    dplyr::select(
      dplyr::rename(
        contrasts,
        cellmean = "value"),
      -p.value),
    signal = .data$cellmean)
}
#' Correlation Table
#' 
#' @param key_trait 
#' @param traitSignal 
#' @param corterm 
#' @param mincor 
#' @param reldataset 
#' @return data frame
#' @rdname founder_helpers
cor_table <- function(key_trait, traitSignal, corterm, mincor = 0,
                      reldataset = NULL) {
  
  if(is.null(key_trait) || is.null(traitSignal))
    return(NULL)
  
  if(is.null(reldataset))
    return(NULL)
  #    return(dplyr::distinct(object, .data$dataset, .data$trait))
  
  # Select rows of traitSignal() with Key Trait or Related Datasets.
  object <- select_data_pairs(traitSignal, key_trait, reldataset)
  
  # Filter by mincor
  dplyr::filter(
    foundr::bestcor(object, key_trait, corterm),
    .data$absmax >= mincor)
}
#' Eigen Contrasts from Dataset
#' 
#' @param object 
#' @param contr_object 
#' @return data frame
#' @rdname founder_helpers
eigen_contrast_dataset <- function(object, contr_object) {
  if(is.null(object) | is.null(contr_object))
    return(NULL)
  
  if(!is_sex_module(object))
    return(eigen_contrast_dataset_value(object, contr_object))
  
  eigen_contrast_dataset_sex(object, contr_object)
}
eigen_contrast_dataset_sex <- function(object, contr_object) {
  datasets <- names(object)
  if(!all(datasets %in% contr_object$dataset))
    return(NULL)
  # Split contrast object by dataset
  contr_object <- split(contr_object, contr_object$dataset)[datasets]
  # For each `dataset`, construct contrast of eigens.
  # The `contr_object` is only used for module information.
  out <- purrr::map(datasets, function(x) {
    foundr::eigen_contrast(object[[x]], contr_object[[x]])
  })
  class(out) <- c("conditionContrasts", class(out))
  c(out)
}
eigen_contrast_dataset_value <- function(object, contr_object) {
  # Can only handle one trait module right now.
  if(length(object) > 1)
    return(NULL)
  
  # Should be one module with element `value`.
  object <- object[[1]]
  datasets <- names(object)
  if(!all(datasets %in% "value"))
    return(NULL)
  
  # Get information for each module.
  # Could add information from `contr_object` later.
  objectInfo <-
    dplyr::ungroup(
      dplyr::summarize(
        dplyr::group_by(
          object$value$modules,
          .data$module),
        kME = signif(max(abs(kME), na.rm = TRUE), 4),
        #       p.value = signif(min(p.value, na.rm = TRUE), 4),
        size = dplyr::n(),
        drop = signif(sum(dropped) / size, 4),
        .groups = "drop"))
  
  # Join contrast object of eigenvalues with module information
  dplyr::mutate(
    dplyr::left_join(
      dplyr::rename(contr_object, module = "trait"),
      objectInfo,
      by = "module"),
    trait = factor(.data$module, unique(.data$module)),
    module = match(.data$trait, levels(.data$trait)))
}
#' Eigen Traits from Dataset
#' 
#' @param object 
#' @param sexname 
#' @param modulename 
#' @param contr_object 
#' @param eigen_object 
#' @return data frame
#' @rdname founder_helpers
eigen_traits_dataset <- function(object = NULL, sexname = NULL,
  modulename = NULL, contr_object = NULL,
  eigen_object = foundr::eigen_contrast(object, contr_object)) {
  if(is.null(object) | is.null(contr_object))
    return(NULL)
  
  if(!is_sex_module(object))
    return(eigen_traits_dataset_value(object, sexname, modulename, contr_object, eigen_object))
  
  eigen_traits_dataset_sex(object, sexname, modulename, contr_object, eigen_object)
}
eigen_traits_dataset_sex <- function(object = NULL, sexname = NULL,
  modulename = NULL, contr_object = NULL,
  eigen_object = foundr::eigen_contrast(object, contr_object)) {
  if(is.null(object) || is.null(contr_object) || is.null(eigen_object) ||
     is.null(modulename))
    return(NULL)
  
  datasetname <- stringr::str_remove(modulename, ": .*")
  modulename <- stringr::str_remove(modulename, ".*: ")
  if(!(datasetname %in% names(object)))
    return(NULL)
  sexes <- c(B = "Both Sexes", F = "Female", M = "Male", C = "Sex Contrast")
  sexes <- names(sexes)[match(sexname, sexes)]
  if(sexes != stringr::str_remove(modulename, "_.*"))
    return(NULL)
  
  foundr::eigen_traits(object[[datasetname]], sexname, modulename,
    dplyr::filter(contr_object, .data$dataset == datasetname),
    dplyr::filter(eigen_object, .data$dataset == datasetname))
}
eigen_traits_dataset_value <- function(object = NULL, sexname = NULL,
  modulename = NULL, contr_object = NULL,
  eigen_object = foundr::eigen_contrast(object, contr_object)) {
  if(is.null(object) | is.null(contr_object) | is.null(eigen_object))
    return(NULL)
  
  # Can only handle one trait module right now.
  if(length(object) > 1)
    return(NULL)
  
  # The `object` is one traitModule with element `value`.
  object <- object[[1]]
  datasets <- names(object)
  if(!all(datasets %in% "value"))
    return(NULL)
  
  modulename <- stringr::str_remove(modulename, "^.*: ")
  contr_object <- 
    dplyr::left_join(
      dplyr::filter(contr_object, sex %in% sexname),
      dplyr::filter(
        dplyr::mutate(
          dplyr::select(object$value$modules, -dropped),
          kME = signif(.data$kME, 4)),
        .data$module %in% modulename),
      by = c("dataset", "trait"))
  # Return contr_object after filtering
  # Could add columns from `object$value$modules`
  dplyr::select(
    dplyr::bind_rows(
      (dplyr::filter(eigen_object, trait == modulename, sex %in% sexname) |>
         dplyr::mutate(trait = "Eigen", module = modulename))[names(contr_object)],
      contr_object),
    -module)
}
#' Is this a sex module?
#' Apply eigen_contrast over list of `traitModule`s and `contr_object`.
#' @param object 
#' @return `TRUE` or `FALSE`
#' @rdname founder_helpers
is_sex_module <- function(object) {
  !("value" %in% names(object[[1]]))
}
#' Mutate Datasets
#' 
#' @param object 
#' @param datasets 
#' @param undo 
#' @return data frame with `dataset` and possibly `probandset` columns
#' @rdname founder_helpers
mutate_datasets <- function(object, datasets = NULL, undo = FALSE) {
  if(is.null(object))
    return(NULL)
  
  if(undo) {
    for(i in seq_along(datasets)) {
      object <- dplyr::mutate(
        object,
        dataset = ifelse(
          .data$dataset == datasets[[i]],
          names(datasets)[i], .data$dataset))
    }
  } else {
    if(is.null(datasets))
      return(object)
    
    object$dataset <- as.character(object$dataset)
    m <- match(object$dataset, names(datasets), nomatch = 0)
    object$dataset[m>0] <- datasets[m]
    if("probandset" %in% names(object)) {
      object$probandset <- as.character(object$probandset)
      m <- match(object$probandset, names(datasets), nomatch = 0)
      object$probandset[m>0] <- datasets[m]
    }
  }
  object
}
#' Order Choices
#' 
#' @param traitStats data frame
#' @return vector of stats terms
#' @rdname founder_helpers
order_choices <- function(traitStats) {
  p_types <- paste0("p_", unique(traitStats$term))
  p_types <- p_types[!(p_types %in% c("p_cellmean", "p_signal", "p_rest", "p_noise", "p_rawSD"))]
  p_types <- stringr::str_remove(p_types, "^p_")
  if("strain:diet" %in% p_types)
    p_types <- unique(c("strain:diet", p_types))
  c(p_types, "alphabetical", "original")
}
#' Order Trait Statistics
#'
#' @param orders name of order criterion 
#' @param traitStats data frame with statistics
#'
#' @return data frame
#' @importFrom dplyr arrange filter left_join select
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#' @rdname founder_helpers
order_trait_stats <- function(orders, traitStats) {
  if(is.null(traitStats)) return(NULL)
  if(is.null(orders)) return(traitStats)
  
  out <- traitStats
  if(orders == "alphabetical") {
    out <- dplyr::arrange(out, .data$trait)
  } else {
    if(orders != "original") {
      # Order by p.value for termname
      termname <- stringr::str_remove(orders, "p_")
      out <- 
        dplyr::arrange(
          dplyr::filter(
            out,
            .data$term == termname),
          .data$p.value)
    }
  }
  out
}
#' @param msg text string
#' @param size number
#' @param angle number
#' @return data frame
#' @rdname founder_helpers
plot_null <- function (msg = "no data", size = 10, angle = 0) 
{
  ggplot2::ggplot(data.frame(x = 1, y = 1)) +
    ggplot2::aes(
      .data$x, 
      .data$y,
      label = msg) +
    ggplot2::geom_text(size = size, angle = angle) + 
    ggplot2::theme_void()
}
#' Select data including Key Trait and Related Datasets
#'
#' @param object data frame
#' @param key_trait name of key trait
#' @param rel_dataset name of related datasets
#'
#' @importFrom dplyr filter select
#' @importFrom tidyr unite
#' @importFrom rlang .data
#' @return data frame
#' @rdname founder_helpers
select_data_pairs <- function(object, key_trait, rel_dataset = NULL) {
  if(is.null(object))
    return(NULL)
  
  dplyr::select(
    dplyr::filter(
      tidyr::unite(
        object,
        datatraits,
        .data$dataset, .data$trait,
        sep = ": ", remove = FALSE),
      (.data$datatraits %in% key_trait) |
        (.data$dataset %in% rel_dataset)),
    -datatraits)
}
#' Stats Time Table
#'
#' @param object list of data frames 
#' @param logp raise values to power 10 if not `TRUE`
#' @return data frame
#' @rdname founder_helpers
stats_time_table <- function(object, logp = FALSE) {
  if(is.null(object))
    return(NULL)
  
  logfn <- ifelse(logp, function(x) x, function(x) 10^-x)
  
  class(object) <- "list"
  for(i in names(object)) {
    object[[i]] <- as.data.frame(
      tidyr::separate_wider_delim(
        dplyr::select(
          dplyr::mutate(
            dplyr::rename(
              object[[i]], 
              p.value = i),
            p.value = signif(logfn(p.value), 4)),
          -strain),
        datatraits,
        delim = ": ",
        names = c("dataset", "trait")))
  }
  
  object <- dplyr::as_tibble(
    dplyr::bind_rows(
      object))
  
  # Find out time column.
  timecols <- c("week", "minute", "week_summary", "minute_summary")
  timecol <- match(timecols, names(object), nomatch = 0)
  timecol <- timecols[timecol > 0]
  
  dplyr::arrange(
    tidyr::pivot_wider(
      dplyr::mutate(
        object,
        term = factor(.data$term, unique(.data$term))),
      names_from = "term", values_from = p.value),
    .data$dataset, .data$trait, .data[[timecol]])
}
#' Summary fo traitTime Object
#'
#' @param object data frame
#' @param traitnames character list
#' @return data frame
#' @rdname founder_helpers
summary_traitTime <- function(object, traitnames = names(object$traits)) {
  # This is messy as it has to reverse engineer `value` in list.
  object <- 
    dplyr::bind_rows(
      purrr::map(
        object$traits[traitnames],
        function(x) {
          names(x)[match(attr(x, "pair")[2], names(x))] <- 
            "value"
          x
        }))
  object <- 
    tidyr::separate_wider_delim(
      dplyr::mutate(
        object,
        strain = factor(.data$strain, names(foundr::CCcolors)),
        value = signif(.data$value, 4)),
      datatraits,
      delim = ": ",
      names = c("dataset", "trait"))
  
  tidyr::pivot_wider(object, names_from = "strain", values_from = "value",
                     names_sort = TRUE)
}
#' Stat Terms
#'
#' @param object 
#' @param signal 
#' @param condition_name 
#' @param drop_noise 
#' @param cellmean 
#' @param ... 
#'
#' @return vector of terms
#' @rdname founder_helpers
term_stats <- function(object, signal = TRUE, condition_name = NULL,
                      drop_noise = TRUE, cellmean = signal, ...) {
  terms <- unique(object$term)
  # Drop noise and other terms not of interest to user.
  terms <- terms[!(terms %in% c("rest","rawSD"))]
  if(drop_noise) { 
    terms <- terms[terms != "noise"]
  }
  
  if(is.null(condition_name))
    condition_name <- "condition"
  if(signal) {
    # Return the strain terms with condition if present
    if(any(grepl(condition_name, terms)))
      terms <- c("signal", terms[grepl(paste0(".*strain.*", condition_name), terms)])
    else
      terms <- c("signal", terms[grepl(".*strain", terms)])
  } else {
    terms <- terms[terms != "signal"]
  }
  if(!cellmean) {
    terms <- terms[terms != "cellmean"]
  }
  terms
}
#' Time Trait Subset
#'
#' @param object 
#' @param timetrait_all 
#' @return data frame
#' @rdname founder_helpers
time_trait_subset <- function(object, timetrait_all) {
  if(is.null(object) || is.null(timetrait_all))
    return(NULL)
  
  object <- tidyr::unite(object,
                         datatraits,
                         .data$dataset, .data$trait,
                         remove = FALSE, sep = ": ")
  timetrait_all <- tidyr::unite(timetrait_all,
                                datatraits,
                                .data$dataset, .data$trait,
                                remove = FALSE, sep = ": ")
  dplyr::select(
    dplyr::filter(
      object,
      .data$datatraits %in% timetrait_all$datatraits),
    -datatraits)
}
#' Time Units
#'
#' @param timetrait_all data frame
#' @return data frame
#' @rdname founder_helpers
time_units <- function(timetrait_all) {
  # Find time units in datasets
  timeunits <- NULL
  if("minute" %in% timetrait_all$timetrait)
    timeunits <- c("minute","minute_summary")
  if("week" %in% timetrait_all$timetrait)
    timeunits <- c(timeunits, "week","week_summary")
  timeunits
}
#' Trait Pairs
#'
#' @param traitnames 
#' @param sep 
#' @param key 
#' @return vector or `trait1 ON trait2`
#' @rdname founder_helpers
trait_pairs <- function(traitnames, sep = " ON ", key = TRUE) {
  if(length(traitnames) < 2)
    return(NULL)
  
  if(key) {
    # Key Trait vs all others.
    paste(traitnames[-1], traitnames[1], sep = sep)
  } else {
    # All Trait Pairs, both directions.
    as.vector(
      unlist(
        dplyr::mutate(
          as.data.frame(utils::combn(traitnames, 2)),
          dplyr::across(
            dplyr::everything(), 
            function(x) {
              c(paste(x, collapse = sep),
                paste(rev(x), collapse = sep))
            }))))
  }
}
#' Volcano Defaults
#'
#' @param ordername name of order column
#' @return list of volcano parameters
#' @rdname founder_helpers
vol_default <- function(ordername) {
  vol <- list(min = 0, max = 10, step = 1, value = 2)
  switch(
    ordername,
    module = {
    },
    kME = {
      vol$min <- 0.8
      vol$max <- 1
      vol$step <- 0.05
      vol$value <- 0.8
    },
    p.value = {
      vol$min <- 0
      vol$max <- 10
      vol$step <- 1
      vol$value <- 2
    },
    size = {
      vol$min <- 0
      vol$max <- 30
      vol$step <- 5
      vol$value <- 15
    })
  vol$label <- ordername
  if(ordername == "p.value")
    vol$label <- "-log10(p.value)"
  
  vol
}