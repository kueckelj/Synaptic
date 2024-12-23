
# S4 classes --------------------------------------------------------------

#' Class `MeaDesign`
#'
#' A class that captures the essence and the setup of a Microelectrode Array (MEA) design.
#' This class focuses on the hardware configuration, electrode layout, and properties
#' of the MEA used in experiments.
#'
#' @slot company *character*. The name of the manufacturer of the MEA device.
#' @slot company_abbr *character*. Abbreviation of the manufacturer's name.
#' @slot elec_diam *character*. Electrode diameter (in SI units, e.g., `um` or `m`).
#' @slot elec_pos `data.frame`. Electrode positions in the layout. Required columns:
#'   \itemize{
#'     \item `id` (*integer*): Unique electrode ID.
#'     \item `col` (*integer*): Column index in the layout.
#'     \item `row` (*integer*): Row index in the layout.
#'     \item `x_orig` (*numeric*): X-coordinate position in original units.
#'     \item `y_orig` (*numeric*): Y-coordinate position in original units.
#'   }
#' @slot elec_shape *character*. Electrode shape (e.g., `square`, `circle`).
#' @slot elec_spacing *character*. Spacing or center-to-center distance between electrodes.
#' @slot layout_type *character*. Type of the electrode layout. Options include:
#'   \itemize{
#'     \item `grid`: Regular grid (e.g., *16x16*, *64x64*).
#'     \item `hexagonal`: Hexagonal arrangement.
#'     \item `individual`: Custom electrode arrangement (e.g., hippocampal).
#'   }
#' @slot product_name *character*. The name of the MEA product (e.g., *256MEA*, *MED-P50015*).
#' @slot outline data.frame. Vertices of a polygon to outline the electrodes. Three columns
#' *vertex* (character), *x_orig* (numeric), *y_orig* (numeric).
#' @slot units list. Default units to work with. Reserved slot names are *area*, *dist* and *time*.
#' @slot version `list`. A list containing version information.
#'
#' @seealso [`MeaData`]
#'
#' @export
MeaDesign <-
  setClass(
    Class = "MeaDesign",
    slots = list(
      company = "character",
      company_abbr = "character",
      elec_diam = "character",
      elec_pos = "data.frame",
      elec_shape = "character",
      elec_spacing = "character",
      layout_type = "character",
      outline = "data.frame",
      product_name = "character",
      units = "list",
      version = "list"
    )
  )


#' Class `MeaData`
#'
#' A class for storing MEA experimental data, including the \link[=MeaDesign]{MEA hardware design}, electrode-level
#' data, stimulation events, and metadata. In downstream analysis electrode data is conceptualized as a graph
#' network where *nodes* represent electrodes and *edges* represent their connections.
#'
#' @slot design `MeaDesign`. The MEA design setup as a [`MeaDesign`] object.
#' @slot edges `data.frame`. Connections between electrodes represented as edges.
#'   Required columns:
#'   \itemize{
#'     \item `from` (*integer*): Starting node (electrode ID).
#'     \item `to` (*integer*): Ending node (electrode ID).
#'     \item `weight` (*numeric*): Weight of the connection (e.g., distance or strength).
#'   }
#' @slot meta `list`. Metadata about the experiment, including:
#'   \itemize{
#'     \item `duration`: Duration of the experiment (e.g., in seconds).
#'     \item `time_interval`: Sampling time interval for the recordings.
#'   }
#' @slot images `list`. A list of associated images (e.g., snapshots of MEA recordings).
#' @slot name character. String that provides an informative label about the condition
#' under which the measurements were conducted. (e.g. the drug inside the medium).
#' @slot nodes `data.frame`. Node-level information about the electrodes. Required columns:
#'   \itemize{
#'     \item `id`: Electrode ID.
#'     \item `x_orig`: X-coordinate before scaling to image resolution.
#'     \item `y_orig`: Y-coordinate before scaling to image resolution.
#'   }
#' @slot obj_info A list containing object information such as default instructions and directories.
#' @slot sample character. Name of the tissue sample from which the MEA data was generated.
#' @slot stimulations `data.frame`. Stimulation event data. Columns include:
#'   \itemize{
#'     \item `id`: Electrode ID where stimulation occurred.
#'     \item `instance`: Time point of stimulation.
#'     \item `start`: Start time of stimulation.
#'     \item `end`: End time of stimulation.
#'     \item `amplitude`: Amplitude of the stimulation.
#'   }
#' @slot time_series `data.frame`. Time-series data for each electrode. Columns include:
#'   \itemize{
#'     \item `time`: Time points.
#'     \item `id`: Electrode ID.
#'     \item `value`: Recorded signal or firing rate.
#'   }
#' @slot version `list`. A list containing version information.
#' @export
#'
#' @section Difference between name and sample:
#' Slot `@sample` refers to a name for the tissue sample used. Slot `@name` refers
#' to the specific MEA data set that has been created according to the set up
#' captured by the [`MeaDesign`] object in slot `@MeaDesign`. This distinction is necessary,
#' since one sample (one tissue slide) can undergo multiple MEA experiments with
#' different set ups, particularly with respect to the condition/drug applied to the
#' medium as well as to time resolution.
#'
MeaData <-
  setClass(
    Class = "MeaData",
    slots = list(
      design = "MeaDesign",
      edges = "data.frame",
      meta = "list",
      images = "list",
      name = "character",
      nodes = "data.frame",
      obj_info = "list",
      sample = "character",
      stimulations = "data.frame",
      time_series = "data.frame",
      version = "list"
    )
  )

# Objects -----------------------------------------------------------------

choices_grid_vertices <-
  c("Bottom Left" = "bl",
    "Bottom Right" = "br",
    "Top Left" = "tl",
    "Top Right" = "tr"
  )

current_mea_version <- list(major = 0, minor = 0, patch = 9)

elec_position_dfs <-
  list(
    "256MEA" =
      tidyr::expand_grid(col = 1:16, row = 1:16) %>%
      dplyr::mutate(id = stringr::str_c(LETTERS[c(1:8,10:16, 18)][col], row)) %>%
      dplyr::filter(!id %in% c("A1", "A16", "R1", "R16")) %>%
      dplyr::select(id, col, row)
  )

mea_designs <-
  list(
    "256MEA" =
      MeaDesign(
        company = "MultiChannelSystems",
        company_abbr = "MCS",
        elec_diam = "30um",
        elec_pos = elec_position_dfs[["256MEA"]],
        elec_shape = "circle",
        elec_spacing = "200um",
        layout_type = "grid",
        product_name = "256MEA",
        units = list(area = "um2", dist = "um", time = "ms"),
        version = list(major = 0, minor = 0, patch = 9)
      )
  )




# Concepts ----------------------------------------------------------------



# Functions ---------------------------------------------------------------

#' @title Synaptiq documentation dummy
#'
#' @param object An object of class [`MeaData`], an object that contains MEA data
#' or, in case of S4 generics, objects of classes for which a methods has been defined.
#' @param edges A data.frame in which each observation corresponds to
#' an edge between a graph in which nodes represent the \link[=concept_electrode]{MEA-electrodes}.
#' @param alpha_by Character value or `NULL`. Variable name for transparency mapping.
#' @param color_by Character value or `NULL`. Variable name for color mapping.
#' @param edge_alpha Numeric value. Default transparency for edges. Used when `alpha_by` is `NULL`.
#' @param edge_color Character value. Default color for edges. Used when `color_by` is `NULL`.
#' @param edge_size Numeric value. Default thickness for edges. Used when `size_by` is `NULL`.
#' @param img_name Character value. The name of the image of interest - to which spatial
#' coordinates are scaled. Must be one of [`getImageNames()`]. By default, the active image is chosen.
#' @param mea_name Character value. The name of the [`MeaData`] object of interest.
#' @param node_alpha Numeric value. Default transparency for nodes Used when `alpha_by` is `NULL`.
#' @param node_color Character value. Default color for nodes. Used when `color_by` is `NULL`.
#' @param node_shape Numeric value. Shape of nodes.
#' @param node_size Numeric value. Size of nodes.
#' @param nodes A data.frame in which each observation corresponds to
#' an \link[=concept_electrode]{electrode} in a MEA grid - conceptualized as
#' nodes in a graph.
#' @param size_by Character value or `NULL`. Variable name for size (linewidth) mapping.
#'
mea_doc_dummy <- function(){}

#' @title update
#' @return The updated input object, containing the added, removed or computed results.
#' @keywords internal
update_dummy <- function(){}


# a -----------------------------------------------------------------------

#' @importFrom SPATA2 activateImage activeImage
#' @inherit SPATA2::activateImage title description params return
#' @rdname activateImage
#' @export
setMethod(
  f = "activateImage",
  signature = "MeaData",
  definition = function(object, img_name, unload = TRUE){

    act_img <- activeImage(object)

    if(act_img != img_name){

      object@images[[act_img]]@active <- FALSE

      if(isTRUE(unload)){

        object@images[[act_img]]@image <- EBImage::as.Image(x = base::matrix(0))

      }

      object@images[[img_name]]@active <- TRUE

      object@images[[img_name]] <- loadImage(object = object@images[[img_name]])

    } else {

      message(glue::glue("Image '{img_name}' is already active."))

    }

    return(object)

  }
)

#' @rdname activateImage
#' @export
setMethod(
  f = "activeImage",
  signature = "MeaData",
  definition = function(object){

    out <-
      purrr::keep(.x = object@images, .p = ~ .x@active) %>%
      names()

    if(is.null(out)){ out <- character() }

    return(out)

  }
)


#' @title Default MEA name
#'
#' @description Sets and extracts the active (default) \link[=MeaData]{MEA} name.
#'
#' @inherit SPATA2::argument_dummy params
#' @inherit mea_doc_dummy params
#'
#' @return
#'  \itemize{
#'     \item{activateMea()}: Updated `SPATA2` object.
#'     \item{activeMea()}: Character value. Name of the currently MEA data. Empty string if no MEA data exists.
#'  }
#'
#' @seealso [`getMeaData()`], [`getMeaNodes()`], [`getMeaEdges()`]
#'
#' @export
activeMea <- function(object){

  object@obj_info$active$mea

}

#' @rdname activeMea
#' @export
activateMea <- function(object, mea_name, verbose = NULL){

  hlpr_assign_arguments(object)

  check_mea_name(object, mea_name)

  object@obj_info$active$mea <- mea_name

  confuns::give_feedback(
    msg = glue::glue("Active MEA: {mea_name}."),
    verbose = verbose
  )

  return(object)

}

#' @title Obtain MEA data names
#'
#' @description
#' Extracts the names of all \link[=MeaData]{MEA} data objects in the object.
#'
#' @inherit SPATA2::argument_dummy params
#'
#' @return Character vector.
#'
#' @export
getMeaNames <- function(object){

  names(object@data_add$mea)

}



#' @title Add variables to MEA edges or nodes
#'
#' @description These functions add new variables or update existing ones in the MEA edge or node data
#' of the provided object. The variables are added by merging the existing edge or node data with
#' the supplied data frames (`edges_data` or `node_data`).
#'
#' @param edges_data A data.frame containing new or updated variables for MEA edges. The `id` column is required
#' to match rows with the existing edge data.
#' @param node_data A data.frame containing new or updated variables for MEA nodes. The `id` column is required
#' to match rows with the existing node data.
#' @param var_names *character* or `NULL`. The names of the variables to be added or updated. If `NULL` (default),
#' all columns in `edges_data` or `node_data` (excluding `id`) are used.
#' @inherit mea_doc_dummy params
#' @inherit SPATA2::argument_dummy params
#' @inherit SPATA2::update_dummy params
#'
#' @details
#' - **`addVarsToEdges`**: Adds or updates variables in the MEA edge data.
#' - **`addVarsToNodes`**: Adds or updates variables in the MEA node data.
#' - For both functions, the `id` column is used to align the input data with the existing edge or node data.
#'
#' @seealso [`getMeaEdges()`], [`getMeaNoes()`], [`setMeaEdges()`], [`setMeaNodes()`]
#'
#' @export

setGeneric(name = "addVarsToEdges", def = function(object, ...){

  standardGeneric(f = "addVarsToEdges")

})

#' @rdname addVarsToEdges
#' @export
setMethod(
  f = "addVarsToEdges",
  signature = "SPATA2",
  definition = function(object, edges_data, var_names = NULL, mea_name = activeMea(object), overwrite = FALSE){

    mea_data <- getMeaData(object, mea_name = mea_name)

    mea_data <-
      addVarsToEdges(mea_data, edges_data = edges_data, var_names = var_names, overwrite = overwrite)

    object <- setMeaData(object, mea_data = mea_data)

    return(object)

  }

)

#' @rdname addVarsToEdges
#' @export
setMethod(
  f = "addVarsToEdges",
  signature = "MeaData",
  definition = function(object, edges_data, var_names = NULL, overwrite = FALSE){

    if(is.null(var_names)){

      var_names <- colnames(dplyr::select(edges_data, -id))

    }

    edges <- getMeaEdges(object)

    confuns::check_none_of(
      input = var_names,
      against = colnames(edges),
      overwrite = overwrite
    )

    edges <- dplyr::select(edges, -dplyr::any_of(var_names))

    edges <- dplyr::left_join(x = edges, y = edges_data, by = "id")

    object <- setMeaEdges(object, edges = edges)

    return(object)

  }
)


#' @rdname addVarsToEdges
#' @export
setGeneric(name = "addVarsToNodes", def = function(object, ...){

  standardGeneric(f = "addVarsToNodes")

})

#' @rdname addVarsToEdges
#' @export
setMethod(
  f = "addVarsToNodes",
  signature = "SPATA2",
  definition = function(object, node_data, var_names = NULL, mea_name = activeMea(object), overwrite = FALSE){

    mea_data <- getMeaData(object, mea_name = mea_name)

    mea_data <-
      addVarsToNodes(mea_data, node_data = node_data, var_names = var_names, overwrite = overwrite)

    object <- setMeaData(object, mea_data = mea_data)

    return(object)

  }
)

#' @rdname addVarsToEdges
#' @export
setMethod(
  f = "addVarsToNodes",
  signature = "MeaData",
  definition = function(object, node_data, var_names = NULL, overwrite = FALSE){

    if(is.null(var_names)){

      var_names <- colnames(dplyr::select(node_data, -id))

    }

    nodes <- getMeaNodes(object)

    confuns::check_none_of(
      input = var_names,
      against = colnames(nodes),
      overwrite = overwrite
    )

    nodes <- dplyr::select(nodes, -dplyr::any_of(var_names))

    nodes <- dplyr::left_join(x = nodes, y = nodes_data, by = "id")

    object <- setMeaNodes(object, nodes = nodes)

    return(object)

  }
)

#' @title Adjust MEA IDs
#'
#' @description Ensures valid electrode/node IDS.
#'
#' @param df Nodes data.frame of the respective mea design.
#' @param mea_design Character value. The name of the MEA design/platform.
#' @export
#' @keywords internal
adjust_ids <- function(df, mea_design, verbose = FALSE){

  confuns::check_one_of(
    input = mea_design,
    against = names(mea_designs)
  )

  if(mea_design == "256MEA"){

    df$id <- stringr::str_c(LETTERS[c(1:8,10:16, 18)][df$col], df$row)

  } else {

    confuns::give_feedback(
      msg = glue::glue("No ID adjustment required for MEA design '{mea_design}'."),
      verbose = verbose,
      with.time = FALSE
    )

  }

  return(df)

}


# b -----------------------------------------------------------------------

#' @keywords internal
br_add <- function(height, break_add = NULL){

  if(base::is.null(break_add)){

    x <- base::ceiling((height - 400)/100)

    out <- x*5

  } else {

    out <- break_add

  }

  return(out)

}

#' @keywords internal
breaks <- function(n){

  base::rep("<br>", n) %>%
    stringr::str_c(collapse = "") %>%
    shiny::HTML()

}

# c -----------------------------------------------------------------------

#' @export
#' @keywords internal
check_mea_design <- function(mea_design, verbose = TRUE, ...){

  if(is.character(mea_design)){

    confuns::check_one_of(
      input = mea_design,
      against = names(mea_designs),
      fdb.opt = 2,
      ref.opt.2 = "known MEA designs"
    )

    output <- mea_designs[[mea_design]]

  } else if(is(mea_design, "MeaDesign")){

    if(purrr::is_empty(mea_design@product_name)){

      stop("MeaDesign object must have a valid @product_name.")

    }

    output <- mea_design

  } else {

    stop("Invali input for argument `mea_design`.")

  }

  confuns::give_feedback(
    msg = glue::glue("MEA design: {output@product_name}"),
    verbose = verbose,
    ...
  )

  return(output)

}

#' @export
#' @keywords internal
check_mea_name <- function(object, mea_name){

  containsMEA(object, error = TRUE)

  confuns::check_one_of(
    input = mea_name,
    against = getMeaNames(object),
    fdb.opt = 2,
    ref.opt.2 = "MEA experiments"
  )

}

#' @title Compute distance to nearest MEA electrode (NME)
#'
#' @description This function computes the distance between observations in the [`SPATA2`] object
#' and their \link[=identifyNME]{nearest MEA electrode}. The computed distances
#' are added as a new metadata variable in the object.
#'
#' @param nme_name Character value. The name of the metadata variable indicating the nearest
#' MEA node for each observation - as identified by [`identifyNME()`].
#' @param nme_dist_name Character value. The name of the metadata variable to store the computed distances.
#' @param unit The unit for the computed distances (e.g., `"um"`, `"mm"`).
#' Defaults to the object's default unit (`getDefaultUnit(object)`).
#'
#' @inherit SPATA2::argument_dummy params
#' @inherit update_dummy params
#'
#' @seealso [`addMetaDataObs()`], [`getMeaNodes()`], [`getCoordsDf()`], [`getCoordsMtr()`]
#'
computeDistToNME <- function(object,
                             nme_name,
                             nme_dist_name,
                             mea_name = activeMea(object),
                             unit = getDefaultUnit(object),
                             overwrite = FALSE){

  confuns::check_none_of(
    input = nme_name,
    against = getMetaNames(object),
    ref.against = "meta variable names",
    overwrite = overwrite
  )

  coords_df <- getCoordsDf(object, var_names = nme_name)

  coords_mtr <- getCoordsMtr(object)

  mea_nodes <- levels(coords_df[[nme_name]])

  nodes_mtr <-
    getMeaNodes(object, mea_name = mea_name) %>%
    dplyr::filter(id %in% {{mea_nodes}}) %>%
    dplyr::select(id, x, y) %>%
    tibble::column_to_rownames(var = "id") %>%
    as.matrix()

  nn_out <-
    RANN::nn2(
      data = nodes_mtr,
      query = coords_mtr,
      searchtype = "priority",
      k = 1
    )

  coords_df[[nme_dist_name]] <- nn_out$nn.dists
  coords_df[[nme_dist_name]] <- as_unit(input = coords_df[[nme_dist_name]], object = object, unit = unit)
  coords_df[[nme_dist_name]][coords_df[[nme_name]] == "none"] <- NA

  object <- addMetaDataObs(object, meta_obs_df = coords_df, var_names = nme_dist_name, overwrite = TRUE)

  return(object)

}

#' @importFrom SPATA2 containsHistoImages
#' @inherit SPATA2::containsHistoImages title description params return
#' @export
setMethod(
  f = "containsHistoImages",
  signature = "MeaData",
  definition = function(object, error = FALSE){

    out <- length(object@images) != 0

    if(isFALSE(out) & isTRUE(error)){

      stop("No image datain this MeaData object.")

    }

    return(out)

  }
)

#' @title Check availability of an image
#'
#' @description
#' Checks if the input object has one or more objects of class [`MeaData`] stored
#' in the respective slot.
#'
#' @inherit SPATA2::containsHistoImages params
#'
#' @return Logical value.
#'
#' @export
containsMEA <- function(object, error = FALSE){

  out <-
    length(purrr::keep(object@data_add$mea, .p = ~ is(.x, class2 = "MeaData"))) >= 1

  if(isFALSE(out) & isTRUE(error)){

    stop("This SPATA2 object does not contain any MEA data.")

  }

  return(out)

}

#' @export
#' @keywords internal
create_mea_grid_ui <- function(plot_height = "600px", break_add = NULL){

  height <- stringr::str_remove_all(plot_height, "px")

  shinydashboard::dashboardPage(

    shinydashboard::dashboardHeader(title = "MEA Grid"),

    shinydashboard::dashboardSidebar(
      collapsed = TRUE,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "MEA Grid",
          tabName = "mea_grid",
          selected = TRUE
        )
      )
    ),

    shinydashboard::dashboardBody(

      shinybusy::add_busy_spinner(spin = "cube-grid", color = "red"),

      shinydashboard::tabItems(

        shinydashboard::tabItem(
          tabName = "mea_grid",

          shiny::fluidRow(
            shiny::fluidRow(
              shiny::div(
                class = "large-plot",
                shiny::plotOutput(
                  outputId = "plot_bg",
                  height = plot_height,
                  brush = shiny::brushOpts(
                    id = "brushed_area",
                    resetOnNew = TRUE
                  ),
                  dblclick = "dbl_click",
                  hover = hoverOpts(
                    id = "hover",
                    delay = 100,
                    delayType = "throttle",
                    clip = TRUE,
                    nullOutside = TRUE
                  )
                ),
                shiny::plotOutput(
                  outputId = "plot_sm",
                  brush = shiny::brushOpts(
                    id = "brushed_area",
                    resetOnNew = TRUE
                  ),
                  dblclick = "dbl_click",
                  hover = hoverOpts(
                    id = "hover",
                    delay = 100,
                    delayType = "throttle",
                    clip = TRUE,
                    nullOutside = TRUE
                  ),
                  height = plot_height
                ),
                shiny::tags$style(
                  "
                        .large-plot {
                            position: relative;
                        }
                        #plot_bg {
                            position: absolute;
                        }
                        #plot_sm {
                            position: absolute;
                        }

                      "
                )
              )
            )
          ),
          shiny::HTML(text = base::rep("<br>", 30) %>% stringr::str_c(collapse = "")),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shinyModuleZoomingUI()
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::tags$h5(shiny::strong("Select Vertex:")),
              shinyWidgets::radioGroupButtons(
                inputId = "selected_vertex",
                label = NULL,
                choices = choices_grid_vertices,
                selected = "bl"
              )
            ),
            shiny::column(
              width = 4,
              shiny::fluidRow(
                shiny::tags$h5(shiny::strong("Pick action:"))
              ),
              shiny::fluidRow(
                shiny::actionButton(inputId = "expand_grid", label = "Expand Grid"),
                shiny::actionButton(inputId = "reset", label = "Reset")
              )
            ),
            shiny::column(
              width = 4,
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::tags$h5(shiny::strong("Number of rows:")),
                  shiny::numericInput(inputId = "nr", label = NULL, value = 16, min = 1, max = Inf)
                ),
                shiny::column(
                  width = 6,
                  shiny::tags$h5(shiny::strong("Number of columns:")),
                  shiny::numericInput(inputId = "nc", label = NULL, value = 16, min = 1, max = Inf)
                )
              )
            )
          ),
          shiny::HTML(text = base::rep("<br>", 2) %>% stringr::str_c(collapse = "")),
          shiny::fluidRow(
            shiny::column(width = 2),
            shiny::column(
              width = 8,
              align = "center",
              shinyWidgets::actionBttn(
                inputId = "close_app",
                label = "Close application",
                color = "success",
                style = "gradient"
              )
            ),
            shiny::column(width = 2)
          )
        )
      )
    )

  )

}

#' @export
#' @keywords internal
create_mea_grid <- function(object, img_name = "mea_overlay"){

  shiny::runApp(
    shiny::shinyApp(
      ui = create_mea_grid_ui(),
      server = function(input, output, session){

        # reactive values ---------------------------------------------------------

        mai_vec <- base::rep(0.5, 4)

        mea_grid_vertices_df <-
          tibble::tibble(
            vertex = c("bl", "br", "tl", "tr"),
            x = base::rep(NA, 4) %>% base::as.numeric(),
            y = base::rep(NA, 4) %>% base::as.numeric()
          )

        mea_grid_expanded <-
          tibble::tibble(
            id = base::character(0),
            col = base::numeric(0),
            row = base::numeric(0),
            x = base::numeric(0),
            y = base::numeric(0)
          )

        mgv_df <- shiny::reactiveVal(value = mea_grid_vertices_df)
        mge_df <- shiny::reactiveVal(value = mea_grid_expanded)

        # shiny modules -----------------------------------------------------------

        zooming_output <-
          shinyModuleZoomingServer(
            brushed_area = brushed_area,
            object = object,
            trigger_zoom_out = zoom_out
          )

        # reactive expressions ----------------------------------------------------

        brushed_area <- shiny::reactive({

          input$brushed_area

        })

        default_ranges <- shiny::reactive({

          getImageRange(object, img_name = img_name)

        })

        zoom_out <- shiny::reactive({

          # prevents error

        })

        zooming <- shiny::reactive({

          if(purrr::is_empty(zooming_output())){

            default_ranges()

          } else {

            zooming_output()

          }

        })


        # observe events ----------------------------------------------------------

        oe <- shiny::observeEvent(input$dbl_click,{

          mgv <- mgv_df()

          mgv[mgv$vertex == input$selected_vertex, "x"] <- input$dbl_click$x
          mgv[mgv$vertex == input$selected_vertex, "y"] <- input$dbl_click$y

          mgv_df(mgv)

        })

        oe <- shiny::observeEvent(input$expand_grid, {

          checkpoint(
            evaluate = !base::any(base::is.na(mgv_df()$x)),
            case_false = "vertices_incomplete",
            error_notifications = list(vertices_incomplete = "Need all four vertices.")
          )

          mea_grid_expanded <- expand_mea_grid(vertices = mgv_df(), n_rows = input$nr, n_cols = input$nc)

          mge_df(mea_grid_expanded)

        })

        oe <- shiny::observeEvent(input$reset, {

          mgv_df(mea_grid_vertices_df)
          mge_df(mea_grid_expanded)

        })


        # plots -------------------------------------------------------------------

        output$plot_bg <- shiny::renderPlot({

          shiny::req(zooming())

          ggplot2::ggplot() +
            ggpLayerImage(object, img_name = img_name) +
            ggplot2::theme_void() +
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = 'transparent'), #transparent panel bg
              plot.background = ggplot2::element_rect(fill = 'transparent', color = NA), #transparent plot bg
              panel.grid.major = ggplot2::element_blank(), #remove major gridlines
              panel.grid.minor = ggplot2::element_blank(), #remove minor gridlines
              legend.background = ggplot2::element_rect(fill = 'transparent'), #transparent legend bg
              legend.box.background = ggplot2::element_rect(fill = 'transparent') #transparent legend panel
            ) +
            ggplot2::coord_equal(xlim = zooming()$x, ylim = zooming()$y) +
            ggplot2::labs(x = NULL, y = NULL)

        }, bg = "transparent")

        output$plot_sm <- shiny::renderPlot({

          ggplot2::ggplot() +
            ggplot2::geom_point(
              data = mgv_df() %>% tidyr::drop_na(),
              mapping = ggplot2::aes(x = x, y = y),
              size = 4
            ) +
            ggrepel::geom_label_repel(
              data = mgv_df() %>% tidyr::drop_na(),
              mapping = ggplot2::aes(x = x, y = y, label = vertex),
              size = 4,
              min.segment.length = 0
            ) +
            ggplot2::geom_point(
              data = mge_df() %>% tidyr::drop_na(),
              mapping = ggplot2::aes(x = x, y = y),
              size = 0.5,
              alpha = 0.75
            ) +
            ggplot2::theme_void() +
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = 'transparent'), #transparent panel bg
              plot.background = ggplot2::element_rect(fill = 'transparent', color = NA), #transparent plot bg
              panel.grid.major = ggplot2::element_blank(), #remove major gridlines
              panel.grid.minor = ggplot2::element_blank(), #remove minor gridlines
              legend.background = ggplot2::element_rect(fill = 'transparent'), #transparent legend bg
              legend.box.background = ggplot2::element_rect(fill = 'transparent') #transparent legend panel
            ) +
            ggplot2::coord_equal(xlim = zooming()$x, ylim = zooming()$y) +
            ggplot2::labs(x = NULL, y = NULL)


        }, bg = "transparent")

        # return value ------------------------------------------------------------

        oe <- shiny::observeEvent(input$close_app, {

          isf <- getScaleFactor(object, img_name = img_name, fct_name = "image")

          mgv_out <-
            mgv_df() %>%
            dplyr::mutate(x_orig = x / {{isf}}, y_orig = y / {{isf}}) %>%
            dplyr::select(vertex, x_orig, y_orig)

          mge_out <-
            mge_df() %>%
            dplyr::mutate(x_orig = x / {{isf}}, y_orig = y / {{isf}}) %>%
            dplyr::select(id, col, row, x_orig, y_orig)

          return_value <- list(mgv_df = mgv_out , mea_grid_df = mge_out)

          shiny::stopApp(returnValue = return_value)

        })

      }
    )
  )

}


#' @title Create an empty MEA data object
#'
#' @description Creates an empty object of class [`MeaData`].
#'
#' @param name Character value. Name of the output object (as set in slot `@name`).
#' @param mea_design Character value or [`MeaDesign`]. The design of the MEA platform.
#' If character, must be one of `validMeaDesigns()`.
#'
#' @return An object of class [`MeaData`].
#'
#' @export
createMeaObjectEmpty <- function(name,
                                 mea_design,
                                 ...,
                                 verbose = TRUE){

  confuns::give_feedback(
    msg = "Creating a new MeaData object.",
    verbose = verbose
  )

  mea_design_obj <- check_mea_design(mea_design, verbose = verbose)

  mea_obj <-
    MeaData(
      name = name,
      design = mea_design_obj,
      version = current_mea_version,
      ...
    )

  return(mea_obj)

}

# d -----------------------------------------------------------------------

#' @title Obtain electrode information
#'
#' @description
#' Functions to extract hardwired information about the electrodes of the
#' [`MeaDesign`].
#'
#' @inherit mea_doc_dummy params
#'
#' @export
eDiam <- function(object){

  getMeaDesign(object)@elec_diam

}

#' @rdname eDiam
#' @export
eShape <- function(object){

  getMeaDesign(object)@elec_shape

}

#' @rdname eDiam
#' @export
setGeneric(name = "eSpacing", def = function(object, ...){

  standardGeneric(f = "eSpacing")

})

#' @rdname eDiam
#' @export
setMethod(
  f = "eSpacing",
  signature = "SPATA2",
  definition = function(object,
                        unit = getDefaultUnit(object),
                        img_name = activeImage(object)){

   object <- activateImageInt(object, img_name = img_name)

   getMeaData(object) %>%
     eSpacing(object = ., unit = unit)

  }
)

#' @rdname eDiam
#' @export
setMethod(
  f = "eSpacing",
  signature = "MeaData",
  definition = function(object,
                        unit = getDefaultUnit(object, which = "dist")){

    espace <- getMeaDesign(object)@elec_spacing

    # if unit == px, converts for active image
    as_unit(input = espace, unit = unit, object = object)

  }
)

#' @title Expand MEA Grid
#' @description This function interpolates a grid of points within a quadrilateral defined by four vertices.
#' @param vertices A tibble/data.frame carrying spatial information about the vertices of the grid, with 3 columns:
#'   \itemize{
#'     \item \code{vertex} (character): must include the four vertex labels \emph{'bl'}, \emph{'br'}, \emph{'tl'}, and \emph{'tr'}.
#'     \item \code{x} (numeric): x-coordinates of the vertices.
#'     \item \code{y} (numeric): y-coordinates of the vertices.
#'   }
#' @param n_rows Number of rows in the grid. Must be 2 or higher.
#' @param n_cols Number of columns in the grid. Must be 2 or higher.
#'
#' @return A tibble containing the interpolated grid points with columns:
#' \itemize{
#'   \item \code{id}: Unique grid point identifier in the form \code{x<col>y<row>}.
#'   \item \code{col}: Column index of the grid point.
#'   \item \code{row}: Row index of the grid point.
#'   \item \code{x}: x-coordinate of the grid point.
#'   \item \code{y}: y-coordinate of the grid point.
#' }
#'
#' @details
#' The vertices of `vertices` are named according to their relative positions:
#' \itemize{
#'   \item \strong{l}: Left
#'   \item \strong{r}: Right
#'   \item \strong{t}: Top
#'   \item \strong{b}: Bottom
#' }
#' Therefore, the vertices are named:
#' \itemize{
#'   \item \code{bl}: Bottom Left
#'   \item \code{br}: Bottom Right
#'   \item \code{tl}: Top Left
#'   \item \code{tr}: Top Right
#' }
#' The function linearly interpolates points along the left and right edges of the quadrilateral and
#' then interpolates grid points between these edges.
#' @examples
#' vertices <- tibble::tibble(
#'   vertex = c("lt", "rt", "rb", "lb"),
#'   x = c(598, 578, 286, 305),
#'   y = c(723, 1019, 997, 702)
#' )
#' expand_mea_grid(vertices, n_rows = 4, n_cols = 4)
#' @export

expand_mea_grid <- function(vertices, n_rows, n_cols) {

  # input validation
  if(n_rows < 2){

    give_feedback(fdb.fn = "stop", msg = "Number of grid rows must be 2 or higher.")

  }

  if(n_cols < 2){

    give_feedback(fdb.fn = "stop", msg = "Number of grid columns must be 2 or higher.")

  }

  # interpolate points along the left edge (bottom left to top left)
  is_left_edge <- vertices$vertex %in% c("bl", "tl")
  left_edge_points <-
    stats::approx(
      x = as.numeric(vertices$x[is_left_edge]),
      y = as.numeric(vertices$y[is_left_edge]),
      n = n_rows
    ) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      row_id = stringr::str_c("row_", row)
      )

  dist_bl <-
    compute_distance(
      starting_pos = as.numeric(left_edge_points[1, c("x", "y")]),
      final_pos = as.numeric(vertices[vertices$vertex=="bl", c("x", "y")])
    )

  dist_tl <-
    compute_distance(
      starting_pos = as.numeric(left_edge_points[1, c("x", "y")]),
      final_pos = as.numeric(vertices[vertices$vertex=="tl", c("x", "y")])
    )

  if(dist_bl > dist_tl){

    left_edge_points$row <- (n_rows+1)-left_edge_points$row
    left_edge_points$row_id <- stringr::str_c("row_", left_edge_points$row)

  }

  # interpolate points along the right edge (bottom right to top right)
  is_right_edge <- vertices$vertex %in% c("br", "tr")
  right_edge_points <-
    stats::approx(
      x = as.numeric(vertices$x[is_right_edge]),
      y = as.numeric(vertices$y[is_right_edge]),
      n = n_rows
    ) %>%
    as.data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      row_id = stringr::str_c("row_", row)
      )

  dist_br <-
    compute_distance(
      starting_pos = as.numeric(right_edge_points[1, c("x", "y")]),
      final_pos = as.numeric(vertices[vertices$vertex=="br", c("x", "y")])
    )

  dist_tr <-
    compute_distance(
      starting_pos = as.numeric(right_edge_points[1, c("x", "y")]),
      final_pos = as.numeric(vertices[vertices$vertex=="tr", c("x", "y")])
    )

  if(dist_br > dist_tr){

    right_edge_points$row <- (n_rows+1)-right_edge_points$row
    right_edge_points$row_id <- stringr::str_c("row_", right_edge_points$row)

  }

  # interpolate grid points between left and right edges
  grid_points <-
    purrr::map_df(
      .x = stringr::str_c("row_", 1:n_rows),
      .f = function(current_row) {

        # extract start and end points for the current row
        left_point <- as.numeric(left_edge_points[left_edge_points$row_id == current_row, c("x", "y")])
        right_point <- as.numeric(right_edge_points[right_edge_points$row_id == current_row, c("x", "y")])

        # interpolate points along the row
        interpolated_row <-
          stats::approx(
            x = c(left_point[1], right_point[1]),
            y = c(left_point[2], right_point[2]),
            n = n_cols
          ) %>%
          as.data.frame() %>%
          tibble::as_tibble() %>%
          dplyr::mutate(
            col_id = stringr::str_c("col_", dplyr::row_number()),
            row_id = current_row,
            col = as.numeric(stringr::str_remove_all(col_id, "col_")),
            row = as.numeric(stringr::str_remove_all(row_id, "row_")),
            id = stringr::str_c("x", col, "y", row)
          ) %>%
          dplyr::select(id, col, row, x, y)

        dist_lp <- compute_distance(left_point, as.numeric(interpolated_row[1, c("x", "y")]))
        dist_rp <- compute_distance(right_point, as.numeric(interpolated_row[1, c("x", "y")]))

        if(dist_lp > dist_rp){

          interpolated_row$col <- (n_cols+1)-interpolated_row$col

        }

        return(interpolated_row)
      }
    )

  return(grid_points)

}

# f -----------------------------------------------------------------------


#' @title Subset MEA object with logical expressions
#'
#' @description This function filters a [MeaData] object based on specified logical expressions,
#' retaining only the \link[=concept_mea_nodes]{nodes} and \link[=concept_mea_edges]{edges} that meet the criteria.
#' It has the same effect as the function [`subsetMEA()`] has, but it provides more convenient input options.
#'
#' Note the `.` prefix before the arguments.
#'
#' @inherit dplyr::filter params
#'
#' @inherit SPATA2::argument_dummy params
#' @inherit SPATA2::update_dummy return
#'
#' @details
#' The function filters the input `MeaData` object based on the logical expressions provided in `...`.
#' If no expressions are provided, the function returns the input object with a warning.
#' The observations that meet the criteria specified by the logical expressions are retained.
#'
filterMEA <- function(object, ..., .img_name = activeImage(object)){

  filter_expr <- rlang::enquos(...)

  ids <-
    getMeaNodes(object, img_name = .img_name) %>%
    dplyr::filter(!!!filter_expr) %>%
    dplyr::pull(ids)

  object <- subsetMEA(object, ids = ids, opt = "keep")

  return(object)

}


#' @export
#' @keywords internal
flipMeaCoords <- function(object, axis){

  ranges <- getImageRange(object)

  isf <- getScaleFactor(object, img_name = activeImage(object), fct_name = "image")

  for(mea_name in getMeaNames(object)){

    # extract mea data
    mea_obj <- getMeaData(object, mea_name = mea_name, img_transf = FALSE)

    # nodes
    nodes_df <- getMeaNodes(mea_obj, complete = TRUE)

    nodes_dff <-
      flip_coords_df(
        df = nodes_df,
        axis = axis,
        ranges =
          purrr::map(
            .x = getImageRange(object),
            .f = function(ir){ ir[2] <- ir[2]/isf; return(ir) }
          )
        )

    nodes_df <-
      dplyr::left_join(
        x = dplyr::select(nodes_df, -x_orig, -y_orig),
        y = dplyr::select(nodes_dff, id, x_orig, y_orig),
        by = "id"
      )

    mea_obj <- setMeaNodes(mea_obj, nodes = nodes_df)

    # electrode pos
    mea_obj@design@elec_pos <-
      dplyr::left_join(
        x = dplyr::select(mea_obj@design@elec_pos, -dplyr::any_of(c("x_orig", "y_orig"))),
        y = dplyr::select(nodes_df, id, x_orig, y_orig),
        by = "id"
      )

    # set mea data
    object <- setMeaData(object, mea_data = mea_obj)

  }

  # electrodes
  return(object)

}

# g -----------------------------------------------------------------------

#' @export
#' @keywords internal
gamma_correct_image <- function(object, img_name, exp){

  hist_img <- getHistoImage(object, img_name = img_name)

  image <- hist_img@image

  image <- normalize(image)

  image <- image ^ 0.5

  hist_img@image <- image

  object <- setHistoImage(object, hist_img = hist_img)

  return(object)

}



#' @title Obtain default units for MEA objects
#'
#' @description This function retrieves the default unit for specific measurements or properties
#' (e.g., distances) associated with the given MEA object, based on the object's design or spatial configuration.
#'
#' @param which *character* (optional). Specifies the type of unit to retrieve. Common values include:
#'   \itemize{
#'     \item `"dist"`: Units for distances.
#'   }
#'        Default is `"dist"`.
#' @param ... Additional arguments passed to specific methods (not used in current implementation).
#' @inherit mea_doc_dummy params
#'
#' @return The default unit as a *character* value, corresponding to the specified type (`which`).
#'
#' @details
#' - **SPATA2 Method**: Retrieves the unit from the object's spatial method for distances or delegates
#'   to the MEA design if another unit type is specified.
#' - **MeaData Method**: Delegates to the associated MEA design to retrieve the unit.
#' - **MeaDesign Method**: Retrieves the unit directly from the `@units` slot of the `MeaDesign` object.
#'
#' @seealso [`getSpatialMethod()`], [`getMeaDesign()`]
#'
#' @rdname getDefaultUnit
#' @export
setGeneric(name = "getDefaultUnit", def = function(object, ...) {
  standardGeneric(f = "getDefaultUnit")
})

#' @rdname getDefaultUnit
#' @export
setMethod(
  f = "getDefaultUnit",
  signature = "SPATA2",
  definition = function(object, mea_name = activeMea(object), which = "dist"){

    if (which == "dist") {

      out <- getSpatialMethod(object)@unit

    } else {

      out <-
        getMeaDesign(object, mea_name = mea_name) %>%
        getDefaultUnit(object = ., which = which)

    }

    return(out)

  }
)

#' @rdname getDefaultUnit
#' @export
setMethod(
  f = "getDefaultUnit",
  signature = "MeaData",
  definition = function(object, which = "dist") {

    getMeaDesign(object) %>%
      getDefaultUnit(object = ., which = which)

  }
)

#' @rdname getDefaultUnit
#' @export
setMethod(
  f = "getDefaultUnit",
  signature = "MeaDesign",
  definition = function(object, which = "dist") {

    confuns::check_one_of(
      input = which,
      against = names(object@units),
      fdb.opt = 2,
      ref.opt.2 = "known unit types"
    )

    object@units[[which]]

  }
)



#' @importFrom SPATA2 getHistoImage
#' @inherit SPATA2::getHistoImage title description params return
#' @export
setMethod(
  f = "getHistoImage",
  signature = "MeaData",
  definition = function(object, img_name = activeImage(object), ...){

    confuns::check_one_of(
      input = img_name,
      against = names(object@images)
    )

    object@images[[img_name]]

  }
)

#' @export
getImageScaleFactor <- function(object, img_name = activeImage(object)){

  if(containsHistoImages(object)){

    isf <- getScaleFactor(object, img_name = img_name, fct_name = "image")

  } else {

    isf <- 1

  }

  return(isf)

}

#' @title Obtain MEA data
#'
#' @description
#' Extracts an object of class [`MeaData`].
#'
#' @inherit SPATA2::argument_dummy params
#' @inherit mea_doc_dummy params
#'
#' @return An object of class [`MeaData`].
#'
#' @export
getMeaData <- function(object, mea_name = activeMea(object), img_transf = TRUE){

  check_mea_name(object, mea_name)

  mea <- object@data_add$mea[[mea_name]]

  if(containsHistoImages(object) && isTRUE(img_transf)){

    img_names <- getImageNames(object)

    mea@images <-
      purrr::map(.x = img_names, .f = ~ getHistoImage(object, img_name = .x)) %>%
      purrr::set_names(nm = img_names)

  }

  return(mea)

}


#' @title Obtain MEA design
#'
#' @description
#' Extracts an object of class [`MeaDesign`].
#'
#' @inherit SPATA2::argument_dummy params
#' @inherit mea_doc_dummy params
#'
#' @return An object of class [`MeaDesign`].
#'
#' @export
setGeneric(name = "getMeaDesign", def = function(object, ...){

  standardGeneric(f = "getMeaDesign")

})

#' @rdname getMeaDesign
#' @export
setMethod(
  f = "getMeaDesign",
  signature = "SPATA2",
  definition = function(object, mea_name = activeMea(object)){

    getMeaData(object, mea_name = mea_name) %>%
      getMeaDesign()

  }
)

#' @rdname getMeaDesign
#' @export
setMethod(
  f = "getMeaDesign",
  signature = "MeaData",
  definition = function(object){

    object@design

  }
)



#' @title Obtain feature names for MEA edges and nodes
#'
#' @description These functions extract the feature names (column names) from the data associated
#' with microelectrode array (MEA) edges or nodes, excluding identifiers and protected variables.
#' Optional filtering can be applied to include only specific features.
#'
#' @param object An object containing MEA data and metadata.
#' @param mea_name The name of the MEA data to use. Defaults to the
#' active MEA in the object (`activeMea(object)`).
#' @param .p *function* or *formula* (optional). A predicate function or formula used to filter
#' the features. Only features satisfying the predicate will be included in the output.
#'
#' @return A character vector containing the names of the features for:
#' \itemize{
#'   \item `getMeaEdgeFeatures()`: MEA edges.
#'   \item `getMeaNodeFeatures()`: MEA nodes.
#' }
#'
#' @examples
#' # Retrieve all edge features
#' edge_features <- getMeaEdgeFeatures(object)
#'
#' # Retrieve all node features
#' node_features <- getMeaNodeFeatures(object)
#'
#' # Retrieve only numeric edge features
#' numeric_edge_features <- getMeaEdgeFeatures(object, .p = is.numeric)
#'
#' @seealso [`getMeaEdges()`], [`getMeaNodes()`]
#'
#' @export
getMeaEdgeFeatures <- function(object, mea_name = activeMea(object), .p = NULL) {

  edges_df <-
    getMeaEdges(object, mea_name = mea_name) %>%
    dplyr::select(-id, -from, -to, -mea_name, -sample, -dplyr::any_of(SPATA2::protected_variable_names_lst$spatial))

  if (purrr::is_formula(.p) | purrr::is_function(.p)) {

    edges_df <- purrr::keep(edges_df, .p = .p)

  }

  out <- colnames(edges_df)

  return(out)
}


#' @title Obtain MEA edges
#'
#' @description Extracts the edge data for the specified Microelectrode Array (MEA). The edges
#' represent connections between electrodes (nodes) in the MEA and include spatial coordinates.
#' Coordinates are scaled based on the associated image's scaling factor.
#'
#' @param object An object of class [`SPATA2`] or [`MeaData`].
#' @inherit mea_doc_dummy params
#'
#' @return A `data.frame` containing edge information with the following columns:
#' \itemize{
#'   \item *id*: Edge ID.
#'   \item *mea_name*: Name of [`MeaData`] object.
#'   \item *sample*: Name of tissue sample.
#'   \item `from`: Starting node (electrode ID).
#'   \item `to`: Ending node (electrode ID).
#'   \item `x`: Scaled x-coordinate of the starting node.
#'   \item `y`: Scaled y-coordinate of the starting node.
#'   \item `xend`: Scaled x-coordinate of the ending node.
#'   \item `yend`: Scaled y-coordinate of the ending node.
#'   \item Additional columns based on the specific MEA edge data.
#' }
#'
#' @details
#' - **SPATA2 Method**: Delegates the operation to the corresponding `MeaData` object within the `SPATA2` object.
#' - **MeaData Method**:
#'   - Retrieves the edge data and scales the spatial coordinates (`x`, `y`, `xend`, `yend`)
#'     based on the scaling factor of the associated image.
#'   - The original spatial coordinates (`x_orig`, `y_orig`, `xend_orig`, `yend_orig`) are retained.
#'
#' @seealso [getMeaNodes()], [getMeaData()], [getImageScaleFactor()]
#'
#' @examples
#' # Retrieve edges from a SPATA2 object
#' edges <- getMeaEdges(object)
#'
#' # Retrieve edges from a MeaData object with a specific image
#' edges <- getMeaEdges(mea_data, img_name = "example_image")
#'
#' @rdname getMeaEdges
#' @export

setGeneric(name = "getMeaEdges", def = function(object, ...){

  standardGeneric("getMeaEdges")

})

#' @rdname getMeaEdges
#' @export
setMethod(
  f = "getMeaEdges",
  signature = "SPATA2",
  definition = function(object, mea_name = activeMea(object), img_name = activeImage(object)){

    getMeaData(object, mea_name = mea_name) %>%
      getMeaEdges(object = ., img_name = img_name)

  }
)

#' @rdname getMeaEdges
#' @export
setMethod(
  f = "getMeaEdges",
  signature = "MeaData",
  definition = function(object, img_name = activeImage(object)){

    edges_df <- object@edges

    isf <- getImageScaleFactor(object, img_name = img_name)

    edges_df$x <- edges_df$x_orig * isf
    edges_df$y <- edges_df$y_orig * isf
    edges_df$xend <- edges_df$xend_orig * isf
    edges_df$yend <- edges_df$yend_orig * isf
    edges_df$mea_name <- object@name
    edges_df$sample <- object@sample
    edges_df$id <- stringr::str_c(edges_df$from, edges_df$to, sep = "_")

    edges_df <- dplyr::select(edges_df, id, mea_name, sample, from, to, x, y, xend, yend, dplyr::everything())

    return(edges_df)

  }
)


#' @title Obtain MEA data as a graph
#'
#' @description
#' Extracts nodes and edges of the MEA data as a graph object.
#'
#' @inherit mea_doc_dummy params
#'
#' @return A graph object.
#'
#' @export
setGeneric(name = "getMeaGraph", def = function(object, ...){

  standardGeneric(f = "getMeaGraph")

})

#' @rdname getMeaGraph
#' @export
setMethod(
  f = "getMeaGraph",
  signature = "SPATA2",
  definition = function(object, mea_name = activeMea(object), img_name = activeImage(object), tidy = TRUE){

    getMeaData(object, mea_name = mea_name) %>%
      getMeaGraph(object = ., img_name = img_name, tidy = tidy)

  }
)

#' @rdname getMeaGraph
#' @export
setMethod(
  f = "getMeaGraph",
  signature = "MeaData",
  definition = function(object, img_name = activeImage(object), tidy = TRUE){

    nodes <- getMeaNodes(object, img_name = img_name)

    edges <- getMeaEdges(object)

    if(isTRUE(tidy)){

      out <- tidygraph::tbl_graph(nodes = nodes, edges = edges, node_key = "id")

    } else {

      stop("Input = `tidy = FALSE` is currently not supported.")

    }

    return(out)

  }
)


#' @rdname getMeaEdgeFeatures
#' @export
getMeaNodeFeatures <- function(object, mea_name = activeMea(object), .p = NULL) {
  nodes_df <-
    getMeaNodes(object, mea_name = mea_name) %>%
    dplyr::select(-id, -mea_name, -sample, -dplyr::any_of(protected_variable_names_lst$spatial))

  if (purrr::is_formula(.p) | purrr::is_function(.p)) {
    nodes_df <- purrr::keep(nodes_df, .p = .p)
  }

  out <- colnames(nodes_df)
  return(out)
}



#' @title Obtain MEA nodes
#'
#' @description Extracts the node data for the specified Microelectrode Array (MEA). The nodes represent
#' electrodes in the MEA grid, and their spatial coordinates can be adjusted to account for the
#' scaling factors of associated images.
#'
#' @param object An object of class [`SPATA2`] or [`MeaData`].
#' @param inherit mea_doc_dummy params
#' @param complete *logical* (optional). If `FALSE` (default), filters out nodes marked as missing.
#'
#' @return A `data.frame` containing node information with the following columns:
#' \itemize{
#'   \item `id`: Node identifier.
#'   \item `mea_name`: Name of the MEA.
#'   \item `sample`: Sample identifier.
#'   \item `x`: X-coordinate (scaled).
#'   \item `y`: Y-coordinate (scaled).
#'   \item Additional columns based on the specific MEA data.
#' }
#'
#' @seealso [`getImageScaleFactor()`], [`getMeaData()`]
#'
#' @examples
#' # Retrieve nodes from a SPATA2 object
#' nodes <- getMeaNodes(object)
#'
#' # Retrieve nodes from a MeaData object with specific settings
#' nodes <- getMeaNodes(mea_data, img_name = "example_image", complete = TRUE)
#'
#' @rdname getMeaNodes
#' @export

setGeneric(name = "getMeaNodes", def = function(object, ...){

  standardGeneric("getMeaNodes")

})

#' @rdname getMeaNodes
#' @export
setMethod(
  f = "getMeaNodes",
  signature = "SPATA2",
  definition = function(object, mea_name = activeMea(object), img_name = activeImage(object), complete = FALSE, ...){

    getMeaData(object, mea_name = mea_name, img_transf = TRUE) %>%
      getMeaNodes(object = ., img_name = img_name, complete = complete)

  }
)

#' @rdname getMeaNodes
#' @export
setMethod(
  f = "getMeaNodes",
  signature = "MeaData",
  definition = function(object, img_name = activeImage(object), complete = FALSE, ...){

    isf <- getImageScaleFactor(object, img_name = img_name)

    nodes_df <- object@nodes

    if(isFALSE(complete)){

      nodes_df <- dplyr::filter(nodes_df, !missing)

    }

    out <-
      dplyr::mutate(
        .data = nodes_df,
        x = x_orig * {{isf}},
        y = y_orig * {{isf}},
        sample = object@sample,
        mea_name = object@name,
      ) %>% dplyr::select(id, mea_name, sample, x, y, dplyr::everything())

    return(out)

  }
)

#' @title Obtain MEA outline
#'
#' @description Extracts the outline of the specified Microelectrode Array (MEA). The outline defines
#' the spatial boundary of the MEA, and its coordinates can be adjusted to account for the scaling
#' factors of associated images.
#'
#' @param object An object of class [`SPATA2`] or [`MeaData`].
#' @inherit mea_doc_dummy params
#'
#' @return A `data.frame` containing the outline information with the following columns:
#' \itemize{
#'   \item `vertex`: Vertex label (e.g., `bl`, `tl`, `tr`, `br`).
#'   \item `x`: X-coordinate (scaled).
#'   \item `y`: Y-coordinate (scaled).
#'   \item Additional columns based on the specific MEA design.
#' }
#'
#' @seealso [`getImageScaleFactor()`], [`getMeaData()`], [`getMeaDesign()`]
#'
#' @examples
#' # Retrieve outline from a SPATA2 object
#' outline <- getMeaOutline(object)
#'
#' # Retrieve outline from a MeaData object with specific settings
#' outline <- getMeaOutline(mea_data, img_name = "example_image")
#'
#' @rdname getMeaOutline
#' @export

setGeneric(name = "getMeaOutline", def = function(object, ...){

  standardGeneric(f = "getMeaOutline")

})

#' @rdname getMeaOutline
#' @export
setMethod(
  f = "getMeaOutline",
  signature = "SPATA2",
  definition = function(object, img_name = activeImage(object), ...){

    # mea outline is the same for all MeaDesigns in a SPATA2 object
    # mea_name must not be specified by the user - just pick the active one
    getMeaData(object) %>%
      getMeaOutline(object = ., img_name = img_name)

  }
)

#' @rdname getMeaOutline
#' @export
setMethod(
  f = "getMeaOutline",
  signature = "MeaData",
  definition = function(object, img_name = activeImage(object), ...){

    isf <- getImageScaleFactor(object, img_name = img_name)

    outline_df <-
      dplyr::mutate(
        .data = object@design@outline,
        x = x_orig * {{isf}},
        y = y_orig * {{isf}}
      )

    md <- getMeaDesign(object)

    if(md@layout_type == "grid"){

      vert_order <- c("bl", "tl", "tr", "br")

    }

    outline_df <-
      outline_df %>%
      dplyr::mutate(vertex_order = base::match(vertex, vert_order)) %>%
      dplyr::arrange(vertex_order) %>%
      dplyr::select(-vertex_order)

    return(outline_df)

  }
)


#' @importFrom SPATA2 getPixelScaleFactor
#' @inherit SPATA2::getPixelScaleFactor title description params return
#' @export
setMethod(
  f = "getPixelScaleFactor",
  signature = "MeaData",
  definition = function(object,
                        img_name = activeImage(object),
                        unit = getDefaultUnit(object, "dist"),
                        switch = FALSE,
                        add_attr = TRUE,
                        ...){

    if(containsHistoImages(object)){

      psf <-
        getHistoImage(object, img_name = img_name) %>%
        getPixelScaleFactor(object = ., unit = unit, switch = switch, add_attr = add_attr)

    } else {

      psf <- 1

    }

  }
)

#' @inherit SPATA2::getScaleFactor title description params return details
#' @export
setMethod(
  f = "getScaleFactor",
  signature = "MeaData",
  definition = function(object, img_name = activeImage(object), fct_name){

    if(!purrr::is_empty(img_name)){

      out <-
        getHistoImage(object, img_name = img_name) %>%
        getScaleFactor(object = ., fct_name = fct_name)

    } else {

      out <- 1

    }

    return(out)

  }
)

#' @title Create ggplot2 layer for MEA edges
#'
#' @description This function generates a `ggplot2` layer (`geom_segment`) for visualizing
#' edges of a microelectrode array (MEA) in a 2D plot. The appearance of the edges
#' can be customized by mapping edge properties (e.g., alpha, color, size) to specific variables.
#'
#' @param object An object of class [`MeaData`] or [`SPATA2`].
#' @param edges_subset Character vector or `NULL`. If character, specifies the
#' edges to be displayed by ID. If `NULL`, the default, all edges are displayed.
#' @param ... Additional arguments passed to `scale_color_<color_input>`.
#'
#' @inherit mea_doc_dummy params
#' @inherit SPATA2::argument_dummy params
#' @inherit SPATA2::ggpLayerImage return
#'
#' @export
ggpLayerMeaEdges <- function(object,
                             edges_subset = NULL,
                             alpha_by = NULL,
                             color_by = NULL,
                             size_by = NULL,
                             edge_alpha = 0.9,
                             edge_color = "black",
                             edge_size = 1,
                             mea_name = activeMea(object),
                             img_name = activeImage(object),
                             transform_with = NULL,
                             clrp = "default",
                             clrp_adjust = NULL,
                             clrsp = "inferno",
                             ...){

  if(is.function(transform_with)){

    transform_with <-
      purrr::map(
        .x = c(alpha_by, color_by),
        .f = ~ transform_with
      ) %>%
      purrr::set_names(nm = c(alpha_by, color_by))

  }

  edges_df <-
    getMeaEdges(object, mea_name = mea_name, img_name = img_name)

  if(is.character(edges_subset)){

    edges_df <- dplyr::filter(edges_df, id %in% {{edges_subset}})

  }

  edges_df <-
    confuns::transform_df(df = edges_df, transform.with = transform_with)

  mapping <- ggplot2::aes(x = x, y = y, xend = xend, yend = yend)

  gg_aes <-
    manage_aesthetics(
      alpha_by = alpha_by,
      color_by = color_by,
      size_by = size_by,
      alpha = edge_alpha,
      color = edge_color,
      size = edge_size,
      mapping = mapping
    )

  mapping <- gg_aes$mapping
  params <- gg_aes$params

  # create layer
  out <-
    list(
      geom_segment_mea_edges =
        ggplot2::geom_segment(
          mapping = mapping,
          data = edges_df,
          stat = "identity",
          position = "identity",
          !!!params
        )
    )

  if(is.character(color_by)){

    out$new_color_scale_mea_edges <-
      ggnewscale::new_scale_color()

    out$scale_color_add_on_mea_edges <-
      SPATA2::scale_color_add_on(
        variable = edges_df[[color_by]],
        clrp = clrp,
        clrsp = clrsp,
        clrp.adjust = clrp_adjust
      )

    out <-
      out[c("new_color_scale_mea_edges", "geom_segment_mea_edges", "scale_color_add_on_mea_edges")]

  }

  return(out)

}


#' @title Create ggplot2 layer for MEA nodes
#'
#' @description This function generates a `ggplot2` layer (`geom_point`) for visualizing
#' the nodes of a microelectrode array (MEA) in a 2D plot. The appearance of the nodes
#' can be customized by mapping properties (e.g., alpha, color) to specific variables
#' or by setting default values.
#'
#' @param object An object of class [`MeaData`] or [`SPATA2`].
#' @param nodes_subset Character vector or `NULL`. If character, specifies the
#' nodes to be displayed by ID. If `NULL`, the default, all nodes are displayed.
#' @param ... Additional arguments passed to `scale_color_<color_input>`.
#'
#' @inherit mea_doc_dummy params
#' @inherit SPATA2::argument_dummy params
#' @inherit SPATA2::ggpLayerImage return
#'
#' @export
ggpLayerMeaNodes <- function(object,
                             nodes_subset = NULL,
                             alpha_by = NULL,
                             color_by = NULL,
                             node_alpha = 0.9,
                             node_color = "black",
                             node_shape = eShape(object),
                             node_size = 1,
                             mea_name = activeMea(object),
                             img_name = activeImage(object),
                             transform_with = NULL,
                             clrp = "default",
                             clrp_adjust = NULL,
                             clrsp = "inferno",
                             ...){


  if(is.function(transform_with)){

    transform_with <-
      purrr::map(
        .x = c(alpha_by, color_by),
        .f = ~ transform_with
      ) %>%
      purrr::set_names(nm = c(alpha_by, color_by))

  }

  nodes_df <-
    getMeaNodes(object, mea_name = mea_name, img_name = img_name)

  if(is.character(nodes_subset)){

    nodes_df <-
      dplyr::filter(nodes_df, id %in% {{nodes_subset}})

  }

  nodes_df <-
    confuns::transform_df(df = nodes_df, transform.with = transform_with)

  mapping <- ggplot2::aes(x = x, y = y)

  gg_aes <-
    manage_aesthetics(
      alpha_by = alpha_by,
      color_by = color_by,
      alpha = node_alpha,
      color = node_color,
      shape = node_shape,
      size = node_size,
      mapping = mapping
    )

  mapping <- gg_aes$mapping
  params <- gg_aes$params

  out <-
    list(
      geom_point_fixed_mea_nodes =
        geom_point_fixed(
          params,
          data = nodes_df,
          mapping = mapping,
          stat = "identity",
          position = "identity"
        )
    )

  if(is.character(color_by)){

    out$new_color_scale_mea_nodes <-
      ggnewscale::new_scale_color()

    out$scale_color_add_on_mea_nodes <-
      SPATA2::scale_color_add_on(
        variable = nodes_df[[color_by]],
        clrp = clrp,
        clrsp = clrsp,
        clrp.adjust = clrp_adjust
        )

    out <-
      out[c("new_color_scale_mea_nodes", "geom_point_fixed_mea_nodes", "scale_color_add_on_mea_nodes")]

  }

  return(out)

}

#' @title Create ggplot2 layer for MEA outline
#'
#' @description This function generates a `ggplot2` layer (`geom_polygon`) to visualize the outline
#' of a Microelectrode Array (MEA) in a 2D plot. The outline can optionally be expanded by a
#' specified buffer distance.
#'
#' @param object An object of class [`SPATA2`] or [`MeaData`].
#' @param opt Character vector. Option *'outline'*, outlines the MEA with a line. Option *'crop'* zooms in.
#' Both can be specified simultaneously.
#' @param line_alpha *numeric* (optional). Transparency of the outline. Default is `1` (fully opaque).
#' @param line_color *character* (optional). Color of the outline. Default is `"black"`.
#' @param line_size *numeric* (optional). Thickness of the outline. Default is `1`.
#' @param expand_outline *numeric* (optional). If provided, the outline will be expanded by the
#' buffer distance (in the same units as the MEA coordinates). Default is `NULL`.
#' @param label_vertices Logical value. If `TRUE`, the labels vertices used to outline the
#' electrode array are displayed. Note that `expand_outline` does not affect the position of
#' the labels but only the outline, if drawn.
#' @param ... Additional arguments passed to [`ggrepel::geom_label_repel()`] if `label_vertices = TRUE`.
#'
#' @inherit mea_doc_dummy params
#'
#' @return A `ggplot2` layer (`geom_polygon`) representing the MEA outline.
#'
#' @details
#' - The outline is extracted using the `getMeaOutline()` function and represents the spatial
#'   boundary of the MEA.
#' - If `expand_outline` is provided, the `buffer_area()` function is used to enlarge the outline
#'   by the specified distance.
#' - The outline is drawn as a polygon with customizable transparency, color, and line thickness.
#'
#' @seealso [`getMeaOutline()`]
#'
#' @export
ggpLayerMeaOutline <- function(object,
                               opt = "outline",
                               img_name = activeImage(object),
                               line_alpha = 1,
                               line_color = "black",
                               line_size = 1,
                               expand_outline = NULL,
                               label_vertices = FALSE,
                               ...){

  # mea outline is the same for all MeaDesigns in a SPATA2 object
  # mea_name must not be specified by the user - just pick the active one
  outline <- getMeaOutline(object, img_name = img_name)

  if(is_dist(expand_outline)){

    expand_outline <- as_pixel(input = expand_outline, object = object)
    outline_final <- buffer_area(df = outline, buffer = expand_outline)

  } else {

    outline_final <- outline

  }

  out <- list()

  if("outline" %in% opt){

    out$mea_outline <-
      ggplot2::geom_polygon(
        data = outline_final,
        mapping = ggplot2::aes(x = x, y = y),
        alpha = line_alpha,
        color = line_color,
        size = line_size,
        fill = NA
      )

  }

  if("crop" %in% opt){

    out$mea_crop <-
      ggpLayerZoom(
        object = object,
        xrange = range(outline_final$x),
        yrange = range(outline_final$y),
        expand_x = NULL,
        expand_y = NULL
      )

  }

  if(isTRUE(label_vertices)){

    out$mea_vertices <-
      ggrepel::geom_label_repel(
        mapping = ggplot2::aes(x = x, y = y, label = vertex),
        data = outline,
        ...
      )

  }

  return(out)

}



# h -----------------------------------------------------------------------


# i -----------------------------------------------------------------------


#' @title Identify nearest MEA electrode (NME)
#'
#' @description This function identifies the nearest electrode for each observation
#' in the [`SPATA2`] object and adds the information as a metadata variable. Observations
#' that are farther than a specified maximum distance (`max_dist`) are assigned an
#' "out-of-range" value.
#'
#' @param ... Expressions that return a logical value, used to subset the \link[concept_mea_nodes]{MEA nodes}
#' prior to the neighbor identification.
#' @param nme_name Character value. The name of the new metadata variable that will store the
#' nearest electrode IDs.
#' @param max_dist \link[=concept_distance_measure]{Distance measure}. Maximum allowable distance for assigning a
#' nearest electrode. Observations beyond this distance are marked as "out-of-range". Defaults to half of
#' the average electrode spacing.
#' @param complete Logical value. If `TRUE`, the default, electrodes with no data associated are included
#' as potential neighbors.
#'
#' @inherit SPATA2::argument_dummy params
#' @inherit update_dummy return
#'
#' @details
#' This function identifies the nearest electrode (NME) for each \link[=concept_observations]{observation}
#' in the [`SPATA2`] object. A filtering step prior to the neighbor-mapping allows subsetting of electrodes
#' based on logical conditions, which can exclude irrelevant electrodes from the neighbor
#' assignment process via `...`. Observations farther than `max_dist` from any valid neighbor are labeled with *'none'*.
#' The choice of filtering directly affects the pool of potential neighbors,
#' potentially increasing the likelihood of observations being labeled "out-of-range" if too
#' many electrodes are excluded.
#'
#' The `complete` argument determines whether all electrodes or only those with associated data
#' are considered:
#' - **`TRUE` (default)**: Includes all electrodes, regardless of associated data, ensuring that
#'   spatial relationships use the entire MEA layout.
#' - **`FALSE`**: Considers only electrodes with associated data, focusing the neighbor search
#'   on relevant subsets of electrodes.
#'
#' See `getMeaNodes(..., complete = TRUE/FALSE)`.
#'
#' @seealso [`addMetaDataObs()`], [`getMeaNodes()`], [`computeDistToNMN()`]
#'
#' @export
identifyNME <- function(object,
                        ...,
                        nme_name,
                        max_dist = eSpacing(object, "px")/2,
                        mea_name = activeMea(object),
                        complete = TRUE,
                        overwrite = FALSE){

  containsMEA(object, error = TRUE)

  oor_name = "none"

  # ensure pixel units (numeric value)
  if(is_dist_si(max_dist)){

    max_dist <- as_pixel(input = max_dist, object = object)

  }

  confuns::check_none_of(
    input = nme_name,
    against = getMetaNames(object),
    ref.against = "meta variables",
    overwrite = overwrite
  )

  # extract data
  coords_mtr <- getCoordsMtr(object)

  filter_expr <- rlang::enquos(...)

  nodes_mtr <-
    getMeaNodes(object, complete = complete) %>%
    dplyr::filter(!!!filter_expr) %>%
    dplyr::select(id, x, y) %>%
    tibble::column_to_rownames(var = "id") %>%
    as.matrix()

  if(nrow(nodes_mtr) == 0){

    stop("No MEA nodes after filtering.")

  }

  # identify neighbors
  nn_out <-
    RANN::nn2(
      data = nodes_mtr,
      query = coords_mtr,
      searchtype = "priority",
      k = 1
    )

  nn_df <-
    tibble::tibble(
      barcodes = rownames(coords_mtr),
      !!rlang::sym(nme_name) := rownames(nodes_mtr)[nn_out$nn.idx]
    )

  neighbor_electrodes <- unique(nn_df[[nme_name]])

  nn_df[[nme_name]][nn_out$nn.dists > max_dist] <- oor_name

  nn_df[[nme_name]] <- factor(nn_df[[nme_name]], levels = c(neighbor_electrodes, oor_name))

  # add to object
  object <- addMetaDataObs(object, meta_obs_df = nn_df, overwrite = TRUE)

  return(object)

}

# m -----------------------------------------------------------------------

#' @title Manage ggplot2 aesthetics and parameters
#'
#' @description This function manages the setup of aesthetics (`aes`) for `ggplot2`
#' layers and returns both the `mapping` and unused parameters as a list.
#'
#' @param alpha_by Character value. Column name for transparency mapping.
#' @param color_by Character value. Column name for color mapping.
#' @param shape_by Character value. Column name for size mapping.
#' @param size_by Character value. Column name for size mapping.
#' @param alpha Numeric value. Default alpha value if `alpha_by` is not provided.
#' @param color Character value. Default color if `color_by` is not provided.
#' @param shape Character value. Default shape if `shape_by` is not provided.
#' @param size Numeric value. Default size value if `size_by` is not provided.
#' @param mapping list. Mapping to start with.
#'
#' @return A list containing:
#' \describe{
#'   \item{`mapping`}{A ggplot2 aesthetics mapping object.}
#'   \item{`params`}{A named list of unused parameters to pass directly to `ggplot2::geom_*`.}
#' }
#'
#' @export
manage_aesthetics <- function(alpha_by = NULL,
                              color_by = NULL,
                              shape_by = NULL,
                              size_by = NULL,
                              alpha = NULL,
                              color = NULL,
                              shape = NULL,
                              size = NULL,
                              mapping = ggplot2::aes()) {

  # Identify unused aesthetics
  unused_aesthetics <-
    purrr::keep(
      .x = list(alpha = alpha_by, color = color_by, shape = shape_by, size = size_by),
      .p = is.null
    ) %>% names()

  # Filter unused parameters
  params <- list(alpha = alpha, color = color, shape = shape, size = size)
  params <- params[unused_aesthetics]
  params <- purrr::discard(params, .p = is.null)

  # Dynamically add aesthetics
  if(!is.null(alpha_by)){

    mapping <- utils::modifyList(mapping, ggplot2::aes(alpha = .data[[alpha_by]]))

  }

  if(!is.null(color_by)){

    mapping <- utils::modifyList(mapping, ggplot2::aes(color = .data[[color_by]]))

  }

  if(!is.null(size_by)){

    mapping <- utils::modifyList(mapping, ggplot2::aes(size = .data[[size_by]]))

  }

  return(list(mapping = mapping, params = params))
}

# n -----------------------------------------------------------------------

#' @title Count MEA data content
#'
#' @description Returns the number of instances of miscellaneous aspects in the [`MeaData`]
#' object.
#'
#' @inherit mea_doc_dummy params
#'
#' @return Numeric value.
#'
#' @export
nElectrodes <- function(object){

  nrow(getMeaDesign(object)@elec_pos)

}

#' @rdname nElectrodes
#' @export
setGeneric(name = "nMeaNodes", def = function(object, ...){

  standardGeneric(f = "nMeaNodes")

})

#' @rdname nElectrodes
#' @export
setMethod(
  f = "nMeaNodes",
  signature = "SPATA2",
  definition = function(object, mea_name = activeMea(object)){

    getMeaNodes(object, mea_name = mea_name, complete = FALSE) %>%
      nrow()

  }
)

#' @rdname nElectrodes
#' @export
setMethod(
  f = "nMeaNodes",
  signature = "MeaData",
  definition = function(object, mea_name = activeMea(object)){

    getMeaNodes(object) %>%
      nrow()

  }
)

# p -----------------------------------------------------------------------

#' @title Pass node data to SPATA2 observations
#'
#' @description This function maps metadata from MEA nodes to observations in a [`SPATA2`] object
#' based on the nearest electrode assignment. The metadata is added as new variables to the
#' [`SPATA2`] object.
#'
#' @param var_names A character vector of variable names from the MEA nodes to transfer to
#' the SPATA2 observations.
#' @param naming Character value. A glue-compatible template for renaming transferred variables.
#' Defaults to `"{var_name}"`. Renaming must not change the number of variable names.
#' @inherit identifyNME params
#' @inherit SPATA2::argument_dummy params
#' @inherit mea_doc_dummy params
#'
#' @details
#' The function first identifies the nearest electrode for each observation in the SPATA object using
#' [identifyNME()]. Metadata variables from the MEA nodes, specified in `var_names`, are then
#' transferred to the observations. Observations farther than `max_dist` from any electrode are excluded
#' from the mapping and will obtain NA values!
#'
#' Transferred variables can be renamed using the `naming` argument, allowing for
#' custom naming patterns in the [`SPATA2`] metadata.
#'
#' @seealso [`identifyNME()`], [`addMetaDataObs()`], [`getMeaNodes()`]
#'
#' @export

passNodeDataToSpata <- function(object,
                                var_names,
                                mea_name = activeMea(object),
                                max_dist = Inf,
                                naming = "{var_name}",
                                overwrite = FALSE){

  confuns::check_one_of(
    input = var_names,
    against = getMeaNodeFeatures(object, mea_name = mea_name),
    fdb.opt = 2,
    ref.opt.2 = "meta variable names"
  )

  var_names_renamed <-
    purrr::map_chr(
      .x = var_names,
      .f = function(var_name){ glue::glue(naming)}
      )

  if(dplyr::n_distinct(var_names) != dplyr::n_distinct(var_names_renamed)){

    stop(glue::glue("Renaming with `naming = {naming}` changes the number of unique names."))

  }

  confuns::check_none_of(
    input = var_names_renamed,
    against = getMetaNames(object),
    ref.against = "meta variables",
    overwrite = overwrite
  )

  neighbor_df <-
    identifyNME(object, nme_name = "id", overwrite = TRUE, mea_name = mea_name, max_dist = max_dist) %>%
    getMetaDf(object = .) %>%
    tidyr::drop_na(id) %>%
    dplyr::select(-dplyr::any_of(var_names_renamed), -dplyr::any_of(var_names))

  # work with var_names until variable names have actually been renamed
  var_names <- unique(var_names[var_names != "id"])
  nodes_df <- getMeaNodes(object, mea_name = mea_name)[,c("id", var_names)]

  meta_df <-
    dplyr::left_join(x = neighbor_df, y = nodes_df, by = "id") %>%
    dplyr::rename_with(
      .cols = dplyr::all_of(var_names),
      .fn = function(var_name){ glue::glue(naming) }
    )

  object <-
    addMetaDataObs(
      object = object,
      meta_obs_df = meta_df,
      var_names = var_names_renamed,
      overwrite = overwrite
    )

  return(object)

}





#' @title Pass SPATA2 metadata to MEA nodes
#'
#' @description This function maps metadata variables from a [`SPATA2`] object to MEA nodes, based on
#' the nearest electrode (NME) for each observation. The mapped metadata is added as variables
#' to the nodes in the MEA design.
#'
#' @param var_names A character vector of metadata variable names in the [`SPATA2`] object to transfer
#' to the MEA nodes.
#' @param mea_names A character vector of MEA names to which the metadata should be
#' transferred. Defaults to all MEAs in the object (`getMeaNames(object)`).
#' @param max_dist *numeric* (optional). Maximum allowable distance for mapping metadata from
#' observations to nodes. Observations beyond this distance are excluded from the mapping.
#' Default is `Inf`.
#' @inherit identifyNME params
#' @inherit SPATA2::argument_dummy params
#' @inherit mea_doc_dummy params
#'
#' @details
#' For each MEA specified in `mea_names`, this function identifies the nearest electrode (NME)
#' for each observation using [identifyNME()]. Metadata variables specified in `var_names` are
#' then transferred to the corresponding nodes. If multiple observations map to the same node,
#' the closest observation (based on distance) is used. Observations beyond `max_dist` are
#' excluded from the mapping and will obtain NA values.
#'
#' The function supports transferring variables to multiple MEAs within the same [`SPATA2`] object
#' with `mea_names`, which defaults to all MEA objects.
#'
#' @seealso [identifyNME()], [addVarsToNodes()], [computeDistToNME()]
#'
#' @export
passSpataMetaToNodes <- function(object,
                                 var_names,
                                 mea_names = getMeaNames(object),
                                 max_dist = Inf,
                                 overwrite = FALSE){

  confuns::check_one_of(
    input = var_names,
    against = getMetaNames(object),
    fdb.opt = 2,
    ref.opt.2 = "meta variable names"
  )

  for(mea_name in mea_names){

    var_names <- unique(var_names[!var_names %in% c("id", "barcodes")])

    meta_df <-
      getMetaDf(object) %>%
      dplyr::select(barcodes, dplyr::all_of(var_names))

    node_data <-
      identifyNME(object, mea_name = mea_name, nme_name = "id", max_dist = max_dist, overwrite = TRUE) %>%
      computeDistToNME(object = ., nme_name = "id", nme_dist_name = "ne_dist", unit = "px", overwrite = TRUE) %>%
      getMetaDf(object = .) %>%
      dplyr::select(barcodes, id, ne_dist) %>%
      dplyr::left_join(x = ., y = meta_df, by = "barcodes") %>%
      dplyr::select(-barcodes) %>%
      dplyr::distinct() %>%
      dplyr::group_by(id) %>%
      dplyr::slice_min(ne_dist, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()

    object <-
      addVarsToNodes(
        object = object,
        node_data = node_data,
        var_names = var_names,
        overwrite = overwrite
      )

  }

  return(object)

}


#' @export
#' @keywords internal
plotElectrodes <- function(object,
                           img_name = activeImage(object),
                           label = NULL,
                           node_alpha = 1,
                           node_color = "black",
                           node_size = 1.5,
                           ...){

  nodes_df <-
    getMeaNodes(object, img_name = img_name, complete = TRUE)

  if(is.character(label)){

    label_df <-
      dplyr::filter(nodes_df, id %in% {{label}})

    label_add_on <-
      ggrepel::geom_text_repel(
        data = label_df,
        mapping = ggplot2::aes(x = x, y = y, label = id),
        min.segment.length = 0,
        ...
      )

  } else {

    label_add_on <- NULL

  }

  plotImage(object, img_name = img_name) +
    geom_point_fixed(
      data = nodes_df,
      mapping = ggplot2::aes(x = x, y = y),
      alpha = node_alpha,
      color = node_color,
      size = node_size
    ) +
    label_add_on

}

# r -----------------------------------------------------------------------

#' @export
#' @keywords internal
read_mea_file <- function(dir, ...){

  dir <- dir[1]

  if(stringr::str_detect(dir, pattern = ".csv")){

    out <- readr::read_csv(file = dir, ...)

  } else if(stringr::str_detect(dir, patter = ".xlsx")){

    out <- readxl::read_xlsx(path = dir, ...)

  } else {

    stop("File formats other than .csv and .xlsx are currently not supported.")

  }

  return(out)

}

#' @title Read and set MEA edges
#'
#' @description This function reads edge data from an external file, validates the data, and sets
#' it as the edge information in a `MeaData` object. The function ensures that the edge connections
#' overlap with the nodes in the MEA design and associates spatial coordinates with the edges.
#'
#' @param object An object of class [`MeaData`].
#' @param dir *character*. The directory of the file containing the edge data.
#' @param var_from *character*. The name of the variable in the file that corresponds to the
#' starting node (`from`) of each edge.
#' @param var_to *character*. The name of the variable in the file that corresponds to the
#' ending node (`to`) of each edge.
#' @param var_weight *character*. The name of the variable in the file that specifies the weight
#' of each edge.
#' @param ... Additional arguments passed to [read_mea_file()].
#'
#' @return A modified `MeaData` object with the updated edge data.
#'
#' @details
#' - The function validates the presence of the specified variables (`var_from`, `var_to`, `var_weight`)
#'   in the file and checks for overlap between the edges and nodes in the MEA design.
#' - Spatial coordinates for edges are joined based on the associated node positions.
#'
#' @examples
#' # Read edges from a file and set them in the MeaData object
#' mea_data <- readMeaEdges(
#'   object = mea_data,
#'   dir = "path/to/edges_file.csv",
#'   var_from = "start_node",
#'   var_to = "end_node",
#'   var_weight = "connection_strength"
#' )
#'
#' @export
readMeaEdges <- function(object, dir, var_from, var_to, var_weight, ...){

  nodes_df <- getMeaNodes(object)

  edges_df <- read_mea_file(dir = dir, ...)

  # check columns
  if(!var_from %in% colnames(edges_df)){

    confuns::give_feedback(
      msg = glue::glue("Variable '{var_from}' not found in data."),
      fdb.fn = "stop"
    )

  } else {

    edges_df$from <- edges_df[[var_from]]
    edges_df[[var_from]] <- NULL

  }

  if(!var_to %in% colnames(edges_df)){

    confuns::give_feedback(
      msg = glue::glue("Variable '{var_to}' not found in data."),
      fdb.fn = "stop"
    )

  } else {

    edges_df$to <- edges_df[[var_to]]
    edges_df[[var_to]] <- NULL

  }

  if(!var_weight %in% colnames(edges_df)){

    confuns::give_feedback(
      msg = glue::glue("Variable '{var_weight}' not found in data."),
      fdb.fn = "stop"
    )

  } else {

    edges_df$weight <- edges_df[[var_weight]]
    edges_df[[var_weight]] <- NULL

  }

  # check overlap
  from_ovlp <- length(intersect(edges_df$from, nodes_df$id))

  if(from_ovlp == 0){

    confuns::give_feedback(
      msg = glue::glue("No overlap between node IDs and {var_from}."),
      fdb.fn = "stop"
    )

  }

  to_ovlp <- length(intersect(edges_df$to, nodes_df$id))

  if(to_ovlp == 0){

    confuns::give_feedback(
      msg = glue::glue("No overlap between node IDs and {to_ovlp}."),
      fdb.fn = "stop"
    )

  }

  # join spatial coords
  edges_df <-
    dplyr::left_join(x = edges_df, y = dplyr::select(nodes_df, id, x_orig, y_orig), by = c("from" = "id")) %>%
    dplyr::left_join(x = ., y = dplyr::select(nodes_df, id, xend_orig = x_orig, yend_orig = y_orig), by = c("to" = "id")) %>%
    dplyr::select(from, to, x_orig, y_orig, xend_orig, yend_orig, dplyr::everything())

  # set and return
  object <- setMeaEdges(object, edges = edges_df)

  return(object)

}

#' @title Read and set MEA nodes
#'
#' @description This function reads node data from an external file, validates the data, and sets
#' it as the node information in a `MeaData` object. The function checks for overlap with the MEA
#' design and handles missing data for expected electrodes.
#'
#' @param object An object of class [`MeaData`].
#' @param dir *character*. The directory of the file containing the node data.
#' @param var_id *character*. The name of the variable in the file that corresponds to the node
#' or electrode IDs.
#' @param ... Additional arguments passed to [`read_mea_file()`].
#'
#' @return A modified `MeaData` object with the updated node data.
#'
#' @details
#' - The function validates the presence of the specified variable (`var_id`) in the file and ensures
#'   that the IDs are unique.
#' - Nodes that do not overlap with the expected MEA design electrodes are flagged as missing.
#' - The node data is joined with the MEA design's electrode positions to create the final node data frame.
#'
#' @seealso [read_mea_file()], [setMeaNodes()], [getMeaDesign()]
#'
#' @examples
#' # Read nodes from a file and set them in the MeaData object
#' mea_data <- readMeaNodes(
#'   object = mea_data,
#'   dir = "path/to/nodes_file.csv",
#'   var_id = "electrode_id"
#' )
#'
#' @export
readMeaNodes <- function(object, dir, var_id, ...){

  nodes_df <- read_mea_file(dir = dir, ...)

  # test if var_id in colnames
  if(!var_id %in% colnames(nodes_df)){

    confuns::give_feedback(
      msg = glue::glue("Electrode ID-variable '{var_id}' not found in input file."),
      verbose = verbose,
      fdb.fn = "stop",
      with.time = FALSE
    )

  }

  # rename var id
  if(var_id != "id"){

    nodes_df$id <- nodes_df[[var_id]]
    nodes_df[[var_id]] <- NULL

  }

  # test if key
  confuns::is_key_variable(df = nodes_df, key.name = "id", stop.if.false = TRUE)

  nodes_df$missing <- FALSE

  # test if overlap with MeaDesign
  md <- object@design

  missing <- md@elec_pos$id[!md@elec_pos$id %in% nodes_df$id]
  n_missing <- length(missing)

  if(n_missing == nrow(md@elec_pos)){

    stop("No match between expected electrode IDs and file input.",)

  } else if(n_missing >= 1){

    missing_ref <- confuns::scollapse(missing)

    warning(glue::glue("Missing data for node(s)/electrode(s): '{missing_ref}'."))

  }

  # join and return MEA object
  nodes_final <-
    dplyr::left_join(x = md@elec_pos, y = nodes_df, by = "id") %>%
    dplyr::mutate(missing = is.na(missing))

  object <- setMeaNodes(object, nodes = nodes_final)

  return(object)

}





# s -----------------------------------------------------------------------

#' @title Set MEA data
#'
#' @description
#' Sets [`MeaData`] objects.
#'
#' @inherit SPATA2::argument_dummy params
#' @inherit update_dummy return
#'
#' @export
setMeaData <- function(object, mea_data, verbose = NULL){

  hlpr_assign_arguments(object)

  stopifnot(is(object = mea_data, class2 = "MeaData"))

  activate <- !containsMEA(object)

  # note: also change containsMEA if you relocate mea input in SPATA2
  object@data_add$mea[[mea_data@name]] <- mea_data

  if(activate){

    object <- activateMea(object, mea_name = mea_data@name, verbose = verbose)

  }

  return(object)

}


#' @title Set MEA edges or nodes
#'
#' @description These functions set edge or node data for a `SPATA2` or `MeaData` object. The data
#' is validated and stored in the corresponding slots of the `MeaData` object or within a `SPATA2` object.
#'
#' @param object An object of class [`SPATA2`] or [`MeaData`].
#' @param edges A `data.frame` containing edge data to be stored in the object. Must include the
#'        columns `from`, `to`, and optionally spatial coordinates (`x_orig`, `y_orig`, `xend_orig`, `yend_orig`).
#' @param nodes A `data.frame` containing node data to be stored in the object. Must include the
#'        column `id`, representing the unique identifiers for the nodes.
#' @param ... Additional arguments (not used in the current implementation).
#'
#' @inherit update_dummy return
#'
#' @details
#' - **Edges**:
#'   - `setMeaEdges` stores the provided edge data in the `edges` slot of a `MeaData` object.
#'   - The edge data is validated to ensure it includes required columns and matches the expected data structure.
#' - **Nodes**:
#'   - `setMeaNodes` stores the provided node data in the `nodes` slot of a `MeaData` object.
#'   - The node data is validated to ensure it includes the required column (`id`) and matches the expected structure.
#' - When used with a `SPATA2` object, the methods delegate the operation to the corresponding `MeaData` object.
#'
#' @seealso [getMeaData()], [setMeaData()]
#'
#' @export
setGeneric(name = "setMeaEdges", def = function(object, ...){

  standardGeneric(f = "setMeaEdges")


})

#' @rdname setMeaEdges
#' @export
setMethod(
  f = "setMeaEdges",
  signature = "SPATA2",
  definition = function(object, edges){

    mea_name <- unique(edges$mea_name)

    stopifnot(length(mea_name) == 1)

    mea_data <- getMeaData(object, mea_name = mea_name)

    mea_data <- setMeaEdges(mea_data, edges = edges)

    object <- setMeaData(object, mea_data = mea_data)

    return(object)

  }
)

#' @rdname setMeaEdges
#' @export
setMethod(
  f = "setMeaEdges",
  signature = "MeaData",
  definition = function(object, edges){

    confuns::check_data_frame(edges, var.class = list(from = "character", to = "character"))

    object@edges <-
      dplyr::select(edges, from, to, x_orig, xend_orig, y_orig, yend_orig, dplyr::everything())

    return(object)

  }
)


#' @rdname setMeaEdges
#' @export
setGeneric(name = "setMeaNodes", def = function(object, ...){

  standardGeneric(f = "setMeaNodes")

})


#' @rdname setMeaEdges
#' @export
setMethod(
  f = "setMeaNodes",
  signature = "SPATA2",
  definition = function(object, nodes){

    mea_name <- unique(nodes$mea_name)

    stopifnot(length(mea_name) == 1)

    mea_data <- getMeaData(object, mea_name = mea_name)

    mea_data <- setMeaNodes(mea_data, nodes = nodes)

    object <- setMeaData(object, mea_data = mea_data)

    return(object)

  }
)

#' @rdname setMeaEdges
#' @export
setMethod(
  f = "setMeaNodes",
  signature = "MeaData",
  definition = function(object, nodes){

    confuns::check_data_frame(nodes, var.class = list(id = "character"))

    object@nodes <- dplyr::select(nodes, id, dplyr::everything())

    return(object)

  }
)


#' @describeIn MeaDesign Show method for the MeaDesign class
setMethod(
  f = "show",
  signature = "MeaDesign",
  definition = function(object) {
    cat("Microelectrode Array Design (MeaDesign):\n")
    cat("  Product              :", object@product_name, "\n")
    cat("  Manufacturer         :", object@company, paste0("(", object@company_abbr, ")\n"))
    cat("  Layout Type          :", object@layout_type, "\n")
    cat("  Electrode Shape      :", object@elec_shape, "\n")
    cat("  Electrode Diameter   :", object@elec_diam, "\n")
    cat("  Electrode Spacing    :", object@elec_spacing, "\n")
    cat("  Number of Electrodes :", nrow(object@elec_pos), "\n")
    cat("  Outline Vertices     :", nrow(object@outline), "\n")
    cat("  Version              :", paste(object@version$major, object@version$minor, sep = "."), "\n")
  }
)

#' @describeIn MeaData Show method for the MeaData class
setMethod(
  f = "show",
  signature = "MeaData",
  definition = function(object) {
    cat("Microelectrode Array Data (MeaData):\n")
    cat("  Name             :", object@name, "\n")
    cat("  Design           :", object@design@product_name, "by", object@design@company, "\n")
    cat("  Number of Nodes  :", nrow(getMeaNodes(object)), "(of", nrow(object@design@elec_pos), "electrodes)", "\n")
    cat("  Number of Edges  :", nrow(object@edges), "\n")
    cat("  Stimulations     :", nrow(object@stimulations), "recorded\n")
    cat("  Time Series      :", nrow(object@time_series), "entries\n")
    cat("  Images           :", length(object@images), "associated images\n")
    cat("  Metadata         :", length(object@meta), "items\n")
    cat("  Version          :", paste("v", object@version$major, object@version$minor, object@version$patch, sep = "."), "\n")
  }
)





#' @title Subset MEA data
#'
#' @description This function subsets the nodes and edges in a `SPATA2` or `MeaData` object
#' based on a specified set of node IDs. The subset operation can either keep or remove
#' the specified IDs from the data.
#'
#' @param object An object of class [`SPATA2`] or [`MeaData`].
#' @param ids Character vector of node IDs to subset the data.
#' @param opt Character value. Specifies the subset operation. Must be one of:
#'   \itemize{
#'     \item `"keep"`: Keeps only the specified node IDs and their associated edges.
#'     \item `"remove"`: Removes the specified node IDs and their associated edges.
#'   }
#'   Default is `"keep"`.
#' @param ... Additional arguments (not used in the current implementation).
#'
#' @return A modified object of class [`SPATA2`] or [`MeaData`] with the subsetted nodes and edges.
#'
#' @details
#' - **SPATA2 Method**: Delegates the subsetting operation to the `MeaData` object within the `SPATA2` object.
#' - **MeaData Method**:
#'   - When `opt = "keep"`, only the specified node IDs and their associated edges are retained.
#'   - When `opt = "remove"`, the specified node IDs and their associated edges are excluded.
#'
#' @seealso [getMeaData()], [setMeaData()]
#'
#' @rdname subsetMEA
#' @export
setGeneric(name = "subsetMEA", def = function(object, ...){

  standardGeneric(f = "subsetMEA")

})

#' @rdname subsetMEA
#' @export
setMethod(
  f = "subsetMEA",
  signature = "SPATA2",
  definition = function(object, ids, opt = "keep"){

    mea <-
      getMeaData(object) %>%
      subsetMEA(object = ., ids = ids, opt = opt)

    object <- setMeaData(object, mea_data = mea)

    return(object)

  }
)

#' @rdname subsetMEA
#' @export
setMethod(
  f = "subsetMEA",
  signature = "MeaData",
  definition = function(object, ids, opt = "keep"){

    if(opt == "keep"){

      object@nodes <-
        dplyr::filter(object@nodes, ids %in% {{ids}})

      object@edges <-
        dplyr::filter(object@edges, from %in% {{ids}} & to %in% {{ids}})

    } else if(opt == "remove"){

      object@nodes <-
        dplyr::filter(object@nodes, !ids %in% {{ids}})

      object@edges <-
        dplyr::filter(object@edges, !from %in% {{ids}} & !to %in% {{ids}})

    }

    return(object)

  }
)


#' @title Subset data by MEA outline
#'
#' @description Subsets observations in a `SPATA2` or `MeaData` object based on their spatial location
#' relative to the Microelectrode Array (MEA) outline. Observations falling within or outside the
#' outline, optionally expanded by a buffer, are retained.
#'
#' @param object An object of class [`SPATA2`] or [`MeaData`].
#' @param strictly Logical value. If `TRUE`, only observations strictly inside the MEA outline
#' are considered. Default is `TRUE`.
#' @param expand_outline \link[=concept_distance_measures]{Distance measure}. The distance by which to expand the MEA
#' outline. Can be specified in pixels or other distance units. Defaults to half the spacing
#' between electrodes (`eSpacing(object, unit = "px") / 2`).
#'
#' @inherit update_dummy return
#' @inherit SPATA2::argument_dummy params
#'
#' @seealso [getMeaOutline()], [eSpacing()], [buffer_area()]
#'
#' @export
subsetByMeaOutline <- function(object,
                               strictly = TRUE,
                               expand_outline = eSpacing(object, unit = "px")/2,
                               verbose = NULL){

  coords_df <- getCoordsDf(object)
  outline <- getMeaOutline(object)

  if(is_dist(expand_outline)){

    eo <- as_pixel(expand_outline, object = object)

    outline <- buffer_area(outline, buffer = eo)

  }

  bcs_keep <-
    identify_obs_in_polygon(
      coords_df = coords_df,
      polygon_df = outline,
      strictly = strictly
    ) %>%
    dplyr::pull(barcodes)

  object <- subsetSpataObject(object, barcodes = bcs_keep, opt = "keep")

  return(object)

}
