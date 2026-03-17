#' @title Proteomics Adapter
#' @description Adapter for storing and retrieving proteomics-layer objects.
#' @details
#' This umbrella adapter is lower priority than specific adapters
#' (e.g. QFeatures / MsExperiment / Spectra) and captures
#' proteomics-marked objects.
#' @importFrom R6 R6Class
#' @export
ProteomicsAdapter <- .ol_create_serialized_adapter(
  class_name = "ProteomicsAdapter",
  adapter_name = "Proteomics",
  token = "proteo",
  priority = 97,
  can_handle_fn = function(data) {
    inherits(data, "ProteomicsLayer") ||
      identical(attr(data, "omics_layer"), "proteomics")
  },
  manifest_type = "Proteomics"
)
