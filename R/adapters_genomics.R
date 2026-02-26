#' @title Genomics Adapter
#' @description Adapter for storing and retrieving genomics-layer objects.
#' @details
#' This umbrella adapter is lower priority than specific adapters
#' (e.g. VCF / RaggedExperiment) and captures genomics-marked objects.
#' @importFrom R6 R6Class
#' @export
GenomicsAdapter <- .ol_create_serialized_adapter(
  class_name = "GenomicsAdapter",
  adapter_name = "Genomics",
  token = "genomics",
  priority = 96,
  can_handle_fn = function(data) {
    inherits(data, "GenomicsLayer") ||
      identical(attr(data, "omics_layer"), "genomics")
  },
  manifest_type = "Genomics"
)
