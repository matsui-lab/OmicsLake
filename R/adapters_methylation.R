#' @title Methylation Adapter
#' @description Adapter for storing and retrieving methylation-layer objects.
#' @details
#' Supports common Bioconductor methylation classes (e.g. minfi family) by
#' storing a full serialized object and manifest.
#' @importFrom R6 R6Class
#' @export
MethylationAdapter <- .ol_create_serialized_adapter(
  class_name = "MethylationAdapter",
  adapter_name = "Methylation",
  token = "methyl",
  priority = 107,
  can_handle_fn = function(data) {
    inherits(
      data,
      c(
        "GenomicRatioSet", "MethylSet", "RatioSet",
        "GenomicMethylSet", "RGChannelSet", "MethylationLayer"
      )
    ) || identical(attr(data, "omics_layer"), "methylation")
  },
  manifest_type = "Methylation"
)

