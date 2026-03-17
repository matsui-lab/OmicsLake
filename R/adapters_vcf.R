#' @title VCF Adapter
#' @description Adapter for storing and retrieving VariantAnnotation VCF objects.
#' @details
#' This adapter stores a full-fidelity serialized VCF object and manifest.
#' @importFrom R6 R6Class
#' @export
VCFAdapter <- .ol_create_serialized_adapter(
  class_name = "VCFAdapter",
  adapter_name = "VCF",
  token = "vcf",
  priority = 108,
  can_handle_fn = function(data) {
    inherits(data, c("VCF", "CollapsedVCF", "ExpandedVCF"))
  },
  manifest_type = "VCF"
)

