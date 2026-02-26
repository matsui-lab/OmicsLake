#' @title Transcriptomics Adapter
#' @description Adapter for storing and retrieving transcriptomics-layer
#' objects.
#' @details
#' This umbrella adapter is lower priority than specific adapters
#' (e.g. SingleCellExperiment / SummarizedExperiment) and captures
#' transcriptomics-marked objects.
#' @importFrom R6 R6Class
#' @export
TranscriptomicsAdapter <- .ol_create_serialized_adapter(
  class_name = "TranscriptomicsAdapter",
  adapter_name = "Transcriptomics",
  token = "transcript",
  priority = 98,
  can_handle_fn = function(data) {
    inherits(
      data,
      c(
        "TranscriptomicsLayer",
        "DESeqDataSet",
        "DGEList",
        "ExpressionSet"
      )
    ) || identical(attr(data, "omics_layer"), "transcriptomics")
  },
  manifest_type = "Transcriptomics"
)
