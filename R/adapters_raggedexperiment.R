#' @title RaggedExperiment Adapter
#' @description Adapter for storing and retrieving RaggedExperiment objects.
#' @details
#' This adapter stores a full-fidelity serialized RaggedExperiment object and
#' manifest.
#' @importFrom R6 R6Class
#' @export
RaggedExperimentAdapter <- .ol_create_serialized_adapter(
  class_name = "RaggedExperimentAdapter",
  adapter_name = "RaggedExperiment",
  token = "ragged",
  priority = 109,
  can_handle_fn = function(data) {
    inherits(data, "RaggedExperiment")
  },
  manifest_type = "RaggedExperiment"
)

