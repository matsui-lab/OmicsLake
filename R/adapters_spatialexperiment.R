#' @title SpatialExperiment Adapter
#' @description Adapter for storing and retrieving SpatialExperiment objects.
#' @details
#' This adapter stores a full-fidelity serialized SpatialExperiment object and
#' manifest.
#' @importFrom R6 R6Class
#' @export
SpatialExperimentAdapter <- .ol_create_serialized_adapter(
  class_name = "SpatialExperimentAdapter",
  adapter_name = "SpatialExperiment",
  token = "spatial",
  priority = 111,
  can_handle_fn = function(data) {
    inherits(data, "SpatialExperiment")
  },
  manifest_type = "SpatialExperiment"
)

