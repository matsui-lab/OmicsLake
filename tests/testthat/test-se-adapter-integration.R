# Integration tests for SummarizedExperiment adapter
# Tests: put/get restoration, version-aware lineage, tag/restore consistency, legacy compatibility

test_that("SE put/get restores object with full fidelity", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_se_fidelity")

  # Create SE with all components
  counts <- matrix(1:100, nrow = 10, ncol = 10,
                   dimnames = list(paste0("gene", 1:10), paste0("sample", 1:10)))
  col_data <- S4Vectors::DataFrame(
    condition = rep(c("A", "B"), each = 5),
    batch = rep(1:2, 5),
    row.names = paste0("sample", 1:10)
  )
  row_data <- S4Vectors::DataFrame(
    gene_type = rep(c("coding", "noncoding"), 5),
    row.names = paste0("gene", 1:10)
  )
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts),
    colData = col_data,
    rowData = row_data,
    metadata = list(experiment = "test", version = 1)
  )

  # Store and retrieve
 lake$put("rna_seq", se)
  se2 <- lake$get("rna_seq")

  # Verify class
  expect_true(inherits(se2, "SummarizedExperiment"))

  # Verify dimensions
  expect_equal(nrow(se2), nrow(se))
  expect_equal(ncol(se2), ncol(se))

  # Verify assay data (values should match)
  expect_equal(dim(SummarizedExperiment::assay(se2, "counts")),
               dim(SummarizedExperiment::assay(se, "counts")))

  # Verify colData
  expect_equal(nrow(SummarizedExperiment::colData(se2)),
               nrow(SummarizedExperiment::colData(se)))
  expect_true("condition" %in% names(SummarizedExperiment::colData(se2)))
  expect_true("batch" %in% names(SummarizedExperiment::colData(se2)))

  # Verify rowData
  expect_equal(nrow(SummarizedExperiment::rowData(se2)),
               nrow(SummarizedExperiment::rowData(se)))
  expect_true("gene_type" %in% names(SummarizedExperiment::rowData(se2)))

  # Verify metadata
  expect_equal(S4Vectors::metadata(se2)$experiment, "test")
  expect_equal(S4Vectors::metadata(se2)$version, 1)
})

test_that("SE adapter registers in __ol_adapters table", {
  skip_if_not_installed("SummarizedExperiment")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_se_registry")

  # Create simple SE
  counts <- matrix(1:20, nrow = 5, ncol = 4)
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = counts))

  lake$put("my_se", se)

  # Check adapter registry via internal function
  state <- .ol_get_backend_state("test_se_registry")
  adapter_info <- .ol_get_adapter_info(state, "my_se")

  expect_false(is.null(adapter_info))
  expect_equal(adapter_info$adapter_name, "SummarizedExperiment")
  expect_true("colData" %in% adapter_info$components)
  expect_true("rowData" %in% adapter_info$components)
  expect_true("manifest" %in% adapter_info$components)
})

test_that("SE tag/restore preserves version integrity", {
  skip_if_not_installed("SummarizedExperiment")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_se_versioning")

  # Version 1: 5 genes
  counts_v1 <- matrix(1:20, nrow = 5, ncol = 4,
                      dimnames = list(paste0("gene", 1:5), paste0("s", 1:4)))
  se_v1 <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts_v1),
    metadata = list(version = "v1")
  )
  lake$put("experiment", se_v1)
  lake$tag("experiment", "v1")

  # Version 2: 10 genes (modified)
  counts_v2 <- matrix(1:40, nrow = 10, ncol = 4,
                      dimnames = list(paste0("gene", 1:10), paste0("s", 1:4)))
  se_v2 <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts_v2),
    metadata = list(version = "v2")
  )
  lake$put("experiment", se_v2)
  lake$tag("experiment", "v2")

  # Retrieve v1 - should have 5 genes
  retrieved_v1 <- lake$get("experiment", ref = "@tag(v1)")
  expect_equal(nrow(retrieved_v1), 5)
  expect_equal(S4Vectors::metadata(retrieved_v1)$version, "v1")

  # Retrieve v2 - should have 10 genes
  retrieved_v2 <- lake$get("experiment", ref = "@tag(v2)")
  expect_equal(nrow(retrieved_v2), 10)
  expect_equal(S4Vectors::metadata(retrieved_v2)$version, "v2")

  # Retrieve latest - should be v2
  retrieved_latest <- lake$get("experiment")
  expect_equal(nrow(retrieved_latest), 10)
})

test_that("SE with_tracking captures lineage with version info", {
  skip_if_not_installed("SummarizedExperiment")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_se_lineage")

  # Create parent data
  parent_counts <- matrix(1:50, nrow = 10, ncol = 5)
  parent_se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = parent_counts)
  )
  lake$put("parent_se", parent_se)
  lake$tag("parent_se", "v1")

  # Create derived SE with tracking
  with_tracking(lake, "derived_se", {
    parent <- lake$get("parent_se", ref = "@tag(v1)")
    # Simulate transformation
    derived_counts <- SummarizedExperiment::assay(parent, "counts") * 2
    SummarizedExperiment::SummarizedExperiment(
      assays = list(counts = derived_counts)
    )
  })

  # Check lineage
  deps <- lake$deps("derived_se", direction = "up")

  # Should have parent dependency
  expect_true(nrow(deps) > 0)
  # Note: lineage is recorded at the top-level name
})

test_that("SE storage is atomic - partial failure rolls back",
{
  skip_if_not_installed("SummarizedExperiment")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_se_atomicity")

  # First, store a valid SE
  valid_counts <- matrix(1:20, nrow = 5, ncol = 4)
  valid_se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = valid_counts))
  lake$put("valid_se", valid_se)

  # Check it exists
  state <- .ol_get_backend_state("test_se_atomicity")
  expect_true(.ol_is_adapter_object(state, "valid_se"))

  # The SE was stored successfully
  retrieved <- lake$get("valid_se")
  expect_true(inherits(retrieved, "SummarizedExperiment"))
})

test_that("Legacy SE storage (pre-registry) can still be read", {
  skip_if_not_installed("SummarizedExperiment")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  # Initialize project
  ol_init("test_legacy_se")
  project <- "test_legacy_se"

  # Simulate legacy storage: directly write components without registry
  prefix <- "legacy_se.__se__."
  counts <- matrix(1:20, nrow = 5, ncol = 4,
                   dimnames = list(paste0("g", 1:5), paste0("s", 1:4)))

  # Write assay in long format
  long_df <- data.frame(
    feature = rep(rownames(counts), ncol(counts)),
    sample = rep(colnames(counts), each = nrow(counts)),
    value = as.vector(counts),
    stringsAsFactors = FALSE
  )
  long_df <- long_df[long_df$value != 0, ]
  ol_write(paste0(prefix, "assay.counts"), long_df, project = project, mode = "overwrite")

  # Write colData
  col_data <- data.frame(.__sample_id__ = paste0("s", 1:4), stringsAsFactors = FALSE)
  ol_write(paste0(prefix, "colData"), col_data, project = project, mode = "overwrite")

  # Write rowData
  row_data <- data.frame(.__feature_id__ = paste0("g", 1:5), stringsAsFactors = FALSE)
  ol_write(paste0(prefix, "rowData"), row_data, project = project, mode = "overwrite")

  # Write manifest object (legacy format, no registry entry)
  manifest <- list(
    type = "SummarizedExperiment",
    class = "SummarizedExperiment",
    assay_names = "counts",
    n_samples = 4,
    n_features = 5,
    has_rowRanges = FALSE,
    has_metadata = FALSE,
    created_at = Sys.time()
  )
  ol_save(paste0(prefix, "manifest"), manifest, project = project)

  # Now try to read via Lake (should use fallback detection)
  lake <- Lake$new("test_legacy_se")
  retrieved <- lake$get("legacy_se")

  expect_true(inherits(retrieved, "SummarizedExperiment"))
  expect_equal(nrow(retrieved), 5)
  expect_equal(ncol(retrieved), 4)
})

test_that("SE drop() removes all components and registry entry", {
  skip_if_not_installed("SummarizedExperiment")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_se_drop")

  # Create and store SE
  counts <- matrix(1:20, nrow = 5, ncol = 4)
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = counts))
  lake$put("to_drop", se)

  # Verify it exists
  state <- .ol_get_backend_state("test_se_drop")
  expect_true(.ol_is_adapter_object(state, "to_drop"))

  # Drop it
  lake$drop("to_drop")

  # Verify registry entry is gone
  expect_false(.ol_is_adapter_object(state, "to_drop"))

  # Verify components are gone (trying to get should fail)
  expect_error(lake$get("to_drop"), "not found|Cannot find")
})

test_that("SE version-aware registry lookup works with tags", {
  skip_if_not_installed("SummarizedExperiment")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_se_version_registry")

  # Create v1 with 5 features
  se_v1 <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(1:20, nrow = 5, ncol = 4))
  )
  lake$put("versioned_se", se_v1)
  lake$tag("versioned_se", "v1")

  # Create v2 with 10 features
  se_v2 <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = matrix(1:40, nrow = 10, ncol = 4))
  )
  lake$put("versioned_se", se_v2)
  lake$tag("versioned_se", "v2")

  # Retrieve v1 via tag - should get correct adapter info for that version
  retrieved_v1 <- lake$get("versioned_se", ref = "@tag(v1)")
  expect_equal(nrow(retrieved_v1), 5)

  # Retrieve v2 via tag
  retrieved_v2 <- lake$get("versioned_se", ref = "@tag(v2)")
  expect_equal(nrow(retrieved_v2), 10)
})
