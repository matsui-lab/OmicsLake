# Tests for newly added omics layer adapters

test_that("built-in omics layer adapters are autoloaded", {
  clear_adapters()
  .adapter_registry$autoload_attempted <- FALSE

  adapters <- get_adapters()
  adapter_names <- names(adapters)

  expect_true("Seurat" %in% adapter_names)
  expect_true("SpatialExperiment" %in% adapter_names)
  expect_true("RaggedExperiment" %in% adapter_names)
  expect_true("VCF" %in% adapter_names)
  expect_true("Methylation" %in% adapter_names)
  expect_true("ATAC" %in% adapter_names)
  expect_true("ChIP" %in% adapter_names)
  expect_true("Metabolomics" %in% adapter_names)
  expect_true("Lipidomics" %in% adapter_names)
  expect_true("Glycomics" %in% adapter_names)
  expect_true("Phosphoproteomics" %in% adapter_names)
  expect_true("Transcriptomics" %in% adapter_names)
  expect_true("Proteomics" %in% adapter_names)
  expect_true("Genomics" %in% adapter_names)
  expect_true("Epigenomics" %in% adapter_names)
})

test_that("omics layer adapters support put/get/tag with deterministic registry", {
  clear_adapters()
  .adapter_registry$autoload_attempted <- FALSE

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_omics_layer_adapters")
  state <- .ol_get_backend_state("test_omics_layer_adapters")

  cases <- list(
    list(name = "seurat_case", obj = structure(list(x = 1L), class = "Seurat"), adapter = "Seurat"),
    list(name = "spatial_case", obj = structure(list(x = 2L), class = c("SpatialExperiment", "SingleCellExperiment")), adapter = "SpatialExperiment"),
    list(name = "ragged_case", obj = structure(list(x = 3L), class = "RaggedExperiment"), adapter = "RaggedExperiment"),
    list(name = "vcf_case", obj = structure(list(x = 4L), class = "VCF"), adapter = "VCF"),
    list(name = "methyl_case", obj = structure(list(x = 5L), class = "GenomicRatioSet"), adapter = "Methylation"),
    list(name = "atac_case", obj = structure(list(x = 6L), class = "ATACLayer"), adapter = "ATAC"),
    list(name = "chip_case", obj = structure(list(x = 7L), class = "ChIPLayer"), adapter = "ChIP"),
    list(name = "metabol_case", obj = structure(list(x = 8L), class = "MetabolomicsLayer"), adapter = "Metabolomics"),
    list(name = "lipid_case", obj = structure(list(x = 9L), class = "LipidomicsLayer"), adapter = "Lipidomics"),
    list(name = "glyco_case", obj = structure(list(x = 10L), class = "GlycomicsLayer"), adapter = "Glycomics"),
    list(name = "phospho_case", obj = structure(list(x = 11L), class = "PhosphoproteomicsLayer"), adapter = "Phosphoproteomics"),
    list(name = "transcript_case", obj = structure(list(x = 12L), class = "TranscriptomicsLayer"), adapter = "Transcriptomics"),
    list(name = "proteomics_case", obj = structure(list(x = 13L), class = "ProteomicsLayer"), adapter = "Proteomics"),
    list(name = "genomics_case", obj = structure(list(x = 14L), class = "GenomicsLayer"), adapter = "Genomics"),
    list(name = "epigenomics_case", obj = structure(list(x = 15L), class = "EpigenomicsLayer"), adapter = "Epigenomics")
  )

  for (cs in cases) {
    lake$put(cs$name, cs$obj)

    info <- .ol_get_adapter_info(state, cs$name)
    expect_false(is.null(info))
    expect_equal(info$adapter_name, cs$adapter)

    restored <- lake$get(cs$name)
    expect_true(inherits(restored, class(cs$obj)[1]))
    expect_equal(restored$x, cs$obj$x)

    lake$tag(cs$name, "v1")
    restored_tag <- lake$get(cs$name, ref = "@tag(v1)")
    expect_equal(restored_tag$x, cs$obj$x)
  }
})

test_that("serialized omics adapters materialize table-like sidecar components", {
  clear_adapters()
  .adapter_registry$autoload_attempted <- FALSE

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_omics_layer_sidecars")
  obj <- structure(
    list(
      meta = data.frame(sample = c("s1", "s2"), condition = c("A", "B"), stringsAsFactors = FALSE),
      abund = matrix(c(10, 20, 30, 40), nrow = 2, dimnames = list(c("f1", "f2"), c("s1", "s2"))),
      score = c(s1 = 0.1, s2 = 0.2)
    ),
    class = "MetabolomicsLayer"
  )

  lake$put("metab_sidecar", obj)
  tables <- lake$tables()$table_name
  expect_true(any(grepl("^metab_sidecar\\.__metabol__\\.part\\.", tables)))

  restored <- lake$get("metab_sidecar")
  expect_true(inherits(restored, "MetabolomicsLayer"))
  expect_equal(restored$abund[1, 1], 10)
})

test_that("sidecar extraction traverses nested list payloads", {
  clear_adapters()
  .adapter_registry$autoload_attempted <- FALSE

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_omics_layer_nested_sidecars")
  obj <- structure(
    list(
      nested = list(
        layer1 = list(
          summary = data.frame(feature = c("f1", "f2"), score = c(1.2, 0.8), stringsAsFactors = FALSE),
          profile = matrix(1:4, nrow = 2)
        )
      )
    ),
    class = "ProteomicsLayer"
  )

  lake$put("prot_nested", obj)
  tables <- lake$tables()$table_name
  expect_true(any(grepl("^prot_nested\\.__proteo__\\.part\\.", tables)))

  restored <- lake$get("prot_nested")
  expect_true(inherits(restored, "ProteomicsLayer"))
  expect_equal(restored$nested$layer1$profile[1, 1], 1)
})
