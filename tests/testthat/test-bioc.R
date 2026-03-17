test_that("ol_fread reads table with filtering", {
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(
    feature = c("gene1", "gene2", "gene3"),
    value = c(10, 20, 30),
    category = c("A", "B", "A")
  )
  
  ol_write("test_table", test_data)
  
  result <- ol_fread("test_table", select = c("feature", "value"))
  expect_equal(ncol(result), 2)
  
  result <- ol_fread("test_table", nrows = 2)
  expect_equal(nrow(result), 2)
})

test_that("ol_read_se creates SummarizedExperiment", {
  skip_if_not_installed("SummarizedExperiment")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(
    feature = rep(c("gene1", "gene2", "gene3"), each = 2),
    sample = rep(c("sample1", "sample2"), 3),
    value = c(10, 15, 20, 25, 30, 35)
  )
  
  ol_write("test_counts", test_data)
  
  se <- ol_read_se("test_counts", backing = "memory")
  
  expect_s4_class(se, "SummarizedExperiment")
  expect_equal(nrow(se), 3)
  expect_equal(ncol(se), 2)
})

test_that("ol_read_se handles custom column names", {
  skip_if_not_installed("SummarizedExperiment")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  test_data <- data.frame(
    gene_id = rep(c("A", "B"), each = 2),
    sample_id = rep(c("S1", "S2"), 2),
    expression = c(1, 2, 3, 4)
  )
  
  ol_write("custom_table", test_data)
  
  se <- ol_read_se(
    "custom_table",
    feature_col = "gene_id",
    sample_col = "sample_id",
    value_col = "expression",
    backing = "memory"
  )
  
  expect_s4_class(se, "SummarizedExperiment")
  expect_equal(nrow(se), 2)
  expect_equal(ncol(se), 2)
})

test_that("ol_read_mae creates MultiAssayExperiment", {
  skip_if_not_installed("MultiAssayExperiment")
  skip_if_not_installed("SummarizedExperiment")
  
  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  
  options(ol.root = tmpdir)
  ol_init("test_project")
  
  rna_data <- data.frame(
    feature = rep(c("gene1", "gene2"), each = 2),
    sample = rep(c("S1", "S2"), 2),
    value = c(10, 20, 30, 40)
  )
  
  protein_data <- data.frame(
    feature = rep(c("prot1", "prot2"), each = 2),
    sample = rep(c("S1", "S2"), 2),
    value = c(5, 15, 25, 35)
  )
  
  ol_write("rna", rna_data)
  ol_write("protein", protein_data)
  
  assays_config <- list(
    rna = list(name = "rna"),
    protein = list(name = "protein")
  )
  
  mae <- ol_read_mae(assays_config, backing = "memory")
  
  expect_s4_class(mae, "MultiAssayExperiment")
  expect_equal(length(mae), 2)
})

test_that("Lake handles MultiAssayExperiment via adapter", {
  skip_if_not_installed("MultiAssayExperiment")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("S4Vectors")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_mae_adapter", root = tmpdir)

  counts <- matrix(
    c(1, 0, 3, 4, 5, 0, 7, 8, 9),
    nrow = 3,
    ncol = 3,
    dimnames = list(c("g1", "g2", "g3"), c("c1", "c2", "c3"))
  )
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts),
    colData = S4Vectors::DataFrame(
      batch = c("A", "A", "B"),
      row.names = c("c1", "c2", "c3")
    )
  )
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts + 10),
    colData = S4Vectors::DataFrame(
      batch = c("A", "A", "B"),
      row.names = c("c1", "c2", "c3")
    )
  )
  SingleCellExperiment::reducedDim(sce, "PCA") <- matrix(
    c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    ncol = 2,
    dimnames = list(c("c1", "c2", "c3"), c("PC1", "PC2"))
  )

  col_data <- S4Vectors::DataFrame(
    age = c(60L, 72L, 68L),
    clinical = I(list(c("A", "B"), "C", c("D", "E"))),
    row.names = c("p1", "p2", "p3")
  )
  sample_map <- S4Vectors::DataFrame(
    assay = factor(c("rna", "rna", "rna", "scrna", "scrna", "scrna"),
      levels = c("rna", "scrna")),
    primary = c("p1", "p2", "p3", "p1", "p2", "p3"),
    colname = c("c1", "c2", "c3", "c1", "c2", "c3")
  )
  mae <- MultiAssayExperiment::MultiAssayExperiment(
    experiments = list(rna = se, scrna = sce),
    colData = col_data,
    sampleMap = sample_map
  )
  MultiAssayExperiment::drops(mae) <- list(rna = "unused_col")
  S4Vectors::metadata(mae)$snapshot <- "v1"

  lake$put("mae_obj", mae)
  stored_objects <- lake$objects()$name
  stored_tables <- lake$tables()$table_name

  expect_true("MultiAssayExperiment" %in% names(get_adapters()))
  expect_true(any(grepl("^mae_obj\\.__mae__\\.manifest$", stored_objects)))
  expect_true(any(grepl(
    "^mae_obj\\.__mae__\\.experiment\\.exp[0-9]+\\.__se__\\.manifest$",
    stored_objects
  )))
  expect_true(any(grepl(
    "^mae_obj\\.__mae__\\.experiment\\.exp[0-9]+\\.__sce__\\.manifest$",
    stored_objects
  )))
  expect_true(any(grepl("^mae_obj\\.__mae__\\.colData\\.object$",
    stored_objects)))
  expect_true(any(grepl("^mae_obj\\.__mae__\\.", stored_tables)))
  expect_false("mae_obj" %in% stored_objects)

  restored <- lake$get("mae_obj")
  found <- lake$find("mae_obj", type = "object")
  expect_true(nrow(found) >= 1)
  expect_equal(found$name[[1]], "mae_obj")
  all_objects <- lake$find(type = "object")
  expect_false(any(grepl("\\.__(se|sce|mae|spectra|qfeatures|mse|xcms|chrom|seurat)__\\.",
    all_objects$name,
    perl = TRUE)))

  expect_s4_class(restored, "MultiAssayExperiment")
  restored_experiments <- MultiAssayExperiment::experiments(restored)
  expect_equal(names(restored_experiments), c("rna", "scrna"))
  expect_s4_class(restored_experiments[["rna"]], "SummarizedExperiment")
  expect_s4_class(restored_experiments[["scrna"]], "SingleCellExperiment")
  expect_true("PCA" %in% names(SingleCellExperiment::reducedDims(
    restored_experiments[["scrna"]]
  )))

  normalize_sample_map <- function(x) {
    df <- as.data.frame(x)[, c("assay", "primary", "colname"), drop = FALSE]
    df[] <- lapply(df, as.character)
    df[order(df$assay, df$primary, df$colname), , drop = FALSE]
  }
  expect_equal(
    normalize_sample_map(MultiAssayExperiment::sampleMap(restored)),
    normalize_sample_map(sample_map)
  )

  col_data_in <- as.data.frame(MultiAssayExperiment::colData(mae))
  col_data_out <- as.data.frame(MultiAssayExperiment::colData(restored))
  expect_equal(rownames(col_data_out), rownames(col_data_in))
  expect_equal(as.integer(col_data_out$age), as.integer(col_data_in$age))
  expect_true(is.list(col_data_out$clinical))
  expect_equal(col_data_out$clinical, col_data_in$clinical)

  expect_equal(S4Vectors::metadata(restored)$snapshot, "v1")
  drops_in <- MultiAssayExperiment::drops(mae)
  drops_out <- MultiAssayExperiment::drops(restored)
  expect_equal(names(drops_out), names(drops_in))
  expect_equal(
    as.character(unlist(drops_out, use.names = TRUE)),
    as.character(unlist(drops_in, use.names = TRUE))
  )

  lake$tag("mae_obj", "v1")
  mae_v2 <- mae
  S4Vectors::metadata(mae_v2)$snapshot <- "v2"
  lake$put("mae_obj", mae_v2)

  latest <- lake$get("mae_obj")
  tagged <- lake$get("mae_obj", ref = "@tag(v1)")
  expect_equal(S4Vectors::metadata(latest)$snapshot, "v2")
  expect_equal(S4Vectors::metadata(tagged)$snapshot, "v1")

  lake$drop("mae_obj")
  expect_false(lake$exists("mae_obj"))
  expect_false(any(grepl("^mae_obj\\.__mae__\\.", lake$tables()$table_name)))
  expect_false(any(grepl("^mae_obj\\.__mae__\\.", lake$objects()$name)))
})

test_that("Lake handles Spectra via adapter for proteomics/metabolomics", {
  skip_if_not_installed("Spectra")
  skip_if_not_installed("S4Vectors")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_spectra_adapter", root = tmpdir)

  make_spectra <- function(spectra_data, peaks_list) {
    df <- as.data.frame(spectra_data)
    df$mz <- I(lapply(peaks_list, function(x) as.numeric(x[, "mz"])))
    df$intensity <- I(lapply(peaks_list, function(x) as.numeric(
      x[, "intensity"]
    )))
    backend <- Spectra::backendInitialize(Spectra::MsBackendMemory(),
      data = df)
    Spectra::Spectra(backend)
  }

  peaks1 <- cbind(mz = c(100.1, 101.5), intensity = c(1000, 320))
  peaks2 <- cbind(mz = c(200.2, 201.7, 205.4), intensity = c(1500, 640, 120))
  sp_prot <- make_spectra(
    spectra_data = S4Vectors::DataFrame(
      msLevel = c(2L, 2L),
      rtime = c(120.5, 125.0),
      precursorMz = c(500.2, 620.3),
      sample = c("protA", "protB"),
      annotations = I(list(c("PEPTIDEA", "PEPTIDEB"), "PEPTIDEC"))
    ),
    peaks_list = list(peaks1, peaks2)
  )

  lake$put("proteomics_ms", sp_prot)
  stored_objects <- lake$objects()$name
  stored_tables <- lake$tables()$table_name

  expect_true("Spectra" %in% names(get_adapters()))
  expect_true(any(grepl("^proteomics_ms\\.__spectra__\\.manifest$",
    stored_objects)))
  expect_true(any(grepl("^proteomics_ms\\.__spectra__\\.peaksData$",
    stored_objects)))
  expect_true(any(grepl("^proteomics_ms\\.__spectra__\\.spectraData\\.object$",
    stored_objects)))
  expect_false(any(grepl("^proteomics_ms\\.__spectra__\\.spectraData$",
    stored_tables)))
  expect_false("proteomics_ms" %in% stored_objects)

  restored <- lake$get("proteomics_ms")
  expect_s4_class(restored, "Spectra")
  expect_equal(length(restored), length(sp_prot))
  expect_equal(Spectra::peaksData(restored), Spectra::peaksData(sp_prot))

  sd_in <- as.data.frame(Spectra::spectraData(sp_prot))
  sd_out <- as.data.frame(Spectra::spectraData(restored))
  expect_equal(as.integer(sd_out$msLevel), as.integer(sd_in$msLevel))
  expect_equal(as.numeric(sd_out$rtime), as.numeric(sd_in$rtime))
  expect_equal(as.numeric(sd_out$precursorMz), as.numeric(sd_in$precursorMz))
  expect_equal(as.character(sd_out$sample), as.character(sd_in$sample))
  expect_true(is.list(sd_out$annotations))
  expect_equal(sd_out$annotations, sd_in$annotations)

  found <- lake$find("proteomics_ms", type = "object")
  expect_true(nrow(found) >= 1)
  expect_equal(found$name[[1]], "proteomics_ms")
  all_objects <- lake$find(type = "object")
  expect_false(any(grepl("\\.__(se|sce|mae|spectra|qfeatures|mse|xcms|chrom|seurat)__\\.",
    all_objects$name, perl = TRUE)))

  lake$tag("proteomics_ms", "v1")
  prot_v2_data <- as.data.frame(Spectra::spectraData(sp_prot))
  prot_v2_data$sample <- c("protA_v2", "protB_v2")
  sp_prot_v2 <- make_spectra(
    spectra_data = S4Vectors::DataFrame(prot_v2_data),
    peaks_list = Spectra::peaksData(sp_prot)
  )
  lake$put("proteomics_ms", sp_prot_v2)

  latest <- lake$get("proteomics_ms")
  tagged <- lake$get("proteomics_ms", ref = "@tag(v1)")
  latest_sd <- as.data.frame(Spectra::spectraData(latest))
  tagged_sd <- as.data.frame(Spectra::spectraData(tagged))
  expect_equal(as.character(latest_sd$sample), c("protA_v2", "protB_v2"))
  expect_equal(as.character(tagged_sd$sample), c("protA", "protB"))

  peaks3 <- cbind(mz = c(300.3, 301.8), intensity = c(2100, 980))
  sp_met <- make_spectra(
    spectra_data = S4Vectors::DataFrame(
      msLevel = 1L,
      rtime = 210.0,
      sample = "metA",
      polarity = 1L
    ),
    peaks_list = list(peaks3)
  )

  lake$put("metabolomics_ms", sp_met)
  stored_tables <- lake$tables()$table_name
  expect_true(any(grepl("^metabolomics_ms\\.__spectra__\\.spectraData$",
    stored_tables)))
  met_restored <- lake$get("metabolomics_ms")
  expect_s4_class(met_restored, "Spectra")
  expect_equal(Spectra::peaksData(met_restored), Spectra::peaksData(sp_met))
  met_sd_in <- as.data.frame(Spectra::spectraData(sp_met))
  met_sd_out <- as.data.frame(Spectra::spectraData(met_restored))
  expect_equal(as.integer(met_sd_out$msLevel), as.integer(met_sd_in$msLevel))
  expect_equal(as.numeric(met_sd_out$rtime), as.numeric(met_sd_in$rtime))
  expect_equal(as.character(met_sd_out$sample), as.character(met_sd_in$sample))
  expect_equal(as.integer(met_sd_out$polarity), as.integer(met_sd_in$polarity))

  lake$drop("proteomics_ms")
  lake$drop("metabolomics_ms")
  expect_false(lake$exists("proteomics_ms"))
  expect_false(lake$exists("metabolomics_ms"))
  expect_false(any(grepl("^proteomics_ms\\.__spectra__\\.",
    lake$tables()$table_name)))
  expect_false(any(grepl("^proteomics_ms\\.__spectra__\\.",
    lake$objects()$name)))
  expect_false(any(grepl("^metabolomics_ms\\.__spectra__\\.",
    lake$tables()$table_name)))
  expect_false(any(grepl("^metabolomics_ms\\.__spectra__\\.",
    lake$objects()$name)))
})

test_that("Lake handles QFeatures via adapter", {
  skip_if_not_installed("QFeatures")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("MultiAssayExperiment")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_qfeatures_adapter", root = tmpdir)

  mat_psm <- matrix(
    c(10, 12, 5, 7),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("pep1", "pep2"), c("s1", "s2"))
  )
  mat_pep <- matrix(
    c(8, 9, 3, 4),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("prot1", "prot2"), c("s1", "s2"))
  )
  se_psm <- SummarizedExperiment::SummarizedExperiment(
    assays = list(intensity = mat_psm),
    rowData = S4Vectors::DataFrame(id = c("id1", "id2"),
      row.names = c("pep1", "pep2"))
  )
  se_pep <- SummarizedExperiment::SummarizedExperiment(
    assays = list(intensity = mat_pep),
    rowData = S4Vectors::DataFrame(id = c("id1", "id2"),
      row.names = c("prot1", "prot2"))
  )
  col_data <- S4Vectors::DataFrame(condition = c("A", "B"),
    row.names = c("s1", "s2"))
  qf <- QFeatures::QFeatures(
    assays = list(psms = se_psm, peptides = se_pep),
    colData = col_data
  )
  qf <- tryCatch(
    QFeatures::addAssayLinkOneToOne(qf, from = "psms", to = "peptides",
      varFrom = "id", varTo = "id"),
    error = function(e) qf
  )

  lake$put("qf_obj", qf)
  stored_objects <- lake$objects()$name
  expect_true("QFeatures" %in% names(get_adapters()))
  expect_true(any(grepl("^qf_obj\\.__qfeatures__\\.manifest$", stored_objects)))
  expect_true(any(grepl("^qf_obj\\.__qfeatures__\\.mae\\.__mae__\\.manifest$",
    stored_objects)))
  expect_false("qf_obj" %in% stored_objects)

  restored <- lake$get("qf_obj")
  expect_s4_class(restored, "QFeatures")
  exp_names <- names(MultiAssayExperiment::experiments(restored))
  expect_equal(exp_names, c("psms", "peptides"))
  expect_equal(
    SummarizedExperiment::assay(restored[["psms"]], "intensity"),
    SummarizedExperiment::assay(qf[["psms"]], "intensity")
  )

  lake$tag("qf_obj", "v1")
  mat_psm_v2 <- mat_psm
  mat_psm_v2[1, 1] <- mat_psm_v2[1, 1] + 100
  se_psm_v2 <- SummarizedExperiment::SummarizedExperiment(
    assays = list(intensity = mat_psm_v2),
    rowData = S4Vectors::DataFrame(id = c("id1", "id2"),
      row.names = c("pep1", "pep2"))
  )
  qf_v2 <- QFeatures::QFeatures(
    assays = list(psms = se_psm_v2, peptides = se_pep),
    colData = col_data
  )
  lake$put("qf_obj", qf_v2)

  latest <- lake$get("qf_obj")
  tagged <- lake$get("qf_obj", ref = "@tag(v1)")
  expect_equal(
    SummarizedExperiment::assay(latest[["psms"]], "intensity")[1, 1],
    mat_psm_v2[1, 1]
  )
  expect_equal(
    SummarizedExperiment::assay(tagged[["psms"]], "intensity")[1, 1],
    mat_psm[1, 1]
  )

  lake$drop("qf_obj")
  expect_false(lake$exists("qf_obj"))
  expect_false(any(grepl("^qf_obj\\.__qfeatures__\\.", lake$tables()$table_name)))
  expect_false(any(grepl("^qf_obj\\.__qfeatures__\\.", lake$objects()$name)))
})

test_that("Lake handles MsExperiment via adapter", {
  skip_if_not_installed("MsExperiment")
  skip_if_not_installed("Spectra")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_msexperiment_adapter", root = tmpdir)

  make_spectra <- function(spectra_data, peaks_list) {
    df <- as.data.frame(spectra_data)
    df$mz <- I(lapply(peaks_list, function(x) as.numeric(x[, "mz"])))
    df$intensity <- I(lapply(peaks_list, function(x) as.numeric(
      x[, "intensity"]
    )))
    backend <- Spectra::backendInitialize(Spectra::MsBackendMemory(),
      data = df)
    Spectra::Spectra(backend)
  }

  peaks1 <- cbind(mz = c(100.1, 101.5), intensity = c(1000, 320))
  peaks2 <- cbind(mz = c(200.2, 201.7), intensity = c(1500, 640))
  sp <- make_spectra(
    spectra_data = S4Vectors::DataFrame(
      msLevel = c(2L, 2L),
      rtime = c(100.0, 110.0),
      sample = c("s1", "s2")
    ),
    peaks_list = list(peaks1, peaks2)
  )
  q_mat <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("f1", "f2"), c("s1", "s2"))
  )
  q_se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(intensity = q_mat)
  )
  sample_df <- S4Vectors::DataFrame(batch = c("B1", "B2"),
    row.names = c("s1", "s2"))
  mse <- MsExperiment::MsExperiment(
    experimentFiles = MsExperiment::MsExperimentFiles(),
    otherData = S4Vectors::List(source = "demo"),
    qdata = q_se,
    sampleData = sample_df,
    spectra = sp
  )

  lake$put("mse_obj", mse)
  stored_objects <- lake$objects()$name
  expect_true("MsExperiment" %in% names(get_adapters()))
  expect_true(any(grepl("^mse_obj\\.__mse__\\.manifest$", stored_objects)))
  expect_true(any(grepl("^mse_obj\\.__mse__\\.spectra\\.__spectra__\\.manifest$",
    stored_objects)))
  expect_false("mse_obj" %in% stored_objects)

  restored <- lake$get("mse_obj")
  expect_s4_class(restored, "MsExperiment")
  expect_equal(rownames(as.data.frame(MsExperiment::sampleData(restored))),
    c("s1", "s2"))
  restored_sp <- MsExperiment::spectra(restored)
  expect_s4_class(restored_sp, "Spectra")
  expect_equal(Spectra::peaksData(restored_sp), Spectra::peaksData(sp))
  restored_q <- MsExperiment::qdata(restored)
  expect_s4_class(restored_q, "SummarizedExperiment")
  expect_equal(
    SummarizedExperiment::assay(restored_q, "intensity"),
    SummarizedExperiment::assay(q_se, "intensity")
  )

  lake$tag("mse_obj", "v1")
  sample_df_v2 <- sample_df
  sample_df_v2$batch <- c("C1", "C2")
  mse_v2 <- MsExperiment::MsExperiment(
    experimentFiles = MsExperiment::MsExperimentFiles(),
    otherData = S4Vectors::List(source = "demo"),
    qdata = q_se,
    sampleData = sample_df_v2,
    spectra = sp
  )
  lake$put("mse_obj", mse_v2)
  latest <- lake$get("mse_obj")
  tagged <- lake$get("mse_obj", ref = "@tag(v1)")
  expect_equal(as.character(MsExperiment::sampleData(latest)$batch),
    c("C1", "C2"))
  expect_equal(as.character(MsExperiment::sampleData(tagged)$batch),
    c("B1", "B2"))

  lake$drop("mse_obj")
  expect_false(lake$exists("mse_obj"))
  expect_false(any(grepl("^mse_obj\\.__mse__\\.", lake$tables()$table_name)))
  expect_false(any(grepl("^mse_obj\\.__mse__\\.", lake$objects()$name)))
})

test_that("Lake handles XCMS-like objects via adapter fallback", {
  if (requireNamespace("xcms", quietly = TRUE)) {
    skip("xcms is installed; fallback-only mock test is not applicable")
  }

  if (methods::isClass("XCMSnExp")) {
    skip("XCMSnExp class already defined in this session")
  }
  methods::setClass("XCMSnExp", slots = c(payload = "list"))

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)
  lake <- Lake$new("test_xcms_adapter_fallback", root = tmpdir)

  xcms_obj <- methods::new("XCMSnExp", payload = list(
    chrom_peaks = data.frame(mz = c(100.1, 150.2), rt = c(30.2, 31.5)),
    params = list(method = "centWave", ppm = 15L)
  ))

  lake$put("xcms_obj", xcms_obj)
  stored_objects <- lake$objects()$name
  expect_true("xcms" %in% names(get_adapters()))
  expect_true(any(grepl("^xcms_obj\\.__xcms__\\.manifest$", stored_objects)))
  expect_true(any(grepl("^xcms_obj\\.__xcms__\\.object$", stored_objects)))
  expect_false("xcms_obj" %in% stored_objects)

  restored <- lake$get("xcms_obj")
  expect_s4_class(restored, "XCMSnExp")
  expect_equal(restored@payload, xcms_obj@payload)

  lake$tag("xcms_obj", "v1")
  xcms_obj_v2 <- methods::new("XCMSnExp", payload = list(
    chrom_peaks = data.frame(mz = c(100.1, 150.2), rt = c(30.2, 31.5)),
    params = list(method = "centWave", ppm = 20L)
  ))
  lake$put("xcms_obj", xcms_obj_v2)
  latest <- lake$get("xcms_obj")
  tagged <- lake$get("xcms_obj", ref = "@tag(v1)")
  expect_equal(latest@payload$params$ppm, 20L)
  expect_equal(tagged@payload$params$ppm, 15L)

  found <- lake$find("xcms_obj", type = "object")
  expect_true(nrow(found) >= 1)
  expect_equal(found$name[[1]], "xcms_obj")
  all_objects <- lake$find(type = "object")
  expect_false(any(grepl("\\.__(se|sce|mae|spectra|qfeatures|mse|xcms|chrom|seurat)__\\.",
    all_objects$name, perl = TRUE)))

  lake$drop("xcms_obj")
  expect_false(lake$exists("xcms_obj"))
  expect_false(any(grepl("^xcms_obj\\.__xcms__\\.", lake$tables()$table_name)))
  expect_false(any(grepl("^xcms_obj\\.__xcms__\\.", lake$objects()$name)))
})

test_that("Lake handles Chromatograms-like objects via adapter fallback", {
  if (requireNamespace("Chromatograms", quietly = TRUE)) {
    skip("Chromatograms is installed; fallback-only mock test is not applicable")
  }

  if (methods::isClass("Chromatograms")) {
    skip("Chromatograms class already defined in this session")
  }
  methods::setClass("Chromatograms", slots = c(chroms = "list", meta = "list"))

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)
  lake <- Lake$new("test_chrom_adapter_fallback", root = tmpdir)

  chrom_obj <- methods::new("Chromatograms",
    chroms = list(
      data.frame(rt = c(1, 2, 3), intensity = c(10, 20, 15)),
      data.frame(rt = c(1, 2, 3), intensity = c(5, 8, 7))
    ),
    meta = list(mode = "positive", n = 2L)
  )

  lake$put("chrom_obj", chrom_obj)
  stored_objects <- lake$objects()$name
  expect_true("Chromatograms" %in% names(get_adapters()))
  expect_true(any(grepl("^chrom_obj\\.__chrom__\\.manifest$", stored_objects)))
  expect_true(any(grepl("^chrom_obj\\.__chrom__\\.object$", stored_objects)))
  expect_false("chrom_obj" %in% stored_objects)

  restored <- lake$get("chrom_obj")
  expect_s4_class(restored, "Chromatograms")
  expect_equal(restored@meta, chrom_obj@meta)
  expect_equal(restored@chroms, chrom_obj@chroms)

  lake$tag("chrom_obj", "v1")
  chrom_obj_v2 <- methods::new("Chromatograms",
    chroms = chrom_obj@chroms,
    meta = list(mode = "negative", n = 2L)
  )
  lake$put("chrom_obj", chrom_obj_v2)
  latest <- lake$get("chrom_obj")
  tagged <- lake$get("chrom_obj", ref = "@tag(v1)")
  expect_equal(latest@meta$mode, "negative")
  expect_equal(tagged@meta$mode, "positive")

  found <- lake$find("chrom_obj", type = "object")
  expect_true(nrow(found) >= 1)
  expect_equal(found$name[[1]], "chrom_obj")

  lake$drop("chrom_obj")
  expect_false(lake$exists("chrom_obj"))
  expect_false(any(grepl("^chrom_obj\\.__chrom__\\.", lake$tables()$table_name)))
  expect_false(any(grepl("^chrom_obj\\.__chrom__\\.", lake$objects()$name)))
})

test_that("SE adapter preserves rowRanges for RangedSummarizedExperiment", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("GenomicRanges")
  skip_if_not_installed("IRanges")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_ranged_se_adapter", root = tmpdir)

  counts <- matrix(
    c(1, 0, 3, 4, 5, 0),
    nrow = 3,
    ncol = 2,
    dimnames = list(c("g1", "g2", "g3"), c("s1", "s2"))
  )
  rr <- GenomicRanges::GRanges(
    seqnames = c("chr1", "chr1", "chr2"),
    ranges = IRanges::IRanges(start = c(1L, 101L, 201L), width = 10L),
    strand = c("+", "-", "+")
  )
  names(rr) <- rownames(counts)

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts),
    colData = S4Vectors::DataFrame(batch = c("A", "B"),
      row.names = c("s1", "s2")),
    rowRanges = rr
  )

  lake$put("ranged_se", se)
  restored <- lake$get("ranged_se")

  expect_s4_class(restored, "RangedSummarizedExperiment")
  restored_rr <- SummarizedExperiment::rowRanges(restored)
  expect_equal(as.character(GenomicRanges::seqnames(restored_rr)),
    as.character(GenomicRanges::seqnames(rr)))
  expect_equal(IRanges::start(restored_rr), IRanges::start(rr))
  expect_equal(IRanges::end(restored_rr), IRanges::end(rr))
  expect_equal(as.character(GenomicRanges::strand(restored_rr)),
    as.character(GenomicRanges::strand(rr)))
})

test_that("SE adapter preserves GRangesList rowRanges via object fallback", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("GenomicRanges")
  skip_if_not_installed("IRanges")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_grangeslist_rowranges_adapter", root = tmpdir)

  counts <- matrix(
    c(1, 0, 3, 4, 5, 0),
    nrow = 3,
    ncol = 2,
    dimnames = list(c("g1", "g2", "g3"), c("s1", "s2"))
  )
  grl <- GenomicRanges::GRangesList(
    g1 = GenomicRanges::GRanges(
      seqnames = c("chr1", "chr1"),
      ranges = IRanges::IRanges(start = c(1L, 5L), width = c(2L, 3L))
    ),
    g2 = GenomicRanges::GRanges(
      seqnames = "chr2",
      ranges = IRanges::IRanges(start = 10L, width = 5L)
    ),
    g3 = GenomicRanges::GRanges(
      seqnames = c("chr3", "chr3"),
      ranges = IRanges::IRanges(start = c(7L, 20L), width = c(1L, 2L))
    )
  )

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts),
    colData = S4Vectors::DataFrame(batch = c("A", "B"),
      row.names = c("s1", "s2")),
    rowRanges = grl
  )

  lake$put("grl_se", se)
  expect_true(any(grepl("^grl_se\\.__se__\\.rowRanges\\.object$",
    lake$objects()$name)))

  restored <- lake$get("grl_se")
  restored_rr <- SummarizedExperiment::rowRanges(restored)
  expect_s4_class(restored_rr, "GRangesList")
  expect_equal(names(restored_rr), names(grl))
  expect_equal(length(restored_rr), length(grl))
  expect_equal(as.list(IRanges::start(restored_rr)),
    as.list(IRanges::start(grl)))
  expect_equal(as.list(IRanges::end(restored_rr)),
    as.list(IRanges::end(grl)))
})

test_that("Lake handles SingleCellExperiment via adapter", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")
  skip_if_not_installed("GenomicRanges")
  skip_if_not_installed("IRanges")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_sce_adapter", root = tmpdir)

  counts <- matrix(
    c(1, 0, 3, 4, 5, 0),
    nrow = 3,
    ncol = 2,
    dimnames = list(c("g1", "g2", "g3"), c("c1", "c2"))
  )
  rr <- GenomicRanges::GRanges(
    seqnames = c("chr1", "chr1", "chr2"),
    ranges = IRanges::IRanges(start = c(1L, 101L, 201L), width = 10L),
    strand = c("+", "-", "+")
  )
  names(rr) <- rownames(counts)
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts),
    colData = S4Vectors::DataFrame(batch = c("A", "B"),
      row.names = c("c1", "c2")),
    rowRanges = rr
  )
  SingleCellExperiment::reducedDim(sce, "PCA") <- matrix(
    c(0.1, 0.2, 0.3, 0.4),
    ncol = 2,
    dimnames = list(c("c1", "c2"), c("PC1", "PC2"))
  )
  alt <- SummarizedExperiment::SummarizedExperiment(
    assays = list(logcounts = counts + 1),
    colData = S4Vectors::DataFrame(batch = c("A", "B"),
      row.names = c("c1", "c2"))
  )
  SingleCellExperiment::altExp(sce, "alt1") <- alt
  SingleCellExperiment::sizeFactors(sce) <- c(0.9, 1.1)
  SingleCellExperiment::colLabels(sce) <- c("L1", "L2")
  SingleCellExperiment::rowPair(sce, "gene_graph") <- S4Vectors::SelfHits(
    from = c(1L, 2L),
    to = c(2L, 3L),
    nnode = nrow(sce)
  )
  SingleCellExperiment::colPair(sce, "cell_graph") <- S4Vectors::SelfHits(
    from = 1L,
    to = 2L,
    nnode = ncol(sce)
  )
  SingleCellExperiment::mainExpName(sce) <- "RNA"
  SingleCellExperiment::rowSubset(sce, "hvgs") <- c("g1", "g3")
  SingleCellExperiment::int_metadata(sce)$foo <- list(seed = 42L)
  SingleCellExperiment::int_colData(sce)$bar <- c("u", "v")
  SingleCellExperiment::int_elementMetadata(sce)$baz <- c(TRUE, FALSE, TRUE)

  lake$put("sce_obj", sce)
  stored_objects <- lake$objects()$name
  stored_tables <- lake$tables()$table_name
  expect_true("SingleCellExperiment" %in% names(get_adapters()))
  expect_true(any(grepl("^sce_obj\\.__sce__\\.manifest$", stored_objects)))
  expect_true(any(grepl("^sce_obj\\.__sce__\\.main\\.__se__\\.manifest$",
    stored_objects)))
  expect_true(any(grepl("^sce_obj\\.__sce__\\.", stored_tables)))
  expect_false("sce_obj" %in% stored_objects)

  restored <- lake$get("sce_obj")
  found <- lake$find("sce_obj", type = "object")
  expect_true(nrow(found) >= 1)
  expect_equal(found$name[[1]], "sce_obj")
  all_objects <- lake$find(type = "object")
  expect_false(any(grepl("\\.__(se|sce|mae|spectra|qfeatures|mse|xcms|chrom|seurat)__\\.",
    all_objects$name,
    perl = TRUE)))

  expect_s4_class(restored, "SingleCellExperiment")
  expect_equal(dim(restored), c(3, 2))
  expect_true("PCA" %in% names(SingleCellExperiment::reducedDims(restored)))
  expect_true("alt1" %in% SingleCellExperiment::altExpNames(restored))
  expect_equal(as.numeric(SingleCellExperiment::sizeFactors(restored)),
    c(0.9, 1.1))
  expect_equal(as.character(SingleCellExperiment::colLabels(restored)),
    c("L1", "L2"))
  restored_rr <- SummarizedExperiment::rowRanges(restored)
  expect_equal(as.character(GenomicRanges::seqnames(restored_rr)),
    as.character(GenomicRanges::seqnames(rr)))
  expect_equal(IRanges::start(restored_rr), IRanges::start(rr))
  expect_equal(IRanges::end(restored_rr), IRanges::end(rr))
  expect_equal(as.character(GenomicRanges::strand(restored_rr)),
    as.character(GenomicRanges::strand(rr)))
  expect_true("gene_graph" %in% SingleCellExperiment::rowPairNames(restored))
  expect_true("cell_graph" %in% SingleCellExperiment::colPairNames(restored))
  expect_equal(SingleCellExperiment::mainExpName(restored), "RNA")
  expect_true(identical(
    SingleCellExperiment::rowSubset(restored, "hvgs"),
    SingleCellExperiment::rowSubset(sce, "hvgs")
  ))
  expect_false(is.null(SingleCellExperiment::int_metadata(restored)$foo))
  expect_true("bar" %in% colnames(SingleCellExperiment::int_colData(restored)))
  expect_true(
    "baz" %in% colnames(SingleCellExperiment::int_elementMetadata(restored))
  )

  lake$tag("sce_obj", "v1")
  tagged <- lake$get("sce_obj", ref = "@tag(v1)")
  expect_s4_class(tagged, "SingleCellExperiment")
  expect_true("gene_graph" %in% SingleCellExperiment::rowPairNames(tagged))
  expect_true("cell_graph" %in% SingleCellExperiment::colPairNames(tagged))
  expect_equal(SingleCellExperiment::mainExpName(tagged), "RNA")
  expect_false(is.null(SingleCellExperiment::int_metadata(tagged)$foo))
  expect_true("bar" %in% colnames(SingleCellExperiment::int_colData(tagged)))
  expect_true(
    "baz" %in% colnames(SingleCellExperiment::int_elementMetadata(tagged))
  )
  tagged_rr <- SummarizedExperiment::rowRanges(tagged)
  expect_equal(as.character(GenomicRanges::seqnames(tagged_rr)),
    as.character(GenomicRanges::seqnames(rr)))
  expect_equal(IRanges::start(tagged_rr), IRanges::start(rr))
  expect_equal(IRanges::end(tagged_rr), IRanges::end(rr))
  expect_equal(as.character(GenomicRanges::strand(tagged_rr)),
    as.character(GenomicRanges::strand(rr)))

  lake$drop("sce_obj")
  expect_false(lake$exists("sce_obj"))
  expect_false(any(grepl("^sce_obj\\.__sce__\\.", lake$tables()$table_name)))
  expect_false(any(grepl("^sce_obj\\.__sce__\\.", lake$objects()$name)))
})

test_that("SCE adapter preserves nested altExp SingleCellExperiment", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_sce_nested_alt", root = tmpdir)

  main_counts <- matrix(
    c(1, 2, 0, 4, 0, 6, 7, 8, 9),
    nrow = 3,
    ncol = 3,
    dimnames = list(c("g1", "g2", "g3"), c("c1", "c2", "c3"))
  )
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = main_counts),
    colData = S4Vectors::DataFrame(
      donor = factor(c("D1", "D1", "D2")),
      row.names = c("c1", "c2", "c3")
    ),
    rowData = S4Vectors::DataFrame(
      symbol = c("G1", "G2", "G3"),
      pathways = I(list(c("P1", "P2"), "P3", "P4")),
      row.names = c("g1", "g2", "g3")
    )
  )
  S4Vectors::metadata(sce)$analysis <- list(version = "v1", seed = 42L)
  SingleCellExperiment::reducedDim(sce, "UMAP") <- matrix(
    c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    ncol = 2,
    dimnames = list(c("c1", "c2", "c3"), c("UMAP1", "UMAP2"))
  )
  SingleCellExperiment::rowPair(sce, "gene_graph") <- S4Vectors::SelfHits(
    from = c(1L, 2L),
    to = c(2L, 3L),
    nnode = nrow(sce)
  )
  SingleCellExperiment::colPair(sce, "cell_graph") <- S4Vectors::SelfHits(
    from = c(1L, 2L),
    to = c(2L, 3L),
    nnode = ncol(sce)
  )
  SingleCellExperiment::mainExpName(sce) <- "RNA"

  alt_counts <- matrix(
    c(5, 0, 1, 2, 3, 4),
    nrow = 2,
    ncol = 3,
    dimnames = list(c("ag1", "ag2"), c("c1", "c2", "c3"))
  )
  alt_sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = alt_counts),
    colData = S4Vectors::DataFrame(
      donor = factor(c("D1", "D1", "D2")),
      row.names = c("c1", "c2", "c3")
    )
  )
  SingleCellExperiment::reducedDim(alt_sce, "ALT_PCA") <- matrix(
    c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4),
    ncol = 2,
    dimnames = list(c("c1", "c2", "c3"), c("PC1", "PC2"))
  )
  nested_se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(logcounts = alt_counts + 1),
    colData = S4Vectors::DataFrame(
      donor = factor(c("D1", "D1", "D2")),
      row.names = c("c1", "c2", "c3")
    )
  )
  SingleCellExperiment::altExp(alt_sce, "nested_se") <- nested_se
  SingleCellExperiment::altExp(sce, "alt_sce") <- alt_sce

  lake$put("nested_sce", sce)
  nested_objects <- lake$objects()$name
  expect_true(any(grepl("^nested_sce\\.__sce__\\.manifest$", nested_objects)))
  expect_true(any(grepl(
    "^nested_sce\\.__sce__\\.altExp\\.alt_sce\\.__sce__\\.manifest$",
    nested_objects
  )))
  expect_true(any(grepl(
    paste0(
      "^nested_sce\\.__sce__\\.altExp\\.alt_sce\\.__sce__\\.",
      "altExp\\.nested_se\\.__se__\\.manifest$"
    ),
    nested_objects
  )))

  restored <- lake$get("nested_sce")
  found <- lake$find("nested_sce", type = "object")
  expect_true(nrow(found) >= 1)
  expect_equal(found$name[[1]], "nested_sce")
  all_objects <- lake$find(type = "object")
  expect_false(any(grepl("\\.__(se|sce|mae|spectra|qfeatures|mse|xcms|chrom|seurat)__\\.",
    all_objects$name,
    perl = TRUE)))

  expect_s4_class(restored, "SingleCellExperiment")
  expect_true("alt_sce" %in% SingleCellExperiment::altExpNames(restored))
  expect_true("UMAP" %in% names(SingleCellExperiment::reducedDims(restored)))
  expect_equal(SingleCellExperiment::mainExpName(restored), "RNA")
  expect_equal(
    S4Vectors::metadata(restored)$analysis$version,
    "v1"
  )
  expect_true(
    identical(
      as.character(SummarizedExperiment::rowData(restored)$symbol),
      c("G1", "G2", "G3")
    )
  )

  restored_alt <- SingleCellExperiment::altExp(restored, "alt_sce")
  expect_s4_class(restored_alt, "SingleCellExperiment")
  expect_true("ALT_PCA" %in% names(SingleCellExperiment::reducedDims(
    restored_alt
  )))
  expect_true("nested_se" %in% SingleCellExperiment::altExpNames(restored_alt))
  nested <- SingleCellExperiment::altExp(restored_alt, "nested_se")
  expect_s4_class(nested, "SummarizedExperiment")

  lake$tag("nested_sce", "nested_v1")
  tagged <- lake$get("nested_sce", ref = "@tag(nested_v1)")
  expect_s4_class(tagged, "SingleCellExperiment")
  expect_true("alt_sce" %in% SingleCellExperiment::altExpNames(tagged))
  expect_true("gene_graph" %in% SingleCellExperiment::rowPairNames(tagged))
  expect_true("cell_graph" %in% SingleCellExperiment::colPairNames(tagged))
  expect_equal(SingleCellExperiment::mainExpName(tagged), "RNA")

  lake$drop("nested_sce")
  expect_false(lake$exists("nested_sce"))
  expect_false(any(grepl("^nested_sce\\.__sce__\\.", lake$tables()$table_name)))
  expect_false(any(grepl("^nested_sce\\.__sce__\\.", lake$objects()$name)))
})

test_that("SCE tag preserves historical extended slots across updates", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("S4Vectors")

  tmpdir <- withr::local_tempdir()
  old_opt <- getOption("ol.root")
  on.exit(options(ol.root = old_opt), add = TRUE)
  options(ol.root = tmpdir)

  lake <- Lake$new("test_sce_tag_history", root = tmpdir)

  counts <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    ncol = 2,
    dimnames = list(c("g1", "g2"), c("c1", "c2"))
  )
  sce_v1 <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts),
    colData = S4Vectors::DataFrame(batch = c("A", "B"),
      row.names = c("c1", "c2"))
  )
  SingleCellExperiment::rowPair(sce_v1, "gene_graph") <- S4Vectors::SelfHits(
    from = 1L,
    to = 2L,
    nnode = nrow(sce_v1)
  )
  SingleCellExperiment::colPair(sce_v1, "cell_graph") <- S4Vectors::SelfHits(
    from = 1L,
    to = 2L,
    nnode = ncol(sce_v1)
  )
  SingleCellExperiment::mainExpName(sce_v1) <- "RNA"

  lake$put("sce_hist", sce_v1)
  lake$tag("sce_hist", "v1")

  sce_v2 <- sce_v1
  SingleCellExperiment::mainExpName(sce_v2) <- "RNA_v2"
  SingleCellExperiment::rowPair(sce_v2, "gene_graph") <- NULL
  SingleCellExperiment::colPair(sce_v2, "cell_graph") <- NULL
  lake$put("sce_hist", sce_v2)

  latest <- lake$get("sce_hist")
  expect_false("gene_graph" %in% SingleCellExperiment::rowPairNames(latest))
  expect_false("cell_graph" %in% SingleCellExperiment::colPairNames(latest))
  expect_equal(SingleCellExperiment::mainExpName(latest), "RNA_v2")

  tagged <- lake$get("sce_hist", ref = "@tag(v1)")
  expect_true("gene_graph" %in% SingleCellExperiment::rowPairNames(tagged))
  expect_true("cell_graph" %in% SingleCellExperiment::colPairNames(tagged))
  expect_equal(SingleCellExperiment::mainExpName(tagged), "RNA")
})
