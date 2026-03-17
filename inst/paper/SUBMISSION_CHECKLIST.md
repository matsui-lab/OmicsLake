# GigaScience Submission Checklist for OmicsLake

## Pre-submission Checklist

### Manuscript
- [x] Title page complete
- [x] Abstract: Background/Findings/Conclusions structure
- [x] Abstract <= 250 words and no references
- [x] All figures referenced in text
- [x] All tables referenced in text
- [x] References complete (no missing citations)
- [x] Numeric square-bracket citation style
- [x] Line numbers enabled for review
- [x] List of Abbreviations section added
- [x] Ethics Approval and Consent to Participate section added
- [x] Consent for Publication section added

### Figures
- [x] `Rscript inst/paper/figures/generate_all_figures.R` を実行して最新図版を再生成
- [x] Figure 1: Architecture diagram (PDF generated)
- [x] Figure 2: Lineage workflow (PDF generated)
- [x] Figure 3: Performance benchmarks (PDF generated)
- [x] Figure 4: Reproducibility workflow (PDF generated)
- [x] Figure 5: Reproducibility comparison (PDF generated)
- [x] Figure 6: Storage efficiency (PDF generated)
- [x] Figure S1: Lineage graph (PDF generated)
- [x] Figure S2 (optional): Major results dashboard (PDF generated)
- [x] `inst/paper/figures/output/submission_bundle_gigascience/submission_manifest.csv` confirms no `missing_required`
- [x] All figures 300 DPI or vector format
- [x] Captions complete and accurate
- [x] Figures/legends moved to manuscript end for TeX submission

### Tables
- [x] Table 1: Tool comparison - complete
- [x] Table 2: Performance benchmarks - values filled
- [x] Table 3: Reproducibility results - complete
- [x] Table S1: Complete benchmark results

### Data Availability
- [x] GitHub repository public
- [ ] Zenodo archive created (external step)
- [ ] Zenodo DOI obtained (external step)
- [ ] Code Ocean capsule created (external step)
- [ ] Code Ocean DOI obtained (external step)
- [x] All scripts executable
- [x] Versioned code snapshot and accession-like identifiers cited (Git revision + SWHID)

### Identifiers
- [ ] RRID obtained and added to manuscript (external step)
- [ ] ORCID for all authors
- [ ] DOIs for all references where available

### Reproducibility
- [x] Benchmark scripts tested
- [x] Reproducibility tests pass
- [x] Docker/container tested
- [x] README instructions verified

### GigaScience Specific
- [ ] Cover letter prepared
- [x] FAIR principles addressed
- [x] Data availability statement complete
- [x] Competing interests declared
- [x] Author contributions (CRediT) complete
- [x] Acknowledgements complete

## Post-acceptance Checklist
- [ ] Update Zenodo with accepted version
- [ ] Update Code Ocean capsule
- [ ] Announce on social media/mailing lists
- [ ] Update package documentation with citation
