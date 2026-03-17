# Zenodo Archive for OmicsLake

This directory contains documentation and guidelines for creating and maintaining the Zenodo archive for the OmicsLake paper.

## Archive Structure

The Zenodo archive should include the following files and directories:

### Core Package Files
```
OmicsLake/
├── R/                          # R source code
├── man/                        # Documentation
├── inst/
│   └── paper/
│       ├── 00_generate_datasets.R           # Synthetic data generation
│       ├── 01_performance_benchmark.R       # Performance benchmarks
│       ├── 02_reproducibility_test.R        # Reproducibility validation
│       ├── 03_reproducibility_experiments.R # Extended experiments
│       ├── figures/                         # Generated figures
│       ├── manuscript/                      # Manuscript files
│       └── codeocean/                       # Code Ocean capsule
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
└── .zenodo.json                # Zenodo metadata
```

### Files to Include

| File/Directory | Description | Required |
|----------------|-------------|----------|
| `R/` | Package source code | Yes |
| `man/` | Function documentation | Yes |
| `inst/paper/01_performance_benchmark.R` | Benchmark scripts | Yes |
| `inst/paper/02_reproducibility_test.R` | Reproducibility tests | Yes |
| `inst/paper/03_reproducibility_experiments.R` | Extended experiments | Yes |
| `inst/paper/figures/` | Publication figures | Yes |
| `inst/paper/manuscript/` | Manuscript source files | Yes |
| `inst/paper/codeocean/` | Code Ocean configuration | Yes |
| `vignettes/` | Package tutorials | Yes |
| `tests/` | Unit tests | Yes |
| `DESCRIPTION` | Package metadata | Yes |
| `LICENSE` | MIT License | Yes |
| `.zenodo.json` | Zenodo metadata | Yes |

### Files to Exclude

| File/Pattern | Reason |
|--------------|--------|
| `.git/` | Version control history |
| `.Rproj.user/` | RStudio user settings |
| `*.Rproj` | RStudio project file |
| `.DS_Store` | macOS metadata |
| `*.Rhistory` | R session history |
| `results/` | Generated output (reproducible) |
| `cache/` | Temporary cache files |

## Upload Procedure

### 1. Prepare the Archive

```bash
# Navigate to project root
cd /path/to/OmicsLake

# Create archive (excluding unnecessary files)
zip -r OmicsLake-v1.0.0.zip . \
  -x "*.git*" \
  -x "*.Rproj.user/*" \
  -x "*.DS_Store" \
  -x "*.Rhistory" \
  -x "*results/*" \
  -x "*cache/*" \
  -x "*.Rproj"

# Alternatively, using tar
tar --exclude='.git' \
    --exclude='.Rproj.user' \
    --exclude='.DS_Store' \
    --exclude='*.Rhistory' \
    --exclude='results' \
    --exclude='cache' \
    -czvf OmicsLake-v1.0.0.tar.gz .
```

### 2. Create Zenodo Deposit

1. Go to [Zenodo](https://zenodo.org/) and log in
2. Click "New Upload"
3. Upload the archive file (`.zip` or `.tar.gz`)
4. The metadata will be auto-populated from `.zenodo.json`
5. Review and complete any missing fields:
   - Verify author ORCID IDs
   - Add publication date
   - Link to related publications (if available)

### 3. Review Metadata

Before publishing, verify:
- [ ] Title matches the manuscript
- [ ] All authors are listed with correct affiliations
- [ ] ORCID IDs are valid (replace `0000-0000-0000-0000` with actual IDs)
- [ ] Keywords are appropriate
- [ ] License is correct (MIT)
- [ ] Related identifiers point to correct URLs

### 4. Publish

1. Click "Preview" to review the deposit page
2. Click "Publish" to make the deposit public
3. Note the assigned DOI for citation

## DOI and Versioning

### Initial DOI

After publishing, Zenodo assigns two DOIs:
- **Concept DOI**: Persistent DOI for all versions (e.g., `10.5281/zenodo.XXXXXXX`)
- **Version DOI**: Specific DOI for this version (e.g., `10.5281/zenodo.YYYYYYY`)

**Recommendation**: Use the Concept DOI in the manuscript for long-term stability.

### Updating After DOI Assignment

Once the DOI is assigned, update the following files:

#### 1. DESCRIPTION file
```
URL: https://github.com/matsui-lab/OmicsLake,
     https://doi.org/10.5281/zenodo.XXXXXXX
```

#### 2. README.md (project root)
Add a citation section:
```markdown
## Citation

If you use OmicsLake in your research, please cite:

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)

Matsui, Y. (2024). OmicsLake: Versioned Data Management with Automatic
Lineage Tracking for Reproducible Bioinformatics. Zenodo.
https://doi.org/10.5281/zenodo.XXXXXXX
```

#### 3. inst/CITATION file
```r
bibentry(
  bibtype = "Software",
  title   = "OmicsLake: Versioned Data Management with Automatic Lineage Tracking for Reproducible Bioinformatics",
  author  = person("Yusuke", "Matsui"),
  year    = "2024",
  doi     = "10.5281/zenodo.XXXXXXX",
  url     = "https://doi.org/10.5281/zenodo.XXXXXXX"
)
```

### Creating New Versions

When releasing a new version:

1. Go to your Zenodo deposit
2. Click "New version"
3. Upload the new archive
4. Update version number in metadata
5. Add release notes describing changes
6. Publish

The Concept DOI remains the same; a new Version DOI is assigned.

## Integration with GitHub

### GitHub-Zenodo Integration

For automatic archiving of GitHub releases:

1. Go to [Zenodo GitHub settings](https://zenodo.org/account/settings/github/)
2. Enable the repository `matsui-lab/OmicsLake`
3. Create a new release on GitHub
4. Zenodo automatically creates a new deposit

### Release Workflow

```bash
# Tag a new release
git tag -a v1.0.0 -m "Release v1.0.0"
git push origin v1.0.0

# Or use GitHub CLI
gh release create v1.0.0 --title "OmicsLake v1.0.0" --notes "Initial release"
```

## Checklist for Paper Submission

Before submitting the manuscript:

- [ ] `.zenodo.json` has correct metadata
- [ ] All author ORCIDs are valid and verified
- [ ] Archive has been uploaded to Zenodo
- [ ] DOI has been obtained
- [ ] DOI is referenced in manuscript
- [ ] DESCRIPTION file updated with DOI
- [ ] README.md includes citation information
- [ ] Code Ocean capsule linked (if applicable)

## Contact

For questions about the Zenodo archive:
- Repository: https://github.com/matsui-lab/OmicsLake
- Issues: https://github.com/matsui-lab/OmicsLake/issues
