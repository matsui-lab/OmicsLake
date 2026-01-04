# OmicsLake v2.0 DataLineage Audit Report

**Audit Date:** 2026-01-04
**Auditor:** Claude Code
**Scope:** Lake.R, DependencyTracker, ol_* backend functions
**Purpose:** Evaluate implementation quality for paper-grade DataLineage claims

---

## Executive Summary

The audit identified **3 P0 (critical)** and **4 P1 (high)** issues that must be addressed before the paper submission. The core problems are:

1. **Lazy/collect is broken** - `get(collect=FALSE)` always fetches data eagerly
2. **No version-aware lineage** - Dependencies store `parent_name` only, not version/ref
3. **Auto-tracking scope is misleading** - Only works within specific wrappers

---

## Audit Report Table

| Item | Status | Evidence | Risk | Fix | Test |
|------|--------|----------|------|-----|------|
| **2.1 Lazy/collect semantics** |
| `get(collect=FALSE)` returns lazy | **FAIL** | `Lake.R:645` calls `ol_read(..., collect=TRUE)` unconditionally | Users get eager data when expecting lazy; performance regression | Pass `collect` param through `.get_data()` | `test-lake.R`: verify `class(lake$get(collect=FALSE))` is `tbl_lazy` |
| `ref()` returns lazy tbl | PASS | `Lake.R:162-171` uses `ol_read(collect=FALSE)` | - | - | Already tested |
| `.is_lazy()` covers all lazy types | **PARTIAL** | `Lake.R:702-704` only checks `tbl_lazy` | May miss `tbl_sql`, `tbl_dbi` | Add checks for `tbl_sql`, `tbl_dbi` | Test with various dplyr backends |
| `where/select` pushed to SQL | **FAIL** | `.apply_filter()` operates on collected data | No SQL pushdown, defeats lazy purpose | Apply filter before collect decision | Verify `show_query()` includes WHERE |
| **2.2 Version-aware lineage** |
| `__ol_dependencies` has version columns | **FAIL** | `backend.R:88-90` schema lacks version columns | Cannot distinguish parent versions; breaks reproducibility claims | Add `parent_ref`, `parent_version_id` columns | Query schema for new columns |
| `track_read(name, ref)` stores ref | **FAIL** | `Lake.R:758-761` discards `ref` param | Version info lost at collection time | Store `ref` alongside `name` in tracker | Unit test ref preservation |
| `put()` records parent version | **FAIL** | `Lake.R:677-687` uses only `parent_name` | No version resolution | Resolve ref to version_id before recording | Integration test with tagged versions |
| `lake_deps` attr includes version | **FAIL** | `dplyr_compat.R:44-47` stores name only | dplyr pipes lose version info | Extend attr to include ref | Test attr after `ref()` |
| **2.3 Atomicity/Consistency** |
| `ol_write` is atomic | PASS | Uses DuckDB `CREATE OR REPLACE TABLE` | - | - | - |
| `snap()` uses transaction | **FAIL** | `Lake.R:186-189` calls `ol_commit` then `ol_label` separately | Orphaned commits if label fails | Wrap in transaction or use marker pattern | Force error in `ol_label`, verify no orphan |
| `ol_tag` uses transaction | **FAIL** | `backend.R:245-270` creates backup then updates refs | Orphaned backup tables | Wrap in transaction | Force error after backup, verify cleanup |
| `restore/checkout` handles broken refs | **PARTIAL** | `backend.R:349-372` warns on missing backups | Does not fail hard; may partially restore | Add pre-validation step | Test with corrupted refs table |
| **2.4 Tracking scope** |
| `put()` calls `start_write/end_write` | **FAIL** | `Lake.R:78-115` never calls tracker start/end | Auto-tracking claim is false for plain `put()` | Document scope or add auto-tracking | Verify `put()` after `get()` tracks dependency |
| `with_tracking()` works | PASS | `observe.R:266-282` correctly uses tracker | - | - | Already tested |
| `save_as()` works | PASS | `dplyr_compat.R:38-59` uses `lake_deps` attr | - | - | Already tested |
| observe mode tracks file I/O | **PARTIAL** | `observe.R:114-162` only sets flags, no actual hook | Does not intercept `read.csv` etc. | Implement actual I/O hooks or document limitation | Test with actual file I/O |
| **2.5 tidy-eval / NSE** |
| Formula filter works | PASS | `Lake.R:658-664` uses `rlang::f_rhs()` | - | - | Already tested |
| Expression/operation saved | **FAIL** | No storage of transformation details | Cannot replay operations | Document as dataset-level lineage only | N/A (documentation) |
| Bracket `[` with tbl_lazy | **FAIL** | `Lake.R:528-556` uses `substitute()` | Won't work correctly with lazy tbls | Guard against lazy input or handle correctly | Test `lake[name, expr]` with lazy |
| **2.6 API bugs** |
| `diff(ref1, ref2)` uses refs | **FAIL** | `Lake.R:230-233` ignores `ref1`, `ref2` params | Documented API broken | Pass refs to `ol_compare_versions` | Test diff with different refs |
| `.is_lazy()` complete | **PARTIAL** | `Lake.R:702-704` misses `tbl_sql` | Edge case detection failures | Extend inheritance check | Test with direct SQL tbl |

---

## P0 Critical Issues (Must Fix)

### P0-1: `get(collect=FALSE)` is Broken

**Title:** Lake$get() ignores collect parameter, always returns eager data

**Scope:** `R/Lake.R` lines 643-649

**Current Behavior:**
```r
.get_data = function(name, ref) {
  tryCatch({
    ol_read(name, ref = ref, project = private$.project, collect = TRUE)  # ALWAYS TRUE
  }, error = function(e) {
    ol_read_object(name, ref = ref, project = private$.project)
  })
}
```

**Impact:**
- Users expecting lazy evaluation get eager data
- Memory issues with large tables
- No SQL pushdown for `where`/`select`
- API contract violation

**Acceptance Criteria:**
1. `lake$get("t", collect=FALSE)` returns `tbl_lazy` for tables
2. `where`/`select` parameters are pushed to SQL when `collect=FALSE`
3. Objects still return R objects (not lazy)

**Files to Change:**
- `R/Lake.R`: `.get_data()`, `get()`, `.apply_filter()`, `.apply_select()`

**Tests to Add:**
```r
test_that("get(collect=FALSE) returns lazy table", {
  lake <- Lake$new("test_lazy")
  lake$put("data", data.frame(x = 1:1000))

  lazy <- lake$get("data", collect = FALSE)
  expect_true(inherits(lazy, "tbl_lazy") || inherits(lazy, "tbl_sql"))

  # Verify SQL generation
  query <- capture.output(dplyr::show_query(lazy))
  expect_true(any(grepl("SELECT", query)))
})

test_that("get with where/select uses SQL pushdown when lazy", {
  lake <- Lake$new("test_pushdown")
  lake$put("data", data.frame(x = 1:100, y = letters[1:100]))

  lazy <- lake$get("data", where = ~x > 50, select = "x", collect = FALSE)
  query <- capture.output(dplyr::show_query(lazy))
  expect_true(any(grepl("WHERE", query)))
})
```

---

### P0-2: Dependencies Lack Version Information

**Title:** Lineage graph stores parent name only, not version/ref

**Scope:**
- `R/backend.R` lines 86-96, 98-132
- `R/Lake.R` lines 755-762, 677-687

**Current Schema:**
```sql
CREATE TABLE __ol_dependencies (
  child_name TEXT,
  child_type TEXT,
  parent_name TEXT,
  parent_type TEXT,
  relationship_type TEXT,
  created_at TIMESTAMP
)
```

**Missing Columns:**
- `parent_ref` - The ref string used (e.g., "@tag(raw)", "@latest")
- `parent_version_id` - Resolved version identifier
- `parent_snapshot` - For tables, the backup table name if tagged
- `child_version_id` - Version of the child being created

**Impact:**
- Cannot answer "which version of parent A was used to create child B?"
- Breaks reproducibility claims for paper
- `lake$deps()` output is ambiguous

**Acceptance Criteria:**
1. `__ol_dependencies` includes `parent_ref`, `parent_version_id` columns
2. `DependencyTracker$track_read()` stores ref, not just name
3. `put()` resolves refs to version IDs before recording
4. `lake$deps()` shows version information
5. Migration script for existing DBs

**Files to Change:**
- `R/backend.R`: Schema, `.ol_record_dependency()`
- `R/Lake.R`: `DependencyTracker`, `.record_dependencies()`
- `R/io.R`: `ol_get_dependencies()`

**Tests to Add:**
```r
test_that("dependencies include version information", {
  lake <- Lake$new("test_versioned_deps")

  lake$put("a", data.frame(x = 1:5))
  lake$tag("a", "v1")
  lake$put("a", data.frame(x = 10:15))
  lake$tag("a", "v2")

  # Create b from a@tag(v1)
  result <- with_tracking(lake, "b", {
    d <- lake$get("a", ref = "@tag(v1)")
    d$y <- d$x * 2
    d
  })

  deps <- lake$deps("b", "up")
  expect_true("parent_ref" %in% names(deps) || "parent_version_id" %in% names(deps))
  # Should show v1, not v2
})
```

---

### P0-3: Auto-Tracking Scope is Misleading

**Title:** DependencyTracker only works within explicit wrappers, not plain put()

**Scope:** `R/Lake.R` lines 78-115

**Current Behavior:**
```r
put = function(name, data, depends_on = NULL, tags = NULL) {
  if (is.null(depends_on) && private$.auto_track) {
    tracked_deps <- private$.tracker$current_reads()  # Always empty!
    attr_deps <- attr(data, "lake_deps")
    depends_on <- unique(c(tracked_deps, attr_deps))
  }
  # ...
}
```

`current_reads()` is always empty because `put()` never calls `start_write()`.

**Impact:**
- Users think `auto_track=TRUE` means automatic dependency tracking
- Actually requires `with_tracking()`, `save_as()`, or explicit `depends_on`
- Documentation/paper claims are inaccurate

**Acceptance Criteria:**
One of:
1. **Option A:** Make `put()` truly auto-track by wrapping in start/end
2. **Option B:** Clearly document the limited scope of auto-tracking
3. **Option C:** Remove misleading `auto_track` parameter

**Recommended Fix (Option B + improvement):**
- Document clearly that auto-tracking works via:
  - `with_tracking()` wrapper
  - `save_as()` pipe terminator
  - `wrap_fn()` / `wrap_call()`
  - `lake_deps` attribute on data
- Consider: auto-start tracking context when `get()` is called

**Files to Change:**
- `R/Lake.R`: `put()` and documentation
- `man/Lake.Rd`: Update documentation
- Vignettes: Clarify auto-tracking scope

---

## P1 High Priority Issues

### P1-1: `diff()` Ignores ref Parameters

**Title:** Lake$diff(name, ref1, ref2) doesn't use ref1/ref2

**Scope:** `R/Lake.R` lines 230-233

**Current:**
```r
diff = function(name, ref1 = "@latest", ref2 = "@first") {
  private$.validate_name(name)
  ol_compare_versions(name, project = private$.project)  # refs not passed!
}
```

**Fix:**
```r
diff = function(name, ref1 = "@latest", ref2 = "@first") {
  private$.validate_name(name)
  ol_compare_versions(name, versions = c(ref1, ref2), project = private$.project)
}
```

**Files:** `R/Lake.R`, `R/versions.R`

---

### P1-2: `snap()` Not Atomic

**Title:** Snapshot creation can leave orphaned commits

**Scope:** `R/Lake.R` lines 180-192

**Current:**
```r
snap = function(label, note = "", params = list()) {
  ol_commit(note = note, params = params, project = private$.project)
  ol_label(label, project = private$.project)  # If this fails...
  invisible(self)
}
```

**Fix:** Use transaction or marker pattern:
```r
snap = function(label, note = "", params = list()) {
  state <- private$.state
  conn <- state$conn
  DBI::dbBegin(conn)
  tryCatch({
    ol_commit(note = note, params = params, project = private$.project)
    ol_label(label, project = private$.project)
    DBI::dbCommit(conn)
  }, error = function(e) {
    DBI::dbRollback(conn)
    stop(e)
  })
  invisible(self)
}
```

**Files:** `R/Lake.R`

---

### P1-3: `ol_tag()` Not Atomic

**Title:** Tag creation leaves orphaned backup tables on failure

**Scope:** `R/backend.R` lines 233-272

**Current Flow:**
1. Create backup table
2. Delete old ref entry
3. Insert new ref entry
→ If step 3 fails, orphaned backup table remains

**Fix:** Wrap in transaction

**Files:** `R/backend.R`

---

### P1-4: `.is_lazy()` Too Narrow

**Title:** Lazy detection misses `tbl_sql` and other db backends

**Scope:** `R/Lake.R` lines 702-704

**Current:**
```r
.is_lazy = function(data) {
  inherits(data, "tbl_lazy")
}
```

**Fix:**
```r
.is_lazy = function(data) {
  inherits(data, c("tbl_lazy", "tbl_sql", "tbl_dbi"))
}
```

**Files:** `R/Lake.R`

---

## P2 Medium Priority Issues

### P2-1: Bracket Notation with Lazy Tables

`Lake$[` uses `substitute()` which may not work correctly with lazy tables. Add type checking and appropriate handling.

### P2-2: Observe Mode File I/O Hooks

`observe()` sets tracking flags but doesn't actually intercept file I/O functions. Either implement proper hooks or document this limitation.

### P2-3: Expression Storage for Operation Lineage

Currently only dataset-level lineage. If paper claims operation-level lineage, need to store expressions. Otherwise, clarify in paper that only dataset relationships are tracked.

---

## Recommended Changes for Paper Claims

### Current Claims vs Reality

| Claim | Reality | Recommendation |
|-------|---------|----------------|
| "Automatic lineage tracking" | Only within specific wrappers | Clarify scope in paper |
| "Version-aware dependencies" | Only name stored, not version | Must fix (P0-2) |
| "Lazy evaluation support" | Broken | Must fix (P0-1) |
| "Operation-level provenance" | Dataset-level only | Clarify in paper |

### Suggested Paper Language

Instead of:
> "OmicsLake provides automatic data lineage tracking through any R code"

Use:
> "OmicsLake provides dataset-level lineage tracking through dplyr pipes and explicitly wrapped function calls. Dependencies are captured when using `save_as()`, `with_tracking()`, or explicit `depends_on` parameters."

---

## Test Coverage Additions

Add to `tests/testthat/`:

```r
# test-lazy-semantics.R
test_that("get(collect=FALSE) returns lazy for tables", {...})
test_that("get(collect=FALSE) returns eager for objects", {...})
test_that("where/select use SQL pushdown", {...})
test_that(".is_lazy detects all lazy types", {...})

# test-version-lineage.R
test_that("dependencies include parent version info", {...})
test_that("track_read preserves ref parameter", {...})
test_that("tagged version deps are distinguishable", {...})

# test-atomicity.R
test_that("snap() is atomic - rollback on failure", {...})
test_that("tag() is atomic - no orphaned backups", {...})
test_that("restore() validates refs before proceeding", {...})

# test-auto-tracking.R
test_that("plain get+put does not auto-track", {...})
test_that("with_tracking enables auto-tracking", {...})
test_that("save_as preserves lake_source", {...})
```

---

## Migration Notes

### Schema Migration for P0-2

```sql
-- Add version columns to __ol_dependencies
ALTER TABLE __ol_dependencies ADD COLUMN parent_ref TEXT;
ALTER TABLE __ol_dependencies ADD COLUMN parent_version_id TEXT;
ALTER TABLE __ol_dependencies ADD COLUMN child_version_id TEXT;

-- Backfill existing rows
UPDATE __ol_dependencies SET parent_ref = '@latest' WHERE parent_ref IS NULL;
```

### Backward Compatibility

- New columns should be nullable for existing DBs
- Old code reading dependencies should handle missing columns gracefully
- Version bump to 2.1.0 recommended

---

## Summary of Required Actions

### Before Paper Submission (P0)
1. Fix `get(collect=FALSE)` to return lazy tables
2. Add version columns to `__ol_dependencies`
3. Clarify auto-tracking scope in documentation

### Before v2.1 Release (P1)
4. Fix `diff()` to use ref parameters
5. Make `snap()` atomic with transaction
6. Make `ol_tag()` atomic with transaction
7. Expand `.is_lazy()` check

### Future Improvements (P2)
8. Handle bracket notation with lazy tables
9. Implement actual file I/O hooks in observe mode
10. Consider operation-level lineage storage

---

## Follow-up Verification Audit (2026-01-04)

After implementing the P0/P1 fixes, a verification audit was conducted to ensure correctness.

### Audit Areas

#### A. Transaction Effectiveness (snap/tag)

**Issue Found:** FAIL - Nested transactions causing premature commits

**Evidence:**
- `snap()` in `Lake.R` calls `DBI::dbBegin()`, then `ol_label()`
- `ol_label()` in `init.R` calls `ol_tag()` for each table
- `ol_tag()` in `backend.R` called `DBI::dbBegin()` inside the loop
- Result: Inner `dbCommit()` would commit the outer transaction prematurely

**Fix Applied:**
- Added `.in_transaction` parameter to `ol_tag()`, `ol_tag_object()`, and `ol_label()`
- When called with `.in_transaction = TRUE`, these functions skip their internal transaction management
- `snap()` now passes `.in_transaction = TRUE` to `ol_label()`

**Files Changed:**
- `R/backend.R`: `ol_tag()` and `ol_tag_object()` refactored with `.in_transaction` param
- `R/init.R`: `ol_label()` accepts and passes `.in_transaction` to child calls
- `R/Lake.R`: `snap()` passes `.in_transaction = TRUE`

**Tests Added:**
- `test-atomicity.R`: "nested ol_tag calls within snap don't cause transaction issues"
- `test-atomicity.R`: "ol_label with .in_transaction works correctly"

#### B. Multi-Parent Lineage Completeness

**Issue Found:** FAIL - Version refs not properly paired with names in joins

**Evidence:**
- `lake_source_ref` was stored as a single attribute, not paired with table names
- When joining `counts@v1` with `metadata@baseline`, the refs were stored in separate vectors
- In `put()`, the entire `lake_source_ref` vector was assigned to each dependency
- Result: Both parents would get the same refs instead of their individual refs

**Fix Applied:**
1. Added new `lake_sources` attribute format: `list(list(name=..., ref=...), ...)`
2. Updated `Lake$ref()` to set `lake_sources` with paired name-ref
3. Updated `dplyr_compat.R`:
   - `.preserve_lake_attrs()` preserves `lake_sources`
   - `.merge_lake_attrs()` properly merges paired name-ref objects during joins
   - `collect.lake_tbl()` transfers `lake_sources` to collected data
   - `save_as()` reads `lake_sources` first, falls back to legacy format
4. Updated `Lake$put()` to read from `lake_sources` first

**Files Changed:**
- `R/Lake.R`: `ref()` sets `lake_sources`, `put()` reads `lake_sources`
- `R/dplyr_compat.R`: All S3 methods preserve `lake_sources`, merge logic updated

**Tests Added:**
- `test-version-lineage.R`: "multi-parent join preserves individual version refs"
- `test-version-lineage.R`: "lake_sources attribute flows through dplyr pipe operations"

#### C. Migration Idempotency

**Status:** PASS

**Evidence:**
- `backend.R` uses `ADD COLUMN IF NOT EXISTS` for schema migration
- Running migration multiple times is safe
- SQLite compatibility handled with conditional logic

---

## Verification Audit Summary

| Area | Initial Status | Fix Applied | Verified |
|------|---------------|-------------|----------|
| A. Transaction effectiveness | FAIL (nested tx) | `.in_transaction` param | Tests added |
| B. Multi-parent lineage refs | FAIL (refs not paired) | `lake_sources` attribute | Tests added |
| C. Migration idempotency | PASS | N/A | Confirmed |

### Code Quality Notes

1. **Backward Compatibility:** Legacy `lake_source`/`lake_source_ref` attributes are still set for older code
2. **Graceful Degradation:** `put()` falls back to legacy format if `lake_sources` is not present
3. **Transaction Safety:** `.in_transaction` pattern allows composable transaction control
