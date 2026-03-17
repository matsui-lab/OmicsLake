# Reproducibility Limit/Boundary Stress Test

Generated: 2026-02-24 22:57:14 JST

This report evaluates boundary conditions for fair assessment, including expected failures and known limitations.

## Coverage Summary

```
                             metric value
                     scenario_count 5.000
        ground_truth_breakage_count 3.000
    ground_truth_non_breakage_count 2.000
                     detected_count 3.000
                true_positive_count 3.000
                true_negative_count 2.000
               false_positive_count 0.000
               false_negative_count 0.000
                false_positive_rate 0.000
                false_negative_rate 0.000
        auto_restore_expected_count 2.000
         auto_restore_success_count 2.000
 auto_restore_success_rate_expected 1.000
         known_limit_scenario_count 1.000
          known_limit_exposed_count 1.000
           known_limit_exposed_rate 1.000
            median_diag_runtime_sec 0.327
            median_auto_runtime_sec 0.311
```

## Detection Confusion Matrix

```
       truth    predicted count
    breakage     detected     3
    breakage not_detected     0
 no_breakage     detected     0
 no_breakage not_detected     2
```

## LT-001: Row order permutation only

- Category: Semantic Equivalence
- Limit focus: false_positive_semantic_equivalence
- Ground truth breakage: FALSE
- Semantic equivalent: TRUE
- OmicsLake detected: FALSE
- False positive: FALSE
- False negative: FALSE
- Limitation exposed: FALSE
- Restore status: ok
- Restored to baseline: TRUE

### 1) Situation

```
      project        generated_at_utc     target latest_commit_id
 limit_lt_001 2026-02-24 13:57:09 UTC de_results  20260224-225708
         latest_commit_time validation_previous_label
 2026-02-24 13:57:08.137375                      <NA>
 validation_structural_changes validation_row_count_changes
                             0                            0
 target_reference_label target_value_drift target_compare_mode
                     v1              FALSE      semantic_table
 target_semantic_equivalent target_numeric_tolerance
                       TRUE                    1e-08
 target_external_dependencies_n target_external_dependency_drift
                              0                            FALSE
 doctor_failures
               1
```

### 2) Cause Identification

```
  source                      item
  doctor git working tree is clean
 lineage                de_results
                                               diagnosis
                                         changed files=1
 Dependency footprint was collected for the target node.
                                                                     evidence
 Commit or stash local changes before creating a final reproducible snapshot.
                                         upstream_edges=6, downstream_edges=0
```

### 3) Fix Proposals

```
          action_id                                                   summary
    clean_git_state Commit or stash local Git changes before final snapshots.
   restore_snapshot                          Rollback to snapshot label 'v1'.
 enable_strict_mode Enable strict reproducibility guardrails for future runs.
                                                                                                                                     command
                                                                   git status && git add -A && git commit -m 'snapshot prep'  # or git stash
                                                                                                                               restore('v1')
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//Rtmpf7ooJ9/lt_lt-001_4e63bf78800/repro_ctx_git_renv')
 auto_supported selected
          FALSE    FALSE
           TRUE    FALSE
           TRUE    FALSE
                                                  rationale
                                            changed files=1
              Fastest recovery path when drift is detected.
 Prevents drift by enforcing capture + validation defaults.
```

### 4) Auto Execution

```
          action_id status                             message
   restore_snapshot     ok              restored to label 'v1'
 enable_strict_mode     ok strict reproducibility mode enabled
```

### 5) Before/After Comparison

```
                                      metric before after changed
                             doctor_failures      1     1   FALSE
                                      tables      5     5   FALSE
                                     objects      0     0   FALSE
                           default_shortcuts   TRUE  TRUE   FALSE
                                   git_dirty   TRUE  TRUE   FALSE
                    target_exists:de_results   TRUE  TRUE   FALSE
            target_upstream_edges:de_results      6     3    TRUE
          target_downstream_edges:de_results      0     0   FALSE
               target_value_drift:de_results  FALSE  <NA>    TRUE
 target_external_dependency_drift:de_results  FALSE  <NA>    TRUE
```

## LT-002: Sub-femtoscale numeric jitter

- Category: Numeric Tolerance
- Limit focus: false_positive_numeric_tolerance
- Ground truth breakage: FALSE
- Semantic equivalent: TRUE
- OmicsLake detected: FALSE
- False positive: FALSE
- False negative: FALSE
- Limitation exposed: FALSE
- Restore status: ok
- Restored to baseline: TRUE

### 1) Situation

```
      project        generated_at_utc     target latest_commit_id
 limit_lt_002 2026-02-24 13:57:10 UTC de_results  20260224-225710
        latest_commit_time validation_previous_label
 2026-02-24 13:57:10.40098                      <NA>
 validation_structural_changes validation_row_count_changes
                             0                            0
 target_reference_label target_value_drift target_compare_mode
                     v1              FALSE      semantic_table
 target_semantic_equivalent target_numeric_tolerance
                       TRUE                    1e-08
 target_external_dependencies_n target_external_dependency_drift
                              0                            FALSE
 doctor_failures
               1
```

### 2) Cause Identification

```
  source                      item
  doctor git working tree is clean
 lineage                de_results
                                               diagnosis
                                         changed files=1
 Dependency footprint was collected for the target node.
                                                                     evidence
 Commit or stash local changes before creating a final reproducible snapshot.
                                         upstream_edges=6, downstream_edges=0
```

### 3) Fix Proposals

```
          action_id                                                   summary
    clean_git_state Commit or stash local Git changes before final snapshots.
   restore_snapshot                          Rollback to snapshot label 'v1'.
 enable_strict_mode Enable strict reproducibility guardrails for future runs.
                                                                                                                                      command
                                                                    git status && git add -A && git commit -m 'snapshot prep'  # or git stash
                                                                                                                                restore('v1')
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//Rtmpf7ooJ9/lt_lt-002_4e634380a928/repro_ctx_git_renv')
 auto_supported selected
          FALSE    FALSE
           TRUE    FALSE
           TRUE    FALSE
                                                  rationale
                                            changed files=1
              Fastest recovery path when drift is detected.
 Prevents drift by enforcing capture + validation defaults.
```

### 4) Auto Execution

```
          action_id status                             message
   restore_snapshot     ok              restored to label 'v1'
 enable_strict_mode     ok strict reproducibility mode enabled
```

### 5) Before/After Comparison

```
                                      metric before after changed
                             doctor_failures      1     1   FALSE
                                      tables      5     5   FALSE
                                     objects      0     0   FALSE
                           default_shortcuts   TRUE  TRUE   FALSE
                                   git_dirty   TRUE  TRUE   FALSE
                    target_exists:de_results   TRUE  TRUE   FALSE
            target_upstream_edges:de_results      6     3    TRUE
          target_downstream_edges:de_results      0     0   FALSE
               target_value_drift:de_results  FALSE  <NA>    TRUE
 target_external_dependency_drift:de_results  FALSE  <NA>    TRUE
```

## LT-003: Untracked external factor file drift

- Category: External Dependency
- Limit focus: cause_attribution_gap_external_dependency
- Ground truth breakage: TRUE
- Semantic equivalent: FALSE
- OmicsLake detected: TRUE
- False positive: FALSE
- False negative: FALSE
- Limitation exposed: TRUE
- Restore status: ok
- Restored to baseline: TRUE

### 1) Situation

```
      project        generated_at_utc     target latest_commit_id
 limit_lt_003 2026-02-24 13:57:11 UTC de_results  20260224-225711
         latest_commit_time validation_previous_label
 2026-02-24 13:57:11.339572                      <NA>
 validation_structural_changes validation_row_count_changes
                             0                            0
 target_reference_label target_value_drift target_compare_mode
                     v1               TRUE      semantic_table
 target_semantic_equivalent target_numeric_tolerance
                      FALSE                    1e-08
 target_external_dependencies_n target_external_dependency_drift
                              0                            FALSE
 doctor_failures
               1
```

### 2) Cause Identification

```
      source                      item
      doctor git working tree is clean
     lineage                de_results
 target_diff                de_results
                                               diagnosis
                                         changed files=1
 Dependency footprint was collected for the target node.
       Target differs from the selected reference label.
                                                                                                                                                                                 evidence
                                                                                                             Commit or stash local changes before creating a final reproducible snapshot.
                                                                                                                                                     upstream_edges=6, downstream_edges=0
 reference_label=v1, compare_mode=semantic_table, latest_hash=c1001ffcf2658b69eb38951ac1f0b2fa, reference_hash=2ca9303126c9be9a57412a0e57fda122, max_abs_numeric_diff=0.00061300000000003
```

### 3) Fix Proposals

```
          action_id                                                   summary
    clean_git_state Commit or stash local Git changes before final snapshots.
   restore_snapshot                          Rollback to snapshot label 'v1'.
 enable_strict_mode Enable strict reproducibility guardrails for future runs.
                                                                                                                                      command
                                                                    git status && git add -A && git commit -m 'snapshot prep'  # or git stash
                                                                                                                                restore('v1')
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//Rtmpf7ooJ9/lt_lt-003_4e636e3578ca/repro_ctx_git_renv')
 auto_supported selected
          FALSE    FALSE
           TRUE    FALSE
           TRUE    FALSE
                                                  rationale
                                            changed files=1
              Fastest recovery path when drift is detected.
 Prevents drift by enforcing capture + validation defaults.
```

### 4) Auto Execution

```
          action_id status                             message
   restore_snapshot     ok              restored to label 'v1'
 enable_strict_mode     ok strict reproducibility mode enabled
```

### 5) Before/After Comparison

```
                                      metric before after changed
                             doctor_failures      1     1   FALSE
                                      tables      5     5   FALSE
                                     objects      0     0   FALSE
                           default_shortcuts   TRUE  TRUE   FALSE
                                   git_dirty   TRUE  TRUE   FALSE
                    target_exists:de_results   TRUE  TRUE   FALSE
            target_upstream_edges:de_results      6     3    TRUE
          target_downstream_edges:de_results      0     0   FALSE
               target_value_drift:de_results   TRUE  <NA>    TRUE
 target_external_dependency_drift:de_results  FALSE  <NA>    TRUE
```

## LT-004: Rollback requested without snapshot label

- Category: Rollback Preconditions
- Limit focus: rollback_precondition_gap
- Ground truth breakage: TRUE
- Semantic equivalent: FALSE
- OmicsLake detected: TRUE
- False positive: FALSE
- False negative: FALSE
- Limitation exposed: TRUE
- Restore status: failed
- Restored to baseline: FALSE

### 1) Situation

```
      project        generated_at_utc     target latest_commit_id
 limit_lt_004 2026-02-24 13:57:12 UTC de_results             <NA>
 latest_commit_time validation_previous_label validation_structural_changes
               <NA>                      <NA>                            NA
 validation_row_count_changes target_reference_label target_value_drift
                           NA                     v1                 NA
 target_compare_mode target_semantic_equivalent target_numeric_tolerance
                <NA>                         NA                    1e-08
 target_external_dependencies_n target_external_dependency_drift
                              0                               NA
 doctor_failures
               1
```

### 2) Cause Identification

```
      source                      item
      doctor git working tree is clean
     lineage                de_results
 target_diff                de_results
                                                   diagnosis
                                             changed files=1
     Dependency footprint was collected for the target node.
 Could not compare target with the selected reference label.
                                                                                                                                                                                                                                                                                                                                                                                            evidence
                                                                                                                                                                                                                                                                                                                        Commit or stash local changes before creating a final reproducible snapshot.
                                                                                                                                                                                                                                                                                                                                                                upstream_edges=6, downstream_edges=0
 reference read failed: Data not found: 'de_results' (ref=@v1). Table read error: Failed to read 'de_results' at ref '@v1'. Table read error: Unknown tag: v1. Object read error: Tag not found for object 'de_results': v1.. Object read error: Tag not found for object 'de_results': v1. Available names: analysis_params, counts, de_results, metadata, normalized. Closest matches: de_results.
```

### 3) Fix Proposals

```
          action_id                                                   summary
    clean_git_state Commit or stash local Git changes before final snapshots.
   restore_snapshot                          Rollback to snapshot label 'v1'.
 enable_strict_mode Enable strict reproducibility guardrails for future runs.
                                                                                                                                      command
                                                                    git status && git add -A && git commit -m 'snapshot prep'  # or git stash
                                                                                                                                restore('v1')
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//Rtmpf7ooJ9/lt_lt-004_4e636dd013d8/repro_ctx_git_renv')
 auto_supported selected
          FALSE    FALSE
           TRUE    FALSE
           TRUE    FALSE
                                                  rationale
                                            changed files=1
              Fastest recovery path when drift is detected.
 Prevents drift by enforcing capture + validation defaults.
```

### 4) Auto Execution

```
          action_id status                             message
   restore_snapshot failed         Restore label not found: v1
 enable_strict_mode     ok strict reproducibility mode enabled
```

### 5) Before/After Comparison

```
                                      metric before after changed
                             doctor_failures      1     1   FALSE
                                      tables      5     5   FALSE
                                     objects      0     0   FALSE
                           default_shortcuts   TRUE  TRUE   FALSE
                                   git_dirty   TRUE  TRUE   FALSE
                    target_exists:de_results   TRUE  TRUE   FALSE
            target_upstream_edges:de_results      6     6   FALSE
          target_downstream_edges:de_results      0     0   FALSE
               target_value_drift:de_results   <NA>  <NA>   FALSE
 target_external_dependency_drift:de_results   <NA>  <NA>   FALSE
```

## LT-005: Random-noise recomputation without fixed seed

- Category: Stochastic Analysis
- Limit focus: control_detectable_breakage
- Ground truth breakage: TRUE
- Semantic equivalent: FALSE
- OmicsLake detected: TRUE
- False positive: FALSE
- False negative: FALSE
- Limitation exposed: FALSE
- Restore status: ok
- Restored to baseline: TRUE

### 1) Situation

```
      project        generated_at_utc     target latest_commit_id
 limit_lt_005 2026-02-24 13:57:13 UTC de_results  20260224-225713
         latest_commit_time validation_previous_label
 2026-02-24 13:57:13.393822                      <NA>
 validation_structural_changes validation_row_count_changes
                             0                            0
 target_reference_label target_value_drift target_compare_mode
                     v1               TRUE      semantic_table
 target_semantic_equivalent target_numeric_tolerance
                      FALSE                    1e-08
 target_external_dependencies_n target_external_dependency_drift
                              0                            FALSE
 doctor_failures
               1
```

### 2) Cause Identification

```
      source                      item
      doctor git working tree is clean
     lineage                de_results
 target_diff                de_results
                                               diagnosis
                                         changed files=1
 Dependency footprint was collected for the target node.
       Target differs from the selected reference label.
                                                                                                                                                                      evidence
                                                                                                  Commit or stash local changes before creating a final reproducible snapshot.
                                                                                                                                          upstream_edges=6, downstream_edges=0
 reference_label=v1, compare_mode=semantic_table, latest_hash=e246030a9198f6317bc6120dd5f9ccb0, reference_hash=2ca9303126c9be9a57412a0e57fda122, max_abs_numeric_diff=0.091082
```

### 3) Fix Proposals

```
          action_id                                                   summary
    clean_git_state Commit or stash local Git changes before final snapshots.
   restore_snapshot                          Rollback to snapshot label 'v1'.
 enable_strict_mode Enable strict reproducibility guardrails for future runs.
                                                                                                                                      command
                                                                    git status && git add -A && git commit -m 'snapshot prep'  # or git stash
                                                                                                                                restore('v1')
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//Rtmpf7ooJ9/lt_lt-005_4e636f26fd19/repro_ctx_git_renv')
 auto_supported selected
          FALSE    FALSE
           TRUE    FALSE
           TRUE    FALSE
                                                  rationale
                                            changed files=1
              Fastest recovery path when drift is detected.
 Prevents drift by enforcing capture + validation defaults.
```

### 4) Auto Execution

```
          action_id status                             message
   restore_snapshot     ok              restored to label 'v1'
 enable_strict_mode     ok strict reproducibility mode enabled
```

### 5) Before/After Comparison

```
                                      metric before after changed
                             doctor_failures      1     1   FALSE
                                      tables      5     5   FALSE
                                     objects      0     0   FALSE
                           default_shortcuts   TRUE  TRUE   FALSE
                                   git_dirty   TRUE  TRUE   FALSE
                    target_exists:de_results   TRUE  TRUE   FALSE
            target_upstream_edges:de_results      6     3    TRUE
          target_downstream_edges:de_results      0     0   FALSE
               target_value_drift:de_results   TRUE  <NA>    TRUE
 target_external_dependency_drift:de_results  FALSE  <NA>    TRUE
```

