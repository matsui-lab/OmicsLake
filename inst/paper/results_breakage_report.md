# Reproducibility Breakage Taxonomy Validation

Generated: 2026-02-24 22:57:05 JST

This report classifies typical reproducibility breakages, reproduces each one, and evaluates OmicsLake's mechanical detection and safe resolution behavior.

## Coverage Summary

```
                               metric  value
                       scenario_count 6.0000
                       detected_count 6.0000
                        detected_rate 1.0000
                      visualized_rate 1.0000
                  safe_execution_rate 1.0000
              median_diag_runtime_sec 0.3065
              median_auto_runtime_sec 0.3130
                  auto_restored_count 4.0000
      auto_restored_rate_among_broken 1.0000
             auto_mode_scenario_count 4.0000
              auto_mode_detected_rate 1.0000
        auto_mode_restore_action_rate 1.0000
           guided_mode_scenario_count 2.0000
            guided_mode_detected_rate 1.0000
     guided_mode_manual_proposal_rate 1.0000
 guided_mode_actionable_proposal_rate 1.0000
           guided_mode_guardrail_rate 1.0000
```

## Mode-specific Summary

```
          mode scenario_count detected_rate visualized_rate safe_execution_rate
 auto_rollback              4             1               1                   1
 guided_manual              2             1               1                   1
 median_diag_runtime_sec median_auto_runtime_sec restored_rate_among_broken
                   0.345                   0.330                          1
                   0.174                   0.192                         NA
 manual_proposal_rate actionable_proposal_rate guardrail_rate
                    1                        1              1
                    1                        1              1
```

## BX-001: Input table silently replaced

- Category: Input Drift
- Root cause: Raw count values are replaced without explicit provenance updates.
- Conventional bottleneck: The change is often noticed only after downstream plots/tables diverge; root-cause search is manual and slow.
- Injection: Input values replaced and downstream objects recomputed.
- OmicsLake detected: TRUE
- Resolution outcome: auto_restored
- Auto execution safety (no failed actions): TRUE

### 1) Situation

```
         project        generated_at_utc     target latest_commit_id
 breakage_bx_001 2026-02-24 13:57:00 UTC de_results  20260224-225658
         latest_commit_time validation_previous_label
 2026-02-24 13:56:58.960903                      <NA>
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
 reference_label=v1, compare_mode=semantic_table, latest_hash=0edc2f3e4016c4a944f3e349a3c99ad3, reference_hash=2ca9303126c9be9a57412a0e57fda122, max_abs_numeric_diff=NA
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
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//RtmpdNMQBJ/bx_bx-001_4b0b50d330a7/repro_ctx_git_renv')
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

## BX-002: Input schema/type drift

- Category: Schema Drift
- Root cause: Input field type/shape changes and downstream summaries no longer match previous output semantics.
- Conventional bottleneck: Schema drift can pass silently when code coerces types; impact is discovered late.
- Injection: Schema/type drift injected into counts$count.
- OmicsLake detected: TRUE
- Resolution outcome: auto_restored
- Auto execution safety (no failed actions): TRUE

### 1) Situation

```
         project        generated_at_utc     target latest_commit_id
 breakage_bx_002 2026-02-24 13:57:01 UTC de_results  20260224-225701
         latest_commit_time validation_previous_label
 2026-02-24 13:57:01.309396                      <NA>
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
 reference_label=v1, compare_mode=semantic_table, latest_hash=901a07035cc22204f5ea5999f028c173, reference_hash=2ca9303126c9be9a57412a0e57fda122, max_abs_numeric_diff=NA
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
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//RtmpdNMQBJ/bx_bx-002_4b0b6ca0d82b/repro_ctx_git_renv')
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

## BX-003: Analysis parameter changed

- Category: Parameter Drift
- Root cause: Filtering threshold is altered between runs without clear audit trail.
- Conventional bottleneck: Parameter provenance is dispersed across scripts/notebooks and difficult to compare.
- Injection: Analysis parameter threshold changed and outputs recomputed.
- OmicsLake detected: TRUE
- Resolution outcome: auto_restored
- Auto execution safety (no failed actions): TRUE

### 1) Situation

```
         project        generated_at_utc     target latest_commit_id
 breakage_bx_003 2026-02-24 13:57:02 UTC de_results  20260224-225702
         latest_commit_time validation_previous_label
 2026-02-24 13:57:02.435551                      <NA>
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
 reference_label=v1, compare_mode=semantic_table, latest_hash=2b223c620ea3cebdd305287033da27f0, reference_hash=2ca9303126c9be9a57412a0e57fda122, max_abs_numeric_diff=NA
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
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//RtmpdNMQBJ/bx_bx-003_4b0b6263eaf4/repro_ctx_git_renv')
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

## BX-004: Critical intermediate removed or overwritten

- Category: Artifact Loss
- Root cause: A key derived artifact is removed/replaced during iterative analysis.
- Conventional bottleneck: Recovery depends on ad-hoc backups or rerunning large sections manually.
- Injection: Critical artifact was removed/overwritten.
- OmicsLake detected: TRUE
- Resolution outcome: auto_restored
- Auto execution safety (no failed actions): TRUE

### 1) Situation

```
         project        generated_at_utc     target latest_commit_id
 breakage_bx_004 2026-02-24 13:57:03 UTC de_results  20260224-225703
         latest_commit_time validation_previous_label
 2026-02-24 13:57:03.378637                      <NA>
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
                                                                                                                                    upstream_edges=0, downstream_edges=0
 reference_label=v1, compare_mode=semantic_table, latest_hash=79f770ad499296fb6ac387d003ef9bda, reference_hash=2ca9303126c9be9a57412a0e57fda122, max_abs_numeric_diff=NA
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
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//RtmpdNMQBJ/bx_bx-004_4b0b58bb59bc/repro_ctx_git_renv')
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
            target_upstream_edges:de_results      0     3    TRUE
          target_downstream_edges:de_results      0     0   FALSE
               target_value_drift:de_results   TRUE  <NA>    TRUE
 target_external_dependency_drift:de_results  FALSE  <NA>    TRUE
```

## BX-005: Git provenance context missing

- Category: Provenance Gap
- Root cause: Execution occurs outside a valid Git repository context.
- Conventional bottleneck: Code state at analysis time cannot be reconstructed reliably.
- Injection: Repro context moved to non-git directory.
- OmicsLake detected: TRUE
- Resolution outcome: guided_manual_or_guardrail
- Auto execution safety (no failed actions): TRUE

### 1) Situation

```
         project        generated_at_utc     target latest_commit_id
 breakage_bx_005 2026-02-24 13:57:04 UTC de_results  20260224-225704
         latest_commit_time validation_previous_label
 2026-02-24 13:57:04.353006                      <NA>
 validation_structural_changes validation_row_count_changes
                             0                            0
 target_reference_label target_value_drift target_compare_mode
                   <NA>                 NA                <NA>
 target_semantic_equivalent target_numeric_tolerance
                         NA                    1e-08
 target_external_dependencies_n target_external_dependency_drift
                              0                               NA
 doctor_failures
               1
```

### 2) Cause Identification

```
  source                    item
  doctor git repository detected
 lineage              de_results
                                               diagnosis
          not detected from reproducibility context path
 Dependency footprint was collected for the target node.
                                                                   evidence
 Run inside a Git repository or set options(ol.repro.path = '<repo path>').
                                       upstream_edges=3, downstream_edges=0
```

### 3) Fix Proposals

```
          action_id                                                   summary
           manual_1            Resolve failing check: git repository detected
 enable_strict_mode Enable strict reproducibility guardrails for future runs.
                                                                                                                                  command
                                                               Run inside a Git repository or set options(ol.repro.path = '<repo path>').
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//RtmpdNMQBJ/bx_bx-005_4b0b303c74ee/repro_ctx_none')
 auto_supported selected
          FALSE    FALSE
           TRUE    FALSE
                                                  rationale
             not detected from reproducibility context path
 Prevents drift by enforcing capture + validation defaults.
```

### 4) Auto Execution

```
          action_id status                             message
 enable_strict_mode     ok strict reproducibility mode enabled
```

### 5) Before/After Comparison

```
                                      metric before after changed
                             doctor_failures      1     1   FALSE
                                      tables      5     5   FALSE
                                     objects      0     0   FALSE
                           default_shortcuts   TRUE  TRUE   FALSE
                                   git_dirty   <NA>  <NA>   FALSE
                    target_exists:de_results   TRUE  TRUE   FALSE
            target_upstream_edges:de_results      3     3   FALSE
          target_downstream_edges:de_results      0     0   FALSE
               target_value_drift:de_results   <NA>  <NA>   FALSE
 target_external_dependency_drift:de_results   <NA>  <NA>   FALSE
```

## BX-006: renv lockfile missing

- Category: Environment Gap
- Root cause: Package lockfile is absent, so package versions cannot be pinned mechanically.
- Conventional bottleneck: Environment reconstruction relies on manual package history and memory.
- Injection: Repro context moved to git-only directory without renv.lock.
- OmicsLake detected: TRUE
- Resolution outcome: guided_manual_or_guardrail
- Auto execution safety (no failed actions): TRUE

### 1) Situation

```
         project        generated_at_utc     target latest_commit_id
 breakage_bx_006 2026-02-24 13:57:05 UTC de_results  20260224-225704
        latest_commit_time validation_previous_label
 2026-02-24 13:57:04.78315                      <NA>
 validation_structural_changes validation_row_count_changes
                             0                            0
 target_reference_label target_value_drift target_compare_mode
                   <NA>                 NA                <NA>
 target_semantic_equivalent target_numeric_tolerance
                         NA                    1e-08
 target_external_dependencies_n target_external_dependency_drift
                              0                               NA
 doctor_failures
               1
```

### 2) Cause Identification

```
  source                   item
  doctor renv lockfile detected
 lineage             de_results
                                               diagnosis
                                     renv.lock not found
 Dependency footprint was collected for the target node.
                                                        evidence
 Run renv::init() and renv::snapshot() in your analysis project.
                            upstream_edges=3, downstream_edges=0
```

### 3) Fix Proposals

```
             action_id
 refresh_renv_lockfile
    enable_strict_mode
                                                   summary
   Regenerate renv.lock to stabilize package environments.
 Enable strict reproducibility guardrails for future runs.
                                                                                                                                      command
                                                                                                                             renv::snapshot()
 ol_enable_strict_repro_mode(path = '/var/folders/5d/nnly9gxs5znfq_z0345_8vlc0000gn/T//RtmpdNMQBJ/bx_bx-006_4b0b698a5d46/repro_ctx_git_only')
 auto_supported selected
          FALSE    FALSE
           TRUE    FALSE
                                                       rationale
 Run renv::init() and renv::snapshot() in your analysis project.
      Prevents drift by enforcing capture + validation defaults.
```

### 4) Auto Execution

```
          action_id status                             message
 enable_strict_mode     ok strict reproducibility mode enabled
```

### 5) Before/After Comparison

```
                                      metric before after changed
                             doctor_failures      1     1   FALSE
                                      tables      5     5   FALSE
                                     objects      0     0   FALSE
                           default_shortcuts   TRUE  TRUE   FALSE
                                   git_dirty  FALSE FALSE   FALSE
                    target_exists:de_results   TRUE  TRUE   FALSE
            target_upstream_edges:de_results      3     3   FALSE
          target_downstream_edges:de_results      0     0   FALSE
               target_value_drift:de_results   <NA>  <NA>   FALSE
 target_external_dependency_drift:de_results   <NA>  <NA>   FALSE
```

