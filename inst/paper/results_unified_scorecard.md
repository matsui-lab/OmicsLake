# Unified Reproducibility Scorecard

Generated: 2026-02-24 14:29:27 UTC

## Scope Summary

```
          scope metric_count pass_count pass_rate
         common            8          8         1
  auto_rollback            2          2         1
  guided_manual            3          3         1
 limit_boundary            4          4         1
```

## Detailed Metrics

```
   axis_id          scope                                   metric     value
   CMN-001         common                   state_restoration_rate 1.0000000
   CMN-002         common                    lineage_tracking_rate 1.0000000
   CMN-003         common                   cross_environment_rate 1.0000000
   CMN-004         common              long_term_degradation_ratio 0.8323586
   CMN-005         common                    rollback_cascade_rate 1.0000000
   CMN-006         common                  breakage_detection_rate 1.0000000
   CMN-007         common                 breakage_visualized_rate 1.0000000
   CMN-008         common             breakage_safe_execution_rate 1.0000000
  AUTO-001  auto_rollback           auto_restore_rate_among_broken 1.0000000
  AUTO-002  auto_rollback                 auto_restore_action_rate 1.0000000
 GUIDE-001  guided_manual                    guided_detection_rate 1.0000000
 GUIDE-002  guided_manual          guided_actionable_proposal_rate 1.0000000
 GUIDE-003  guided_manual                    guided_guardrail_rate 1.0000000
   LIM-001 limit_boundary                limit_false_positive_rate 0.0000000
   LIM-002 limit_boundary                limit_false_negative_rate 0.0000000
   LIM-003 limit_boundary limit_auto_restore_success_rate_expected 1.0000000
   LIM-004 limit_boundary         limit_known_limit_guardrail_rate 1.0000000
 target direction pass
   0.99        ge TRUE
   0.99        ge TRUE
   0.99        ge TRUE
   2.00        le TRUE
   0.99        ge TRUE
   0.95        ge TRUE
   0.95        ge TRUE
   1.00        ge TRUE
   0.95        ge TRUE
   1.00        ge TRUE
   0.95        ge TRUE
   0.95        ge TRUE
   1.00        ge TRUE
   0.10        le TRUE
   0.10        le TRUE
   0.90        ge TRUE
   0.90        ge TRUE
```

