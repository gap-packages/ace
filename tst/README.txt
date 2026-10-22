Example inventory
-----------------
Ordinary: A5 (60), A5-C5 (12), C5-fel0 (5), F27-purec/F27-fel0/F27-fel1
(29), SL219-hard (180), perf602p5 (480). Presentations and strategies are
read from examples/, with explicit 2M-entry workspace caps and quiet
progress reporting. Complete tables are checked for permutation columns,
inverses, relators, subgroup stabilisation, transitivity and standardisation.
M12-hlt/M12-fel1 (95040) use statistics only: on macOS arm64 with ACE 3.001
and -O2 these took 0.01/0.04 engine CPU seconds at 2M workspace entries.
They belong in ordinary tests; no timing or intermediate-counter assertion.

Optional: 2p17-fel1a, 2p17-2p3-fel1, 2p17-2p14-fel1, 2p17-id-fel1.
These explore larger 2-groups and are not needed for the fast regression.
Deliberate failed-enumeration/recovery demonstrations: 2p17-fel1,
2p18-fel1, big-fel1, big-hard (starred in examples/index). Never run these
blindly as batch tests. session.tst supplies a small, controlled failure.

src/ex/ex000.in deliberately starts with bad input; ex001.in compares
Sims strategies; ex002.in demonstrates pure-row pitfalls; ex007.in is a
stepped-limit experiment. These transcripts contain algorithm-specific
counters and commands. standalone.tst instead generates real standalone
input from the same A5, A5-C5, F27-purec and SL219-hard fixtures and checks
successful indices and absence of engine errors.

aceds.tst preserves the original PerfectGroup subgroup-deletion reproducer,
including its call sequence, statistics checks and diagnostic transcript.
Its only change is an explicit 1M-entry workspace cap.
subgroup-recovery.tst adds separate checks of the invalid-deletion diagnostic,
unchanged subgroup, statistics fields, complete tables and adding the deleted
generator again. It sends no ACE query between the valid and invalid deletions.
Only the additional test's diagnostic capture and example chatter are
redirected; errors and mathematical checks are not discarded.

Workspace counts are entries, not bytes. Ordinary runs request at most
2 million entries (about 8 MB for the default 32-bit entry model), plus
GAP and engine overhead. The GAP ABI is independent of ACE's entry model.
