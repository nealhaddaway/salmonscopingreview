# Update inputs

Place each new database-search export in `data_updates/incoming/` unchanged.

The update workflow then:

1. archives the current master;
2. archives the incoming files byte-for-byte under the update ID;
3. processes the update separately;
4. records PRISMA/ROSES flow counts in `data_archive/manifests/`;
5. creates and validates a candidate new master;
6. promotes the validated candidate to `data_current/salmon_evidence_map.csv`.

Do not edit incoming search exports after they have been used to start an update. The Lens.org update currently stored here is the existing Update 001 input and should not be re-imported as a new search.