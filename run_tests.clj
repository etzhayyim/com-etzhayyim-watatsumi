(require '[clojure.test :as test])
(def test-namespaces
  '[watatsumi.cells.class-certification-binder.test-state-machine
    watatsumi.cells.hull-ring-fabrication.test-state-machine
    watatsumi.cells.marine-emissions-audit.test-state-machine
    watatsumi.cells.pressure-test.test-state-machine
    watatsumi.cells.sea-trial.test-state-machine
    watatsumi.cells.section-assembly.test-state-machine
    watatsumi.cells.section-joining.test-state-machine
    watatsumi.cells.system-integration.test-state-machine
    watatsumi.cells.weld-inspection.test-state-machine
    watatsumi.methods.test-agent
    watatsumi.methods.test-charter-gates
    watatsumi.methods.test-manifest-invariants
    watatsumi.murakumo-test
    watatsumi.repository-contract-test])
(doseq [namespace test-namespaces] (require namespace))
(let [result (apply test/run-tests test-namespaces)]
  (println "==> watatsumi:" (select-keys result [:test :pass :fail :error]))
  (when (pos? (+ (:fail result) (:error result))) (System/exit 1)))
