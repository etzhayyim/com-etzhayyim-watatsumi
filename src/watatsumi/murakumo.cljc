(ns watatsumi.murakumo
  "Pure cljc actor boundary generated from manifest migration scaffold."
  (:require [clojure.string :as str]))

(def actor-did
  "did:web:etzhayyim.com:watatsumi")

(def common-gates
  [:council-charter-attestation
   :no-platform-held-key-baseline
   :no-probing-baseline
   :murakumo-only-inference-baseline
   :did-primary-baseline
   :append-only-gate-baseline
   :kotoba-only-substrate-baseline])

(defn collection
  [name]
  (str "com.etzhayyim.watatsumi." name))

(def cell-specs {
  :hull_ring_fabrication {:legacy-cell "hull-ring-fabrication"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "hull_ring_fabrication")]
     :required-gates common-gates
     :trigger "manifest cell hull_ring_fabrication"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :section_assembly {:legacy-cell "section-assembly"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "section_assembly")]
     :required-gates common-gates
     :trigger "manifest cell section_assembly"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :weld_inspection {:legacy-cell "weld-inspection"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "weld_inspection")]
     :required-gates common-gates
     :trigger "manifest cell weld_inspection"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :system_integration {:legacy-cell "system-integration"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "system_integration")]
     :required-gates common-gates
     :trigger "manifest cell system_integration"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :section_joining {:legacy-cell "section-joining"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "section_joining")]
     :required-gates common-gates
     :trigger "manifest cell section_joining"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :pressure_test {:legacy-cell "pressure-test"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "pressure_test")]
     :required-gates common-gates
     :trigger "manifest cell pressure_test"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :sea_trial {:legacy-cell "sea-trial"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "sea_trial")]
     :required-gates common-gates
     :trigger "manifest cell sea_trial"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :marine_emissions_audit {:legacy-cell "marine-emissions-audit"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "marine_emissions_audit")]
     :required-gates common-gates
     :trigger "manifest cell marine_emissions_audit"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
  :class_certification_binder {:legacy-cell "class-certification-binder"
     :phase :event
     :murakumo-node "reuben"
     :collections [(collection "class_certification_binder")]
     :required-gates common-gates
     :trigger "manifest cell class_certification_binder"
     :ceiling "Manifest-driven migration scaffold; explicit execution stays in runtime methods"}
})

(defn safe-rkey
  [s]
  (let [clean (-> (str s)
                  (str/replace #"^did:web:" "")
                  (str/replace #"[^A-Za-z0-9._~-]" "-"))]
    (if (str/blank? clean) "unknown" clean)))

(defn gate-value
  [attestations gate]
  (or (get attestations gate)
      (get attestations (name gate))
      (when (set? attestations) (attestations gate))
      (when (set? attestations) (attestations (name gate)))))

(defn missing-gates
  [spec attestations]
  (->> (:required-gates spec)
       (remove #(boolean (gate-value attestations %)))
       vec))

(defn put-record-effect
  [collection rkey record]
  {:op :mst/put-record
   :actor actor-did
   :collection collection
   :rkey rkey
   :record record})

(defn records-for
  [spec {:keys [records record computed-at request-id]
         :as input}]
  (let [input-records (cond
                        (map? records) records
                        (some? record) {0 record}
                        :else {})
        base {:actorDid actor-did
              :computedAt computed-at
              :legacyCell (:legacy-cell spec)
              :phase (:phase spec)
              :requestId request-id
              :actorBoundary "cljc-migration-scaffold"
              :scaffold true
              :constitutionalStatus "attested-plan"}]
    (map-indexed
     (fn [idx coll]
       (let [record* (merge {:$type coll}
                            base
                            (or (get input-records coll)
                                (get input-records idx)
                                {}))
             rkey (safe-rkey (or (:rkey record*)
                                 (get record* "rkey")
                                 (:tid record*)
                                 request-id
                                 (str (:legacy-cell spec) "-" idx)))]
         {:collection coll
          :record record*
          :rkey rkey}))
     (:collections spec))))

(defn cell-plan
  [cell-key {:keys [attestations] :as input}]
  (let [spec (get cell-specs cell-key)]
    (when-not spec
      (throw (ex-info "unknown cell" {:cell cell-key})))
    (let [missing (missing-gates spec attestations)]
      (merge
       {:cell cell-key
        :legacy-cell (:legacy-cell spec)
        :actor actor-did
        :phase (:phase spec)
        :murakumo-node (:murakumo-node spec)
        :trigger (:trigger spec)
        :ceiling (:ceiling spec)
        :required-gates (:required-gates spec)
        :missing-gates missing}
       (if (seq missing)
         {:status :blocked
          :effects []}
         (let [planned-records (records-for spec input)]
           {:status :ready
            :records (vec planned-records)
            :effects (mapv (fn [{:keys [collection record rkey]}]
                             (put-record-effect collection rkey record))
                           planned-records)}))))))

(defn all-cell-plans
  [input]
  (into {}
        (map (fn [cell-key] [cell-key (cell-plan cell-key input)]))
        (keys cell-specs)))
