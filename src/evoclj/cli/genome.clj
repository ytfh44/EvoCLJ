(ns evoclj.cli.genome
  "The genome-facing CLI commands (Task 10.2): `genome validate`,
  `genome inspect`, and `genome diff`.

  All three are READ-ONLY and build no host system: `validate` loads
  a bundle from a path, `inspect` loads a bundle from a path OR a
  content address resolved through the CLI's genome store
  (<state-dir>/genomes or <state-dir>/candidates), and `diff`
  compares two bundles (by path or by id) at the file level — the
  canonical content-address comparison. Genome loading always runs
  through evoclj.genome.load (schema-validated, symlink-rejected,
  deterministic hashing — Global Constraints 1, 6)."
  (:require [clojure.java.io :as io]
            [evoclj.cli.session :as session]
            [evoclj.genome.load :as load]
            [evoclj.kernel.error :as err]))

;; --- shared arg resolution ---------------------------------------------------

(def ^:private genome-id-re #"^sha256:[0-9a-f]{64}$")

(defn- genome-id? [s] (boolean (re-matches genome-id-re s)))

(defn- positional
  "The nth positional arg of the command, or a usage error."
  [opts n]
  (let [pos (:positionals opts)]
    (or (nth pos n nil)
        (throw (err/error :cli/usage-invalid
                          "missing positional argument"
                          {:usage (str "expected " (inc n) " positional argument(s)")})))))

(defn- resolve-genome
  "Load a genome from an id-or-path argument: a content address
  resolves through the CLI genome store (:cli/genome-not-found when
  absent), an existing directory path loads directly, anything else
  fails with the loader's typed error."
  [opts arg]
  (if (or (genome-id? arg) (not (.isDirectory (io/file arg))))
    (load/load-genome (session/resolve-bundle-root opts arg))
    (load/load-genome arg)))

;; --- commands ----------------------------------------------------------------

(defn validate!
  "evoclj genome validate <path>

  Validate the bundle at <path> (schema, symlinks, declared modules,
  deterministic hashing) and report its content address."
  [opts]
  (let [path (positional opts 0)
        loaded (load/load-genome path)]
    {:genome/id (:genome/id loaded)
     :valid? true
     :manifest (:manifest loaded)
     :files (count (:files loaded))}))

(defn inspect!
  "evoclj genome inspect <id-or-path>

  Inspect a bundle by content address (resolved through the CLI
  genome store) or by directory path: its address, root, manifest, and
  every file's digest/kind (the raw payload bytes are stripped —
  Global Constraint 21 keeps payload bodies out of output rows)."
  [opts]
  (let [arg (positional opts 0)
        loaded (resolve-genome opts arg)]
    {:genome/id (:genome/id loaded)
     :root (str (:genome/root loaded))
     :manifest (:manifest loaded)
     :files (into (sorted-map)
                  (map (fn [[p f]] [p (assoc (dissoc f :bytes) :path p)]))
                  (:files loaded))}))

(defn diff!
  "evoclj genome diff <left> <right>

  Compare two bundles (each a content address or a directory path) at
  the file level: paths added in <right> (:added), removed from
  <left> (:removed), and present in both with a different digest
  (:changed, with both digests). :identical? is true when the two
  bundles have the same content address."
  [opts]
  (let [left (resolve-genome opts (positional opts 0))
        right (resolve-genome opts (positional opts 1))
        lf (:files left)
        rf (:files right)
        paths (into (sorted-set) (concat (keys lf) (keys rf)))
        changed (->> paths
                     (keep (fn [p]
                             (when (and (contains? lf p) (contains? rf p)
                                        (not= (get-in lf [p :digest])
                                              (get-in rf [p :digest])))
                               {:path p
                                :left/digest (get-in lf [p :digest])
                                :right/digest (get-in rf [p :digest])})))
                     (sort-by :path)
                     vec)
        added (vec (sort (remove #(contains? lf %) paths)))
        removed (vec (sort (remove #(contains? rf %) paths)))]
    {:left/genome-id (:genome/id left)
     :right/genome-id (:genome/id right)
     :identical? (and (empty? added) (empty? removed) (empty? changed))
     :added added
     :removed removed
     :changed changed}))
