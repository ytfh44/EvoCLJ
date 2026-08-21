(ns evoclj.fs.snapshot
  "Generic immutable tree snapshot for any directory.

  Unlike Genome hashing (CRLF-normalized text hashing), Skill snapshots
  preserve exact raw bytes so upstream package identity and our revision
  identity stay aligned. Every file is stored verbatim in CAS.

  Snapshot produces a canonical tree manifest:

    {:tree/version 1
     :entries {\"SKILL.md\" {:artifact/id \"sha256:...\" :size 123}
               \"references/x.md\" {:artifact/id \"sha256:...\" :size 456}}}

  The manifest itself is stored in CAS and its artifact id is the tree id."
  (:require [clojure.edn :as edn]
            [evoclj.fs.walk :as walk]
            [evoclj.kernel.error :as err]
            [evoclj.store.cas :as cas])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files Path)))

(defn- check-limits!
  [entries limits]
  (let [{:keys [max-depth max-files max-total-bytes max-file-bytes]} limits]
    (when (and max-files (> (count entries) max-files))
      (throw (err/error :fs/snapshot-limit-exceeded "max-files exceeded"
                        {:limit max-files :actual (count entries)})))
    (when max-depth
      (doseq [{:keys [path]} entries]
        (let [depth (count (clojure.string/split path #"/"))]
          (when (> depth max-depth)
            (throw (err/error :fs/snapshot-limit-exceeded "max-depth exceeded"
                              {:path path :depth depth :limit max-depth}))))))
    (when max-file-bytes
      (doseq [{:keys [size path]} entries]
        (when (> size max-file-bytes)
          (throw (err/error :fs/snapshot-limit-exceeded "max-file-bytes exceeded"
                            {:path path :size size :limit max-file-bytes})))))
    (when max-total-bytes
      (let [total (reduce + 0 (map :size entries))]
        (when (> total max-total-bytes)
          (throw (err/error :fs/snapshot-limit-exceeded "max-total-bytes exceeded"
                            {:total total :limit max-total-bytes})))))))

(defn snapshot-tree!
  "Snapshot root directory to CAS under limits.

  root — Path or string
  cas  — CAS config (see evoclj.store.cas/->cas)
  limits — map with optional keys: :max-depth :max-files :max-total-bytes :max-file-bytes

  Returns {:tree/id \"sha256:...\" :manifest {...} :entries {...}}.

  Each file is stored with exact raw bytes (no CRLF normalization).
  Manifest is stored as EDN bytes in CAS."
  [root cas limits]
  (let [entries-raw (walk/walk-tree root)
        ;; read each file with exact bytes and put to CAS
        entries (mapv (fn [{:keys [path physical-path]}]
                        (let [^Path p physical-path
                              ba (Files/readAllBytes p)
                              {:keys [artifact/id size]} (cas/put-bytes! cas ba {:media-type "application/octet-stream"})]
                          {:path path :artifact/id id :size size :physical-path p}))
                      entries-raw)
        _ (check-limits! entries limits)
        ;; canonical ordering for deterministic manifest
        sorted (sort-by :path (fn [a b] (compare a b)) entries)
        manifest {:tree/version 1
                  :entries (into {} (map (fn [{:keys [path artifact/id size]}] [path {:artifact/id id :size size}]) sorted))}
        manifest-bytes (.getBytes (pr-str manifest) StandardCharsets/UTF_8)
        {:keys [artifact/id]} (cas/put-bytes! cas manifest-bytes {:media-type "application/edn"})]
    {:tree/id id
     :manifest manifest
     :entries sorted
     :size (count sorted)}))

(defn load-tree
  "Load a tree manifest from CAS by tree id."
  [cas tree-id]
  (let [ba (cas/get-bytes cas tree-id)
        s (String. ba StandardCharsets/UTF_8)]
    (edn/read-string s)))

(defn get-file-bytes
  "Get raw bytes of a file inside a tree via CAS."
  [cas tree-manifest path]
  (when-let [{:keys [artifact/id]} (get-in tree-manifest [:entries path])]
    (cas/get-bytes cas id)))
