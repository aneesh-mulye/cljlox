(ns cljlox.core
  (:require [cljlox.scanner :as scanner])
  (:gen-class))

(defn run [src]
  (let [[status tokens] (scanner/scan-tokens src)]
    (doseq [token tokens]
      (println token))
    status))

(defn run-file [fpath]
  (when (= :error (run (slurp fpath)))
    (System/exit 65)))

(defn run-prompt []
  (loop []
    (print "> ")
    (flush)
    (let [line (read-line)]
      (case line
        nil nil
        (do
          (run line)
          (recur))))))

(defn -main
  [& args]
  (cond
    ;; too many arguments
    (< 1 (count args) )
    (do 
      (println "Usage: jlox [script]")
      (flush)
      (System/exit 64))

    ;; run file
    (= (count args) 1)
    (run-file (first args))

    ;; default case; run a prompt/repl
    :else
    (run-prompt)))
