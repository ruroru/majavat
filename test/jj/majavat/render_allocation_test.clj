(ns jj.majavat.render-allocation-test
  "Allocation ('GC') regression guard for the render hot path.

   We can't assert on garbage collection directly, but we can measure the bytes
   a thread allocates while rendering (com.sun.management.ThreadMXBean) and hold
   it to a budget.

   This guards the class of regression introduced by d358de5, where
   string-builder/append took an untyped argument so every `.append` became a
   reflective call. That inflated allocation by a fixed amount per render (a
   StringBuilder is built per escaped value, and reflective appends allocate
   where the typed overload does not). Because the extra cost is constant per
   render rather than per row, a per-row differential would cancel it out, so we
   assert on the absolute bytes allocated for a fixed template.

   On the reference JVM the healthy render allocates ~4.2 KB. When this guard
   was written that figure was ~7.5 KB and the reflective regression pushed it
   to ~14.3 KB; the baseline has since come down because the renderers size
   their output buffer from the last render rather than growing into it. The
   budget is set just above the healthy figure (small headroom for JVM/GC
   version drift) so it trips early -- keep it tight when the baseline moves,
   or it stops catching the regression it exists for. If a future JVM shifts
   the healthy baseline, re-run with the value printed below and adjust."
  (:require [clojure.test :refer [deftest is]]
            [jj.majavat :as majavat]
            [jj.majavat.renderer.sanitizer :as san])
  (:import (java.lang.management ManagementFactory)
           (com.sun.management ThreadMXBean)))

(def ^:private ^ThreadMXBean tmx
  (let [b (ManagementFactory/getThreadMXBean)]
    (when (instance? ThreadMXBean b) b)))

(defn- allocation-supported? []
  (and tmx
       (.isThreadAllocatedMemorySupported tmx)
       (.isThreadAllocatedMemoryEnabled tmx)))

(defn- bytes-per-op
  "Mean bytes allocated on the current thread per call of (f), over n iterations,
   after a warmup pass so the measurement reflects JIT-compiled steady state."
  [f n]
  (dotimes [_ n] (f))
  (let [tid    (.getId (Thread/currentThread))
        before (.getThreadAllocatedBytes tmx tid)]
    (dotimes [_ n] (f))
    (/ (double (- (.getThreadAllocatedBytes tmx tid) before)) n)))

;; TechEmpower Fortunes shape: a loop of HTML-escaped rows.
(def ^:private template
  (str "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table>"
       "<tr><th>id</th><th>message</th></tr>"
       "{% for f only in fortunes %}<tr><td>{{ f.id }}</td><td>{{ f.message }}</td></tr>{% endfor %}"
       "</table></body></html>"))

(def ^:private fortunes
  {:fortunes
   [{:id 11 :message "<script>alert(\"This should not be displayed in a browser alert box.\");</script>"}
    {:id 4  :message "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1"}
    {:id 5  :message "A computer program does what you tell it to do, not what you want it to do."}
    {:id 2  :message "A computer scientist is someone who fixes things that aren't broken."}
    {:id 8  :message "A list is only as strong as its weakest link."}
    {:id 0  :message "Additional fortune added at request time."}
    {:id 3  :message "After enough decimal places, nobody gives a damn."}
    {:id 7  :message "Any program that runs right is obsolete."}
    {:id 10 :message "Computers make very fast, very accurate mistakes."}
    {:id 6  :message "Emacs is a nice operating system, but I prefer UNIX."}
    {:id 9  :message "Feature: A bug with seniority."}
    {:id 1  :message "fortune: No such file or directory"}]})

;; Healthy baseline is ~4184 bytes, identical across fresh JVMs; kept tight to
;; catch a reflective-append regression early, with headroom for JVM/GC drift.
(def ^:private budget-bytes 4400.0)

(deftest render-allocation-within-budget
  (if-not (allocation-supported?)
    (println "Thread allocation measurement unsupported/disabled on this JVM; skipping GC test")
    (let [render (majavat/build-string-renderer template {:sanitizer (san/->Html)})]
      (is (re-find #"&lt;script&gt;" (str (render fortunes))) "sanity: output is HTML-escaped")
      (let [per-op (bytes-per-op #(render fortunes) 2000)]
        (println (format "render allocation: %.0f bytes per render (budget %.0f)" per-op budget-bytes))
        (is (< per-op budget-bytes)
            (format (str "Rendering allocates %.0f bytes (budget %.0f). A per-node append is likely "
                         "reflecting -- check that string-builder/append dispatches to a typed "
                         "StringBuilder overload rather than an untyped, reflective .append "
                         "(regression of d358de5).")
                    per-op budget-bytes))))))
