(ns comments.async
  (require [clojure.core.async :as async :refer (<! <!! >!! >! close! go alt! alt!! alts! alts!! buffer chan put! take! thread timeout dropping-buffer sliding-buffer)]))

