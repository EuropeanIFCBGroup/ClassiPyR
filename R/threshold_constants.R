# Shared constants for the threshold review app (run_threshold_app())

# Lower edges of the score bins a predicted class is stratified into. Bin b is
# [edge[b], edge[b + 1]), and the last bin runs up to and including 1. These
# edges are also the only thresholds the fit can choose, so that every
# estimate is built from whole bins: ifcb-classify keeps a prediction when
# `score >= threshold`, which at an edge is exactly the bins from that edge up.
#
# The spacing is uneven on purpose. Most winning scores sit just below 1, so
# the top is cut finely; the low end is kept at 0.1 steps because many fitted
# thresholds of imprecise classes land there.
THR_SCORE_EDGES <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 0.8, 0.9,
                     0.95, 0.98, 0.99, 0.995, 0.999, 0.9999)

# Minimum evidence before a threshold is fitted rather than carried over from
# the model: verdicts for the class as a whole, and per score bin at or above
# the candidate cut (or the whole bin, when it holds fewer predictions).
THR_MIN_CLASS_VERDICTS <- 50L
THR_MIN_STRATUM_VERDICTS <- 10L

# Target precision offered by default when choosing thresholds
THR_DEFAULT_TARGET_PRECISION <- 0.9

# File names inside the review folder. Verdicts are the durable record; the
# index is a cache of the class-score files and can always be rebuilt.
THR_VERDICT_DB_FILE <- "threshold_verdicts.sqlite"
THR_INDEX_DB_FILE <- "threshold_index.sqlite"
