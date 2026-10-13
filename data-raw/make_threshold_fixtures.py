"""Generate the class-score fixtures used by the threshold-review tests.

The fixtures are written by the real ifcb-classify writer
(``ifcb_classify.hdf5_output.write_class_scores``) with real provenance
(``ifcb_classify.provenance.build_provenance``), so the R reader is tested
against the bytes the pipeline actually produces rather than against files
hdf5r wrote itself.

Usage (from the package root, in an environment with ifcb-classify installed):

    python data-raw/make_threshold_fixtures.py tests/testthat/test_data/threshold_review

The folder is deliberately not called ``threshold``: ``R CMD build`` drops every
directory whose name ends in ``old``, so the fixtures would be missing from the
built package and the tests would only pass from the source tree.

Output is deterministic: re-running reproduces the same scores. ``expected.json``
records, per file, what numpy computed for the argmax class and the top score,
which is the ground truth the R tests compare against.
"""

import json
import sys
import tempfile
from pathlib import Path

import numpy as np

from ifcb_classify.hdf5_output import write_class_scores
from ifcb_classify.provenance import build_provenance

# One label with a slash, one ending in a three-digit suffix and one non-ASCII:
# each has a code path elsewhere in ClassiPyR that would rewrite it, and the
# thresholds export must keep them byte for byte.
CLASS_LABELS = [
    "Skeletonema_spp",
    "Snowella/Woronichinia",
    "Pauliella_taeniata_001",
    "Öresund_ciliate",
    "Debris",
]
TRANSFORM = "dataset_squarepad"
ARCHITECTURE = "resnet50"
THRESHOLDS_A = np.array([0.3, 0.05, 0.9, np.nan, 0.6])
THRESHOLDS_A_ALT = np.array([0.5, 0.5, 0.5, 0.5, 0.5])
THRESHOLDS_B = np.array([0.2, 0.2, 0.2, 0.2, 0.2])

# ROI numbers of the real raw bin shipped in tests/testthat/test_data/raw, so
# one fixture can be paired with images that actually extract.
REAL_SAMPLE = "D20220522T000439_IFCB134"
REAL_ROI_NUMBERS = [2, 3, 4, 5, 6, 7, 8]


def _scores(rng: np.random.Generator, n_rois: int) -> np.ndarray:
    """Softmax-like rows whose top scores spread over the whole 0-1 range."""
    n_classes = len(CLASS_LABELS)
    sharpness = rng.choice([0.3, 1.0, 5.0, 40.0, 400.0], size=n_rois)
    logits = rng.normal(size=(n_rois, n_classes)) * sharpness[:, None]
    exp = np.exp(logits - logits.max(axis=1, keepdims=True))
    return exp / exp.sum(axis=1, keepdims=True)


def _tie_scores() -> np.ndarray:
    """Five ROIs by five classes, with an exact two-way tie in the first row.

    Square on purpose: a reader that guesses orientation from the dimensions
    cannot tell rows from columns here. And deliberately not symmetric — read
    transposed, the winning classes come out as 3, 1, 2, 4, 5 instead of
    1, 3, 1, 4, 5, and the tie disappears.
    """
    scores = np.array([
        [0.40, 0.40, 0.10, 0.05, 0.05],
        [0.02, 0.03, 0.90, 0.03, 0.02],
        [0.60, 0.10, 0.10, 0.10, 0.10],
        [0.00, 0.00, 0.00, 1.00, 0.00],
        [0.10, 0.20, 0.10, 0.10, 0.50],
    ])
    return scores


def _write(out_dir, sample, scores, roi_numbers, classifier, thresholds, provenance, expected):
    path = out_dir / f"{sample}_class.h5"
    path.parent.mkdir(parents=True, exist_ok=True)
    write_class_scores(
        path, scores, CLASS_LABELS, np.asarray(roi_numbers), classifier, thresholds,
        provenance=provenance,
    )
    top = scores.argmax(axis=1)
    expected[str(path.relative_to(expected_root))] = {
        "sample_name": sample,
        "classifier_name": classifier,
        "roi_numbers": [int(r) for r in roi_numbers],
        "class_idx": [int(i) + 1 for i in top],
        "class_name_auto": [CLASS_LABELS[i] for i in top],
        "top_score": [float(s) for s in scores.max(axis=1)],
        "thresholds": [None if np.isnan(t) else float(t) for t in thresholds],
        "checkpoint_sha256": provenance.get("checkpoint_sha256"),
        "transform": provenance.get("transform"),
    }


def main(out_dir: Path) -> None:
    global expected_root
    expected_root = out_dir
    rng = np.random.default_rng(20260808)
    expected: dict = {}

    with tempfile.TemporaryDirectory() as tmp:
        ckpt_a, ckpt_b = Path(tmp) / "model_a.pt", Path(tmp) / "model_b.pt"
        ckpt_a.write_bytes(b"threshold-review fixture checkpoint A")
        ckpt_b.write_bytes(b"threshold-review fixture checkpoint B")
        prov_a = build_provenance(TRANSFORM, ARCHITECTURE, ckpt_a)
        prov_b = build_provenance(TRANSFORM, ARCHITECTURE, ckpt_b)

    model_a = out_dir / "model_a"
    for sample, n_rois in [("D20240101T000000_IFCB134", 20), ("D20240102T000000_IFCB134", 20)]:
        rois = np.arange(2, 2 + 2 * n_rois, 2)
        _write(model_a, sample, _scores(rng, n_rois), rois, "fixture_A", THRESHOLDS_A, prov_a, expected)

    # Same model, but the run that wrote this file applied other thresholds.
    _write(model_a, "D20240103T000000_IFCB134", _scores(rng, 12), np.arange(1, 13),
           "fixture_A", THRESHOLDS_A_ALT, prov_a, expected)
    _write(model_a, "D20240104T000000_IFCB134", _scores(rng, 1), [7],
           "fixture_A", THRESHOLDS_A, prov_a, expected)
    _write(model_a, "D20240105T000000_IFCB134", _tie_scores(), [1, 2, 3, 4, 5],
           "fixture_A", THRESHOLDS_A, prov_a, expected)
    _write(model_a, REAL_SAMPLE, _scores(rng, len(REAL_ROI_NUMBERS)), REAL_ROI_NUMBERS,
           "fixture_A", THRESHOLDS_A, prov_a, expected)

    # A second model scoring a bin the first model also scored.
    _write(out_dir / "model_b", "D20240101T000000_IFCB134", _scores(rng, 20),
           np.arange(2, 42, 2), "fixture_B", THRESHOLDS_B, prov_b, expected)

    with open(out_dir / "expected.json", "w", encoding="utf-8") as f:
        json.dump(
            {"class_labels": CLASS_LABELS, "files": expected},
            f, indent=2, ensure_ascii=False,
        )


if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit(__doc__)
    main(Path(sys.argv[1]))
