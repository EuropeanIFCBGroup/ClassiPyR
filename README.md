# ClassiPyR <a href="https://europeanifcbgroup.github.io/ClassiPyR/"><img src="man/figures/logo.png" align="right" height="138" alt="ClassiPyR website" /></a>

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/EuropeanIFCBGroup/ClassiPyR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EuropeanIFCBGroup/ClassiPyR/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/EuropeanIFCBGroup/ClassiPyR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/EuropeanIFCBGroup/ClassiPyR)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18414999.svg)](https://doi.org/10.5281/zenodo.18414999)

A Shiny application for manual (human) image classification and validation of Imaging FlowCytobot (IFCB) plankton images. Built for researchers who need to validate automated classifications or create training datasets for machine learning classifiers.

**Full documentation:** [europeanifcbgroup.github.io/ClassiPyR](https://europeanifcbgroup.github.io/ClassiPyR/)

## Background

`ClassiPyR` was created to provide a lightweight, standalone annotation and validation tool that is fully compatible with the [ifcb-analysis](https://github.com/hsosik/ifcb-analysis) toolbox and custom classifiers (e.g. a CNN). The primary design goals were user-friendliness and portability—enabling researchers to work with IFCB data without complex setup requirements or dependencies on specific computing environments (other than Python and R). To achieve these goals efficiently, [Claude Code](https://code.claude.com/) was used for development.

## Features

- **Dual Mode**: Validate existing classifications or annotate from scratch
- **Multiple Formats**: Load from CSV or MATLAB classifier output
- **Efficient Workflow**: Drag-select, batch relabeling, class filtering
- **MATLAB Compatible**: Export for [ifcb-analysis](https://github.com/hsosik/ifcb-analysis) toolbox
- **CNN Training Ready**: Organized PNG output by class
- **Measure Tool**: Built-in ruler for image measurements

## Installation

```r
install.packages("remotes")
remotes::install_github("EuropeanIFCBGroup/ClassiPyR")
```

`ClassiPyR` depends on [iRfcb](https://github.com/EuropeanIFCBGroup/iRfcb) for IFCB data handling, which is installed automatically.

### Python Setup

Python is required for saving annotations as MATLAB .mat files. If you only need to read existing .mat files or work with CSV files, this step is optional.

```r
library(iRfcb)
ifcb_py_install()
```

## Quick Start

```r
library(ClassiPyR)
run_app()

# Or specify a Python virtual environment (takes priority over saved settings)
run_app(venv_path = "/path/to/your/venv")
```

See the [Getting Started](https://europeanifcbgroup.github.io/ClassiPyR/articles/getting-started.html) guide for detailed setup instructions.

## Documentation

- [Getting Started](https://europeanifcbgroup.github.io/ClassiPyR/articles/getting-started.html) - First-time setup
- [User Guide](https://europeanifcbgroup.github.io/ClassiPyR/articles/user-guide.html) - Complete feature reference
- [Class List Management](https://europeanifcbgroup.github.io/ClassiPyR/articles/class-management.html) - Managing classes for ifcb-analysis
- [FAQ & Troubleshooting](https://europeanifcbgroup.github.io/ClassiPyR/articles/faq.html) - Common issues

## Citation

```r
citation("ClassiPyR")
```

## License

MIT License - see [LICENSE](LICENSE) file.

## Disclaimer

This software is provided for research and educational purposes. Users are responsible for their data and annotations. Always maintain backups of your original data.

This package was partly developed with the assistance of [Claude Code](https://code.claude.com/), an AI programming assistant by Anthropic.
