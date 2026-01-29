# Contributing to ClassiPyR

Thank you for your interest in contributing to `ClassiPyR`! This document provides guidelines for contributing to the project.

## Getting Started

### Prerequisites

- R (>= 4.0.0)
- devtools package for development
- Python with `scipy` (required for saving .mat annotation files)

### Setting Up the Development Environment

1. Fork the repository on GitHub

2. Clone your fork locally:
   ```bash
   git clone https://github.com/YOUR-USERNAME/ClassiPyR.git
   cd ClassiPyR
   ```

3. Install the package in development mode:
   ```r
   devtools::install_deps(dependencies = TRUE)
   devtools::load_all()
   ```

4. Set up Python environment (required for saving .mat annotation files):
   ```r
   library(iRfcb)
   ifcb_py_install(envname = "./venv")
   ```

### Running the App During Development

```r
# From the repository root
shiny::runApp()

# Or run from inst/app
shiny::runApp("inst/app")
```

## Package Structure

```
ClassiPyR/
├── R/                      # Package functions (exported)
│   ├── run_app.R           # App launcher
│   ├── utils.R             # Utility functions
│   ├── sample_loading.R    # Sample loading logic
│   └── sample_saving.R     # Sample saving logic
├── inst/
│   ├── app/                # Shiny application
│   │   ├── app.R           # Entry point
│   │   ├── global.R        # Initialization
│   │   ├── server.R        # Server logic
│   │   └── ui.R            # User interface
│   └── CITATION            # Citation info
├── tests/testthat/         # Test suite
├── vignettes/              # Documentation articles
├── man/                    # Function documentation (auto-generated)
└── docs/                   # pkgdown site (auto-generated)
```

## How to Contribute

### Reporting Bugs

Before submitting a bug report:
- Check existing issues to avoid duplicates
- Collect information about the bug (R version, OS, error messages)

When submitting a bug report, please include:
- A clear, descriptive title
- Steps to reproduce the issue
- Expected vs actual behavior
- R session info (`sessionInfo()` output)
- Screenshots if applicable

### Suggesting Features

Feature suggestions are welcome! Please:
- Check existing issues for similar suggestions
- Describe the feature and its use case
- Explain how it would benefit IFCB researchers

### Submitting Changes

1. Create a new branch for your feature or fix:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes following the code style guidelines below

3. Run tests and checks:
   ```r
   devtools::test()
   devtools::check()
   ```

4. Commit your changes with a descriptive message:
   ```bash
   git commit -m "feat: add support for new classification format"
   ```

5. Push to your fork:
   ```bash
   git push origin feature/your-feature-name
   ```

6. Open a Pull Request against the `main` branch

## Code Style Guidelines

### R Code

- Use meaningful variable and function names
- Document exported functions with roxygen2 comments
- Keep functions focused and modular
- Follow tidyverse style guide where applicable
- Aim for >80% test coverage on new code

### Commit Messages

Use conventional commit format:
- `feat:` new features
- `fix:` bug fixes
- `docs:` documentation changes
- `test:` test additions or modifications
- `refactor:` code refactoring

## Testing

Run the test suite before submitting:

```r
devtools::test()
```

Check for R CMD check issues:

```r
devtools::check()
```

Tests are located in `tests/testthat/`. When adding new functionality, please include appropriate tests.

## Documentation

- Function documentation uses roxygen2 (in R/ files)
- User guides are in `vignettes/` as R Markdown
- The pkgdown site is built automatically via GitHub Actions

To preview documentation locally:

```r
devtools::document()
pkgdown::build_site()
```

## Pull Request Process

1. Ensure all tests pass (`devtools::test()`)
2. Ensure R CMD check passes (`devtools::check()`)
3. Update documentation if needed
4. Describe your changes clearly in the PR description
5. Link any related issues
6. Be responsive to review feedback

## Questions?

Feel free to open an issue for any questions about contributing.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
