# Initialize Python environment for iRfcb

**\[deprecated\]**

`init_python_env()` was deprecated in ClassiPyR 0.3.0 and is now a
no-op. As of iRfcb 0.10.0, MATLAB .mat files are read and written with a
native R implementation, so ClassiPyR no longer requires Python.

If you need a Python environment for other iRfcb features (e.g. feature
extraction), set one up with
[`iRfcb::ifcb_py_install()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_py_install.html).

## Usage

``` r
init_python_env(venv_path = NULL)
```

## Arguments

- venv_path:

  Ignored.

## Value

FALSE, invisibly
