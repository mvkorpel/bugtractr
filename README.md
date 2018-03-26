# bugtractr

This package provides access to the [R](https://www.r-project.org/)
[bug tracker](https://bugs.r-project.org/) which runs on the
[Bugzilla](https://www.bugzilla.org/) platform. It may also work for
some other Bugzilla instances.

## Installation

With [remotes](https://github.com/r-lib/remotes) already installed,
run the following command in the R console:

```R
remotes::install_github("mvkorpel/bugtractr")
```

## Usage

After installing the package, see the help pages of the following functions:

- `list_bugs()`. List bugs in a Bugzilla database.
- `list_changed()`. List reports opened or closed in a given period.
- `bug_info()`. Get information about selected bugs.
- `bug_history()`. Get the change history of a bug.
