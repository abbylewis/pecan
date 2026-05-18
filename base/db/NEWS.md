# PEcAn.DB 1.8.2

## Fixed

* `arrhenius.scaling.traits()` and `filter_sunleaf_traits()`: both functions returned `NULL` instead of the input `data` unchanged when no matching covariates were found. This caused a hard crash (`argument is of length zero`) in `query.trait.data()` whenever temperature-dependent traits (Vcmax, respiration rates) were queried for species where no temperature covariate was recorded in the database. The documented behaviour ("data with no matching covariates will be unchanged") is now implemented correctly.
* `query.trait.data()`: the `warning()` call for missing trait data was placed after `return(NA)` and therefore never fired. Moved before the return and changed to `logger.warn()` for consistency with the rest of the codebase.

* Refactored `convert.input()` internals into smaller, and hopefully more testable, chunks. No user-visible changes expected.
* Roxygen cleanup.



# PEcAn.DB 1.8.1

## License change
* PEcAn.DB is now distributed under the BSD three-clause license instead of the NCSA Open Source license.

## Changed
* Fixed several cases where `dbfile.input.insert` continued instead of returning early
* Removed support for Browndog because the service is defunct 


# PEcAn.DB 1.8.0

## Added

* New functions `stamp_started` and `stamp_finished`, used to record the start
  and end time of model runs in the database. Both used to live in
  `PEcAn.remote` and were moved to resolve a circular dependency.
* New function `convert_input`, used to convert between formats while reusing
  existing files where possible. It previously lived in package `PEcAn.utils`,
  but was moved here to simplify dependencies. (#3026; @nanu1605)
* `get.trait.data` gains new argument `write` (with default FALSE), passed on to `get.trait.data.pft` (@Aariq, #3065).

# PEcAn.DB 1.7.2

## Removed

* `rename_jags_columns()` has been removed from `PEcAn.DB` but is now available
  in package `PEcAn.MA` (#2805, @moki1202).


# PEcAn.DB 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of
  the PEcAn packages; please see
  https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
