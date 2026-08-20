# MESC AGENTS.md file

## Basic information

- MESC a soil carbon cycle model written in Fortran.
- Source code for MESC can be found in the `src/` subdirectory.
- Auxiliary code can be found in the `auxil/` subdirectory, but it isn't used at present.
- Pre-processing code can be found in the `pre-processing/` subdirectory, but it isn't used at present.
- Post-processing code can be found in the `post-processing/` subdirectory, but it isn't used at present.
- The test suite is configured in the `test/` subdirectory.

## Testing instructions

### Test suite

- Find the CI plan in the `.github/workflows` folder.
- Ensure access to a Fortran compiler, netCDF-C, and netCDF-Fortran to run the tests.
- Run `./build.sh` from the package root to build the MESC model.
- To run the MESC test suite, navigate to the `test/` subdirectory and run `./run_main.sh`.

### Developer tasks

- Run `fortitude check src/` apply the Fortitude linter to the source code.
  - Run `fortitude check --fix src/` to apply automated fixes implemented by Fortitude.
- Run `ford mesc.md` to build the API documentation using FORD.

## PR instructions

- Before committing changes to source code, configuration, or tests, run the test suite and fix any errors it reports.
- Before committing changes to Fortran source, apply the Fortitude linter and fix any errors it reports.
- Before committing changes to the `src/` subdirectory, generate the FORD docs and fix any errors or warnings.
