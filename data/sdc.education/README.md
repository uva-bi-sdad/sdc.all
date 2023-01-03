# Social Data Commons: Education
Education-related measures from the [National Center for Education Statistics](https://nces.ed.gov) (NCES).

# Structure
This is a community data repository, created with the `community::init_repository()` function.
1. Scripts in `code/{set}` should download and prepare data from a public source, and output files to `data/{set}/distribution`.
2. `data/{set}/distribution/measure_info.json` should contain metadata for each of the measures in the distribution data file(s).
3. `build.R` will convert the distribution data to site-ready versions, and `site.R` specifies the interface of the repository-specific data site.
