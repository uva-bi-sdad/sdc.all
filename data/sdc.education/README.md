# Social Data Commons: Education
Education-related measures from various sources:
- Daycare locations from [Virginia Department of Social Services](https://www.dss.virginia.gov) (VDSS)
- Secondary education locations from [National Center for Education Statistics](https://nces.ed.gov) (NCES)
- Secondary education attendance from the [American Community Survey](https://www.census.gov/programs-surveys/acs.html) (ACS)
- Reading test pass rates from the [Virginia Department of Education](https://www.doe.virginia.gov) (VDOE)

# Structure
This is a community data repository, created with the `community::init_repository()` function.
1. `{set}/code/distribution/ingest.R` should download and prepare data from a public source, and output files to `{set}/data/distribution`.
2. `{set}/data/distribution/measure_info.json` should contain metadata for each of the measures in the distribution data file(s).
3. `build.R` will convert the distribution data to site-ready versions, and `site.R` specifies the interface of the repository-specific data site.
