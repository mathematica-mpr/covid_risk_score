

# Changelog
All notable changes to the to the api.covid19.mathematica.org API will be documented in this file.

## [1.16.0] - 2021-12-20
### Added
- update the province-level adjusted case fatality rate for Belgium using data until 2021-12-07
### Changed
- modify the vaccination section to 1) account for the waning vaccine effectiveness against infection and severe illness over time and 2) account for the effect of booster shots
- days_since_last_dose deprecated. Please use months_last_vaccination instead.

## [1.15.0] - 2021-11-16
### Added
- update the age-specific risks based on CDC surveillance data from 2021-03-27 to 2021-09-24
- update the province-level adjusted case fatality rate for Belgium using data until 2021-10-28

## [1.14.0] - 2021-10-07
### Added
- update the age-specific risks based on CDC surveillance data from 2021-02-02 to 2021-08-01
- update household transmissibility to the transmissibility of the delta variant 
- update vaccines' efficacy against delta variant infection
- calculate province-level adjusted case fatality rate for Belgium
### Changed
- file names for more clarity between scripts that process US data and scripts that process Belgium data
### Fixed
- return error message if invalid country parameter is provided

## [1.13.0] - 2021-08-11
### Added
- add AstraZeneca vaccine
- add covid data for belgium postal codes and nis codes
- add new optional input parameter "country" which takes the 2 letter country code. If no country code is provided, the default value is "us"

## [1.12.0] - 2021-07-15
### Added
- add vaccine effectiveness against hospitalization, icu, and mortality
- add AstraZeneca vaccine

## [1.11.0] - 2021-05-26
### Added
- add exercise level as input to the API

## [1.10.0] - 2021-05-25
### Added
- Add down syndrome to underlying medical complications which effect risk of sever COVID-19 outcomes
- refactor how condition parameter is validated
### Changed
- Compute age-specific risks based on CDC surveillance data from 2020-10-25 to 2021-04-24
### Fixed
- bug: specify python package numpy version 

## [1.9.0] - 2021-05-03
### Added
- Add risk of COVID-19 exposure from users activities

## [1.8.0] - 2021-03-16
### Added
- add Johnson and Johnson vaccine as possible vaccine input

## [1.7.0] - 2021-01-15
### Added
-  Reduce risk, if any, of contracting COVID-19 based on vaccination status to calculator for Pfizer and Moderna COVID-19 Vaccine

## [1.6.0] - 2020-12-29
### Added
- Added three new comorbidities (pregnancy, cancer, sickle cell disease)
- Added FIPS code as an optional parameter as an alternative to zipcode
### Changed
- Updated odds ratios for comorbidities

## [1.5.0] - 2020-11-18
### Changed
- change conditions names from `['is_renal', 'is_cvd', 'is_diabetes', 'is_hyper', 'is_smoker', 'is_immune', 'is_lung', 'is_obesity', 'is_other']` to `['renal_disease', 'cardiovascular_disease', 'diabetes', 'hypertension', 'smoking', 'immunocompromised', 'lung_disease', 'obesity', 'other']`

## [1.4.0] - 2020-11-10
### Fixed
- handle case where exposure_risk=0 in calc_precaution_delta

## [1.3.0] - 2020-10-29
### Added
- modify risk of COVID-19 exposure risk based on if user takes the precautions of washes hands or wears personal protective equipment.
### Changed
- add Getting Started and Testing sections to README files

## [1.2.0] - 2020-10-20
### Added
- add validation checks for API input parameters 
### Changed
- update README files with project information 

## [1.1.0] - 2020-10-10
### Changed
- get file with county level COVID-19 cases and death data from the `math-api-covid-risk-score-etl` repo

## [1.0.0] - 2020-10-06
- first release!


[1.11.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.10.0...v1.11.0
[1.10.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.9.0...v1.10.0
[1.9.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.8.0...v1.9.0
[1.8.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.7.0...v1.8.0
[1.7.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.6.0...v1.7.0
[1.6.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.5.0...v1.6.0
[1.5.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.4.0...v1.5.0
[1.4.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/mathematica-mpr/math-api-covid-risk-calculator/releases/tag/v1.0.0 
