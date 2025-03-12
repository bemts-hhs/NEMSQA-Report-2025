<!-- README.md is generated from README.Rmd. Please edit that file -->

# NEMSQA Report 2025 <img src="man/figures/README/HHS PFP 5.png" align="right" style="margin-left: auto; margin-right: auto; width: 200px;" alt="HHS PFP Logo" />

The NEMSQA Report 2025 analyzes National EMS Quality Alliance (NEMSQA) measures from 2021-2024 for the state of Iowa, assessing EMS service quality. This project uses the {`nemsqar`} R package, developed to facilitate standardized EMS quality reporting.

This report is completed by the Bureau of Emergency Medical and Trauma Services (BEMTS) epidemiologist to gauge Iowa EMS agencys' performance against the NEMSQA national benchmarks.  In this way, BEMTS can assess patient care quality using standardized measures. The {`nemsqar`} package facilitates easy measure calculation for continuous quality improvement.

BEMTS exists within the Division of Public Health at Iowa Department of Health and Human Services (HHS).

## Installation
To install the {`nemsqar`} package:

### Install from GitHub
```r
remotes::install_github("bemts-hhs/nemsqar")
```

### Install from CRAN
```r
install.packages("nemsqar")
```

## NEMSQA Measure Set

The National EMS Quality Alliance (NEMSQA) Measure Set includes a comprehensive list of EMS quality measures that have been developed and approved for national use. These measures are intended to guide EMS agencies in collecting data to assess and improve patient care. Below are the measures approved by the NEMSQA Measure Development Committee and Board of Trustees as of August 2019. Access to the full technical specifications for these measures is available on the NEMSQA website after creating a user profile. 

## Measure Key
✔️ Developed and included in {nemsqar}
🔧 Planned for future development

| Measure ID       | Description                                                                                                          | Type     | Domain                                | In `{nemsqar}`? |
|-----------------|----------------------------------------------------------------------------------------------------------------------|----------|---------------------------------------|-----------------|
| Airway-01       | Successful advanced airway placement on first attempt without peri-intubation hypotension or hypoxia.              | Outcome  | Clinical Process – Effectiveness      | ✔️              |
| Airway-05       | Adequate oxygen levels before intubation.                                                                           | Process  | Patient Safety                        | ✔️              |
| Airway-18       | Use of waveform capnography for airway confirmation.                                                               | Process  | Clinical Process – Effectiveness      | ✔️              |
| Asthma-01       | Asthma patients receiving aerosolized beta agonists.                                                               | Process  | Clinical Process – Effectiveness      | ✔️              |
| Hypoglycemia-01 | Treatment of symptomatic hypoglycemia.                                                                             | Process  | Clinical Process – Effectiveness      | ✔️              |
| Respiratory-01  | Respiratory assessment for distress patients.                                                                      | Process  | Clinical Process – Effectiveness      | ✔️              |
| Respiratory-02  | Hypoxia patients receiving oxygen.                                                                                 | Process  | Clinical Process – Effectiveness      | ✔️              |
| Pediatrics-03b  | Pediatric patients receiving weight-based medication with documented weight.                                       | Process  | Patient Safety                        | ✔️              |
| Safety-01       | EMS responses without lights and sirens.                                                                           | Process  | Patient Safety                        | ✔️              |
| Safety-02       | Transports without lights and sirens.                                                                              | Process  | Patient Safety                        | ✔️              |
| Safety-04       | Pediatric patients transported with appropriate restraint.                                                         | Process  | Patient Safety                        | ✔️              |
| Seizure-02      | Status epilepticus patients receiving benzodiazepine.                                                             | Process  | Clinical Process – Effectiveness      | ✔️              |
| Stroke-01       | Stroke assessment for suspected stroke patients.                                                                  | Process  | Clinical Process – Effectiveness      | ✔️              |
| Syncope-01      | Syncope patients receiving a 12-lead ECG.                                                                         | Process  | Clinical Process – Effectiveness      | ✔️              |
| TBI-01          | Documentation of oxygen level, ETCO2, and BP in suspected TBI patients.                                          | Process  | Clinical Process – Effectiveness      | ✔️              |
| Trauma-01       | Pain assessment for injury patients.                                                                               | Process  | Patient Experience                    | ✔️              |
| Trauma-03       | Pain score improvement during EMS transport.                                                                      | Outcome  | Patient Experience                    | ✔️              |
| Trauma-04       | Trauma patients meeting field triage criteria transported to a trauma center.                                    | Process  | Clinical Process – Effectiveness      | ✔️              |
| Trauma-08       | Trauma patients with documented GCS, BP, and respiratory rate.                                                   | Process  | Clinical Process – Effectiveness      | ✔️              |
| Trauma-14       | Pre-arrival trauma alert for Step 1 or Step 2 field triage patients.                                              | Process  | Clinical Process – Effectiveness      | ✔️              |
| TTR-01          | Non-transported patients with documented vital signs.                                                            | Process  | Clinical Process – Effectiveness      | ✔️              |


# Resources

Interested persons may reference the following links and resources via NEMSQA's website.  Note - some materials may require a free membership to access.

* [Measure Specification Table (NEMSQA)_2021](https://www.nemsqa.org/assets/docs/Measure-Specification-Table-NEMSQA_2021.pdf): A quick-reference table that includes broad-view measure specifications (i.e., numerator and denominator criteria) for each measure.
* [Project Summary with Measure Narratives](https://www.nemsqa.org/assets/docs/Project-Summary-with-Measure-Narrative-NEMSQA.pdf): An overview of the EMS Compass 2.0 project and narratives for each measure, which include systems implications.
* [Frequently Asked Questions](https://www.nemsqa.org/nemsqa-measure-faq): Answers to commonly asked questions.
* [NEMSQA Measure Technical Release Notes_2021](https://www.nemsqa.org/assets/docs/NEMSQA-Measure-Technical-Release-Notes_2021.pdf): A reference table that documents changes made to the NEMSQA measures during the 2021 annual review.
* [Fact Sheet](https://www.nemsqa.org/assets/docs/Fact-Sheet-NEMSQA.pdf): An info page explaining why agencies and systems should migrate from EMS Compass legacy/candidate measures to the National EMS Quality Alliance Measure Set.
