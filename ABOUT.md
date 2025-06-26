---
title: "Exposure Response Analysis"
---

# Exposure Response Analysis

This tool is used for exploring population health with certain diagnoses. For example we want to explore sleep apnea patients (ICD-10 code `G47`) and what other common diagnoses they have. Then we can select response diagnoses, for example hypertension (ICD-10 code `I10`), and explore how sleep apnea is affected to hypertension. 

To run analysis, use *regex* code (ex. `^G47 | ^G31`) to select diagnose groups. You can also select which registries are used as a source. Report can be downloaded in html or docx format. 


Application is logging user input and runs. If selected group size is under 6 person, results are not printed and error occur.

## How to use?

- On right there is input panel, which can be minimized

- On left is navigation tree for different analysis

- Report can be downloaded to html/docx document on *Download Report* -section

## Application Changes Log

| Date       | Version | Description                                                                                       |
|------------|---------|--------------------------------------------------------------------------------------------------|
| 2025/06/23 | 1.8.9   | healthpopR-package update, small fixes and Docker implemented.
| 2025/06/05 | 1.8.6   | Updated healthpopR-package v.0.1.8 and use of Poisson functions.
| 2025/05/21 | 1.8.5   | Moved to use healthpopR package. Lot of changes under the hood.
| 2025/05/12 | 1.7.9   | Tweaks and improvements to Cox modelling and also general functions (ex. pop-functions). Added html-reporting for Cox modelling.
| 2025/04/15 | 1.7.2   | Cox Proportional Hazard Model Analysis added
| 2025/03/12 | 1.7.0   | Major update: GUI changed using bslib-package, lot of cleaning and development.                                                                   |
| 2025/03/11 | 1.6.1   | Functions are now tweaked better and progress messages available in Shiny, but works also without Shiny. |
| 2025/03/10 | 1.6.0   | Minor changes, ICD codes updated.                                                               |
| 2025/03/03 | 1.5.9   | Progression bars are added to the application to help to know what app is doing.               |
| 2025/02/05 | 1.5.7   | Added synthetic data. All other but Poisson results work with it (this is why report download does not work). Data does not describe a real-world case, only shows how the app works. |
| 2025/01/18 | 1.5.6   | Report added. Still, a lot of adjustment needs to be done.                                      |
| 2024/11/18 | 1.5.5   | First version of ERA Shiny.                                                                    |