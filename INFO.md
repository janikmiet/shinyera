---
title: "OSTPRE Data Description"
---

## OSTPRE Follow up dataset

The Kuopio Osteoporosis Risk Factor and Prevention (*OSTPRE*) follow-up dataset is a long-term population-based cohort study which started 1989 and is still on going. The original cohort consisted total of 14 220 female participant from the Kuopio region of Finland, aged 47-56 years old at the start of the study in 1989. Later cohort was expanded to total of 16 726 participants. Collected data includes a lot of linked data from the health registers including hospital visits and diagnoses since 1969. 

For the Exposure Response Analysis (Poisson Regression model to estimate SIR), follow-up is set from the time participant turns 50 years old and follow-up ends in the last register updating point in 2023.

### Registry Sources

| Source   | Description in Finnish                                             | Description in English                              | Source Codes                                                      | 
|----------|--------------------------------------------------------------------|-----------------------------------------------------|-------------------------------------------------------------------|
| avohilmo | Perusterveydenhuollon avohoidon hoitoilmoitus -rekisteri (Avohilmo)| Register of Primary Health Care Visits (THL)        | avohilmo, avohu_icd10, avohu_pitkadg, avohu_taptyyp, avohu_ulksyy | 
| erko     | Erityiskorvausoikeus rekisteri (Kela)                              | Special Reimbursement Register (Kela)               | erko |
| hilmo    | Hoitoilmoitusrekisteri (Hilmo)                                     | Care Register for Health Care (Hilmo, THL)          | hilmo, hilmou |
| ksyy     | Kuolinsyyrekisteri                                                 | Cause of Death Register                             | ksyy |
| local    | Kuopion perusterveyden huolto ja KYS                               | Kuopio Primary Care and Kuopio University Hospital  | kpo_icd10, kpo_laake, kpo_riski, kys |
| soshilmo | Sosiaali- ja terveydenhuollon hoitoilmoitusjärjestelmä             | Care Register for Social Welfare                    | soshilmo             | 
| syopa    | Syöpärekisteri                                                     | Cancer Registry                                     | syopa |

### Extra Sources

| Source   | Description in Finnish                                             | Description in English                              | Source Codes                                                      | 
|----------|--------------------------------------------------------------------|-----------------------------------------------------|-------------------------------------------------------------------|
| murt     | Murtumat                                                           | Fractures                                           | murt |


### Cox Model Variables

| Variable   | Description in Finnish                          | Description in English                              | Type                                                     | 
|------------|-------------------------------------------------|-----------------------------------------------------|----------------------------------------------------------|
| age_bs     | Age in Baseline Study                           | Ikä baseline kyselyssä                              | Continuous integer |
| bmi        | Body Mass Index in Baseline Study               | Painoindeksi baseline kyselyssä                | Continuous integer |
| bmi_cat1   | Categorized Body Mass Index in Baseline Study   | Kategorisoitu painoindeksi baseline kyselyn hetkenä | Levels: Underweight (bmi < 18.5), Healthy Weight (bmi >= 18.5 & bmi < 25), Overweight (bmi >= 25 & bmi < 30), Class 1 Obesity (bmi >= 30 & bmi < 35), Class 2 Obesity (bmi >= 35 & bmi < 40), Class 3 Obesity (bmi >= 40)   |
| bmi_cat2   | Categorized Body Mass Index in Baseline Study   | Kategorisoitu painoindeksi baseline kyselyn hetkenä | Levels: Underweight (bmi < 18.5), Healthy Weight (bmi >= 18.5 & bmi < 25), Overweight (bmi >= 25 & bmi < 30), Obesity (bmi >= 30)  |
| edu        | Education information in Baseline Study         | Koulutus baseline kyselyn hetkenä                   | Levels: 1 - Low, 2 - Med, 3 - High  |



