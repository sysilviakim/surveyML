# surveyML

This repository contains reproducible code for Kim and Zilinsky (2021), *The Divided (But Not More Predictable) Electorate: A Machine Learning Analysis of Voting in American Presidential Elections*. APSA preprint is available [here](https://doi.org/10.33774/apsa-2021-45w3m).

## Datsets

The dataset comprises of three national surveys:

* American National Election Studies ([ANES](https://electionstudies.org))
* Cooperative Congressional Election Study ([CCES](https://cces.gov.harvard.edu))
* [Nationscape](https://www.voterstudygroup.org/publication/nationscape-data-set)

## Feature sets: Nationscape

* Specification 1: **Demographics*. Categorical variables for: gender, ethnicity (4 categories), income (5 categories), and education (4 categories). Age as continuous variable is also included.
* Specification 2: S1 + 7 Party ID indicators.
* Specification 3: S2 + Issues (see the list below)
* Specification 4: S3 + Remaining information

### Specification 3 details

Specification 3 included views on 7 social issues, 9 economic issues, and 4 policies related to immigration.
Although some additional policy questions are asked on some Nationscape waves, we choose the policies are included frequently, in order to maximize coverage. 

| Issue                                                                              | Variable name          |
|------------------------------------------------------------------------------------|------------------------|
| Don't permit late term abortion                                                    | late_term_abortion     |
| Never permit abortion                                                              | abortion_never         |
| Against abortion when woman's life is in danger                                    | abortion_conditions    |
| Against background checks for gun purchases                                        | guns_bg                |
| Against banning all guns                                                           | ban_guns               |
| Against an assault rifle ban                                                       | ban_assault_rifles     |
| Against a limit gun magazines                                                      | limit_magazines        |
| Provide government-run health insurance to all Americans                           | gov_insurance          |
| Provide government health insurance option                                         | public_option          |
| Subsidize health insurance for low income Americans                                | health_subsidies       |
| Abolish private health insurance                                                   | abolish_priv_insurance |
| Raise taxes on families making over $600,000                                       | raise_upper_tax        |
| Cut taxes for families making less than $100,000 per year                          | mctaxes                |
| Against eliminating the estate tax                                                 | estate_tax             |
| Provide debt-free state college                                                    | college                |
| Cap carbon emissions to combat climate change                                      | cap_carbon             |
| Against a path to citizenship for undocumented immigrants brought here as children | dreamers               |
| Against a path to citizenship for all undocumented immigrants                      | path_to_citizenship    |
| Build a wall on the southern U.S. border                                           | wall                   |
| Deport all undocumented immigrants                                                 | deportation            |

### What else is in Specification 4?

Categorical variables for political knowledge, sexual orientation, primary language spoken at home, religion (specific faith + an Evangelical indicator), union membership, gun ownership, racial attitudes, gender attitudes, and favorability of the following groups: Asians, Blacks, Whites, Democrats, Labor Unions, Latinos, LGBT, Muslims, Republicans, Socialists, the police, and undocumented immigrants.