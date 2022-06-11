# Linear Model for Rent Analysis in Buenos Aires

In this analysis, they were extracted from the argenprop site the rental values of apartments in the City of Buenos Aires will try to predict the price of rent through 1 multiple regression depending on the available covariates. Then it will try to a logistic regression to see if there is any pattern for those who ask for the rental value in USD

## Dataset
| Name                  | Description                                            |
|-----------------------|--------------------------------------------------------|
| "currency"            | Currency{ARS,USD}                                      |
| "amount"              | Original amount                                        |
| "normalized_amount"   | Amount normalized in ARS (Dependant on exchange rate)  |
| "m2"                  | Square Meters                                          |
| "dorms"               | Number of Dorms                                        |
| "bath"                | Number of baths                                        |
| "age"                 | Age in years                                           |
| "backside"            | Is apparment on the backside?{T,F}                     |
| "loft"                | Is apparment a loft?{T,F}                              |
| "office"              | Is apparment an office?{T,F}                           |
