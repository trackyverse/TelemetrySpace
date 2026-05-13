# Generate Quantities

Used internally to calculate generated quantities for each draw

## Usage

``` r
generated_quantities(model, standata, ndraws = NULL)
```

## Arguments

- model:

  Stan model object

- standata:

  Data fed to Stan model

- ndraws:

  is the number of draws to take. Default to 10.

## Value

generated quantities from the model
