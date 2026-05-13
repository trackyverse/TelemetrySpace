# Error functions

Error functions

## Usage

``` r
check_num_vec_len(x, vec_length = NULL, arg_name = NULL)

check_array(x, arg_name = NULL)

check_array_tag(x, len, arg_name = NULL)

check_stan_object(x, arg_name = NULL)
```

## Arguments

- x:

  is a `Stan` object

- vec_length:

  is the length of the vector to check.

- arg_name:

  the name of the argument to check.

- len:

  is the length to make the array. This needs to be the same length as
  `ntest` or the number of tags.
