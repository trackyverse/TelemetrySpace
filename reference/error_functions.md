# Error functions

Error functions

## Usage

``` r
check_aeqd(sf, arg_name = NULL)

check_aeqd_string(vec, arg_name = NULL)

check_array(array, arg_name = NULL)

check_array_tag(array, len, arg_name = NULL)

check_char_vec_len(vec, vec_length = NULL, arg_name = NULL)

check_column_names(df, arg_name = NULL, coords = FALSE)

check_column_type(df, arg_name = NULL, coords = FALSE)

check_data_frame(df, arg_name = NULL)

check_delay(vec, type, arg_name = NULL)

check_list(list, arg_name = NULL)

check_nrec(df, vec, arg_name_df = NULL, arg_name_vec = NULL)

check_numerical(vec, arg_name = NULL)

check_num_vec_len(vec, vec_length = NULL, arg_name = NULL)

check_present(df, cols, fnct, label)

check_sf_object(sf, arg_name = NULL)

check_stan_object(stan, arg_name = NULL)

check_time(df, arg_name = NULL)

check_unit(vec, arg_name = NULL)

check_utm(sf, arg_name = NULL)
```

## Arguments

- sf:

  is a `sf` object that needs to be checked.

- arg_name:

  the name of the argument to check.

- vec:

  is a `vector` that needs to be checked.

- array:

  is a `array` that needs to be checked.

- len:

  is the length to make the array. This needs to be the same length as
  `ntest` or the number of tags.

- vec_length:

  is the length of the vector to check.

- df:

  is a `data.frame` object that needs to be checked.

- type:

  is a `character` that is the type of delay desired.

- list:

  is a `list` to be checked.

- arg_name_df:

  the name of the argument of df to check.

- arg_name_vec:

  the name of the argument of vec to check.

- cols:

  is a character `vector` of column names to check

- fnct:

  is the name of a function to appply e.g., `is.numeric`.

- label:

  is the name of the group of cols e.g., `receiver`.

- stan:

  is a `Stan` object.
