# Log in to your MATOS account

This function prompts you for the username (email) and password
associated with your MATOS account. This is necessary so that you may
interface with any project-specific files. If you don't have a MATOS
account [you can sign up for one
here](https://matos.asascience.com/account/signup).

## Usage

``` r
matos_login(credentials = NULL)
```

## Arguments

- credentials:

  list with names "UserName" and "Password". This argument only exists
  for testing purposes and should not be used! It will store your
  credentials in your R history, which is definitely not good.

## Details

A pop up will appear asking for your username and password. If
everything works out, your credentials will be kept in the sessions'
cookies. Your username/password will not be saved – this was done
intentionally so that you don't accidentally save credentials in a
public script.

## Examples

``` r
if (FALSE) { # \dontrun{
# Type:
matos_login()
# ...then follow the on-screen prompts
} # }
```
