# Declare global variables
#
# This avoids notes during check
# Somehow surprisingly, this is CRAN's recommendation:
# https://stackoverflow.com/a/12429344
globalVariables(c(".data", ":="))
