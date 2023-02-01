# Update ------------------------------------------------------------------

# Update documentation
devtools::document()

# Load the package
devtools::load_all()

# Unload the package
devtools::unload()

# Install the dev version
devtools::install()

# Create new function -----------------------------------------------------

usethis::use_r("stat_vhistogram")

# Create new test ---------------------------------------------------------

usethis::use_test()

# Add dependency ----------------------------------------------------------

usethis::use_package("MetBrewer", "Suggests")
