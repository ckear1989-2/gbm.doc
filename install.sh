#! /bin/bash

# Rscript -e "devtools::create(\"gbm.doc\")"
# Rscript -e "devtools::load_all()"
# Rscript -e "devtools::build_manual()"
# Rscript -e "devtools::check(document=TRUE, manual=TRUE, force_suggests=TRUE, run_dont_test=FALSE)"
Rscript -e "devtools::document()"
Rscript -e "devtools::install()"
# Rscript tests/create.test.data.R
# Rscript temp.R
# Rscript -e "testthat::test_file(\"tests/testthat/test_univariates.R\", package=\"gbm.doc\")"
Rscript -e "testthat::test_file(\"tests/testthat/test_summary.R\", package=\"gbm.doc\")"
# Rscript -e "devtools::test()"
