#!/bin/bash
emacs -Q --batch --no-site-file -l ert.el -l ./indent-advices.el \
   -l ./indent-tests.el -f ert-run-tests-batch-and-exit || exit 1
