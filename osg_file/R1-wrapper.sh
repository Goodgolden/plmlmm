#!/bin/bash

module load jags/4.3.0
module load r

# Uncompress the tarball
tar -xzf R-packages.tar.gz

export R_LIBS="$PWD/R-packages"

# set TMPDIR variable
mkdir rtmp
export TMPDIR=$_CONDOR_SCRATCH_DIR/rtmp

ls

# run the R program
Rscript --no-save Randy_plmlmm_0.R $1

ls
ls rtmp/
# move csv files to scratch dir
# mv rtmp/mcmc_1_1.Rdata $_CONDOR_SCRATCH_DIR
