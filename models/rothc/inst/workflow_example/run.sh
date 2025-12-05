#!/usr/bin/env bash

set -e
start_time=$(date +'%Y%m%d%H%M%S')
logfile=${1:-runlog_"$start_time".txt}

exec > >(tee -a "$logfile") 2>&1
echo "start: $start_time"

export NCPUS=8

if [[ ! -d data/ERA5_CA_RothC ]]; then
	echo "================= Converting met data ================="
	./ERA5_nc_to_RothC.R --n_cores="$NCPUS"
fi

echo "================= Building XML settings ================="
./xml_build.R --output_dir_name=output_"$start_time"

echo "================= Setting up model files ================="
./set_up_rothc_runs.R

echo "================= Running RothC ================="
./run_model.R --n_cores="$NCPUS" \
	--settings=output_"$start_time"/pecan.CONFIGS.xml

echo "end: $(date +'%Y%m%d%H%M%S')"
