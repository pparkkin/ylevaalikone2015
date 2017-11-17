#!/bin/bash

set -e

## Build ETL tool
stack build

## Download CSV data file
if [ ! -f vastaukset_avoimena_datana.csv ]; then
    wget --quiet http://data.yle.fi/dokumentit/Eduskuntavaalit2015/vastaukset_avoimena_datana.csv
fi

## Load data from CSV file into a SQLite database
data_dir=work/data
mkdir -p "$data_dir"
if [ ! -f "$data_dir/ylevaalikone2015.sqlite3" ]; then
    stack exec dbcreate "$data_dir/ylevaalikone2015.sqlite3"
fi

## Run Jupyter Docker image
docker run -it --rm -p 8888:8888 -v "`pwd`/work:/home/jovyan/work" jupyter/datascience-notebook
