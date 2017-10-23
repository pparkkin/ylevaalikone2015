#!/bin/bash

set -e

## Build ETL tool
stack build

## Download CSV data file
if [ ! -f vastaukset_avoimena_datana.csv ]; then
    wget --quiet http://data.yle.fi/dokumentit/Eduskuntavaalit2015/vastaukset_avoimena_datana.csv
fi

## Load data from CSV file into a SQLite database
mkdir -p notebook
if [ ! -f notebook/ylevaalikone2015.sqlite3 ]; then
    stack exec dbcreate notebook/ylevaalikone2015.sqlite3
fi

## Build Zeppelin Docker image
docker build -t ylevaalikone2015 .

## Run Zeppelin Docker image
mkdir -p logs
docker run -p 8080:8080 --rm -v $PWD/logs:/logs -v $PWD/notebook:/notebook -e ZEPPELIN_LOG_DIR='/logs' -e ZEPPELIN_NOTEBOOK_DIR='/notebook' --name ylevaalikone2015 ylevaalikone2015
