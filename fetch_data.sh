#!/bin/bash

wget http://data.yle.fi/dokumentit/Eduskuntavaalit2015/vastaukset_avoimena_datana.csv
iconv -f ISO-8859-15 -t UTF-8 vastaukset_avoimena_datana.csv > vastaukset_avoimena_datana-utf8.csv

