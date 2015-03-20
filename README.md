# ylevaalikone2015

Explore the data from YLE Vaalikone 2015.


## Get the Data

I did not want to host the data in the repo, so there's a script to download the data. The script will also convert it to UTF-8. The script uses wget and iconv. Make sure both are installed before you run it.

Before you get started, run the script.

    $ fetch_data.sh


## Build and Run

The program uses cabal to build. Start by creating a sandbox and installing dependencies.

    $ cabal sandbox init
    $ cabal install --only-dependencies

Build the program.

    $ cabal build

Run it.

    $ cabal run vastaukset_avoimena_datana-utf8.csv

That should give you a nice table with distances between parties' views.

          Krist ruots Kesku Sosia Kansa Itsen Vasem Vihre Kommu Piraa Perus TyÃ¶v Muuto Kommu
    Krist  0.00 14.18 11.87 10.82 13.42 15.43 13.82 14.53 16.46 15.23 14.04 14.93 14.49 15.87
    ruots 14.18  0.00 16.31 15.30 15.00 18.63 16.25 14.83 17.66 16.46 18.38 15.75 18.36 17.97
    Kesku 11.87 16.31  0.00 15.75 13.23 15.07 17.15 16.55 18.87 15.20 11.75 16.61 14.59 16.82
    Sosia 10.82 15.30 15.75  0.00 15.13 15.33 11.40 13.04 15.62 15.72 16.85 14.90 15.72 15.59
    Kansa 13.42 15.00 13.23 15.13  0.00 18.49 18.41 16.03 20.81 14.63 16.76 17.64 17.26 18.71
    Itsen 15.43 18.63 15.07 15.33 18.49  0.00 14.18 17.75 15.84 17.49 14.39 15.65 13.49 14.21
    Vasem 13.82 16.25 17.15 11.40 18.41 14.18  0.00 12.08 12.08 16.09 17.55 14.21 14.93 13.89
    Vihre 14.53 14.83 16.55 13.04 16.03 17.75 12.08  0.00 14.21 15.39 18.60 16.12 17.86 14.53
    Kommu 16.46 17.66 18.87 15.62 20.81 15.84 12.08 14.21  0.00 18.73 19.29 14.90 17.64 14.46
    Piraa 15.23 16.46 15.20 15.72 14.63 17.49 16.09 15.39 18.73  0.00 16.03 16.46 15.87 15.87
    Perus 14.04 18.38 11.75 16.85 16.76 14.39 17.55 18.60 19.29 16.03  0.00 17.03 13.60 16.22
    TyÃ¶v 14.93 15.75 16.61 14.90 17.64 15.65 14.21 16.12 14.90 16.46 17.03  0.00 16.22 13.45
    Muuto 14.49 18.36 14.59 15.72 17.26 13.49 14.93 17.86 17.64 15.87 13.60 16.22  0.00 15.30
    Kommu 15.87 17.97 16.82 15.59 18.71 14.21 13.89 14.53 14.46 15.87 16.22 13.45 15.30  0.00 


## Thank You

Please let me know if you have any comments, or find any bugs!
