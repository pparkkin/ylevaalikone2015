# ylevaalikone2015

Explore the data from YLE Vaalikone 2015.


## Get the Data

I did not want to host the data in the repo, so there's a script to download the data. The script will also convert it to UTF-8. The script uses wget and iconv. Make sure both are installed before you run it.

Before you get started, run the script.

    $ fetch_data.sh


## Build

The program uses cabal to build. Start by creating a sandbox and installing dependencies.

    $ cabal sandbox init
    $ cabal install --only-dependencies

Build the program.

    $ cabal build

## Explore

Open a REPL.

    $ cabal repl

Now that you're in the REPL, you can load the data, and start taking a look at it.

The function `loadData` loads the data from the data file.

    *VK> d <- loadData "vastaukset_avoimena_datana-utf8.csv"
    *VK> :t d
    d :: VKData

Functions `parties`, `issues`, and `positions` let you take a look inside the data.

    *VK> length $ parties d
    42
    *VK> length $ issues d
    111
    *VK> length $ positions d
    42

The function `position` will give you the position for a single party.

    *VK> let kesk = parties d !! 2
    *VK> position kesk d
    [1.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,0.0,2.0,2.0,2.0,2.0,-1.0,-1.0,1.0,-2.0,-1.0,1.0,1.0,1.0,0.0,-1.0,1.0,-1.0,-1.0,-2.0,1.0,-2.0,-1.0,-1.0,-1.0,1.0,-1.0,0.0,1.0,-1.0,-1.0,1.0,0.0,-1.0,-2.0,-1.0,2.0,1.0,1.0,-1.0,-1.0,2.0,1.0,-1.0,-1.0,0.0,1.0,1.0,1.0,-2.0,-2.0,2.0,-2.0,1.0,-2.0,-2.0,1.0,1.0,0.0,1.0,2.0,-1.0,-2.0,2.0,1.0,2.0,-2.0,-1.0,1.0,-1.0,-1.0,0.0,0.0,0.0,0.0,1.0,-1.0,-1.0,-2.0,-1.0,-1.0,-1.0,1.0,-1.0,1.0,1.0,1.0,1.0,1.0,1.0,-1.0,1.0,-1.0,-1.0,-1.0,1.0,-2.0,-1.0,1.0,1.0,1.0,1.0,1.0]

The function `distance` shows the distance between two parties.

    *VK> let peruss = parties d !! 0
    *VK> distance (position kesk d) (position peruss d)
    11.74734

And finally, `dropUninteresting` will drop some uninteresting parties from the data set, and `printDistanceTable` will print out a table of distance from the data.

    *VK> let di = dropUninteresting d
    *VK> printDistanceTable di
          Krist ruots Kesku Sosia Kansa Itsen Vasem Vihre Kommu Piraa Perus TyÃ¶v Muuto Kommu
    Krist  0.00 14.04 11.75 10.91 12.96 16.52 14.00 15.33 16.64 15.03 13.71 15.17 15.07 15.56
    ruots 14.04  0.00 15.72 14.76 15.13 19.80 15.65 15.68 18.33 16.09 18.25 16.88 18.44 17.75
    Kesku 11.75 15.72  0.00 15.33 13.34 15.65 16.19 17.18 18.73 14.28 11.14 16.55 14.80 16.73
    Sosia 10.91 14.76 15.33  0.00 13.96 17.38 13.08 13.04 16.43 15.91 16.82 15.07 17.20 15.59
    Kansa 12.96 15.13 13.34 13.96  0.00 19.92 18.22 15.13 20.90 15.03 16.55 17.78 17.35 18.87
    Itsen 16.52 19.80 15.65 17.38 19.92  0.00 14.32 17.72 16.19 15.00 15.46 15.39 14.14 14.04
    Vasem 14.00 15.65 16.19 13.08 18.22 14.32  0.00 12.04 12.12 16.25 17.03 14.35 15.78 12.88
    Vihre 15.33 15.68 17.18 13.04 15.13 17.72 12.04  0.00 15.68 15.33 19.57 16.28 17.49 15.13
    Kommu 16.64 18.33 18.73 16.43 20.90 16.19 12.12 15.68  0.00 18.73 19.16 15.65 18.06 14.53
    Piraa 15.03 16.09 14.28 15.91 15.03 15.00 16.25 15.33 18.73  0.00 16.12 16.61 15.97 16.25
    Perus 13.71 18.25 11.14 16.82 16.55 15.46 17.03 19.57 19.16 16.12  0.00 17.32 14.59 17.03
    TyÃ¶v 15.17 16.88 16.55 15.07 17.78 15.39 14.35 16.28 15.65 16.61 17.32  0.00 16.03 13.27
    Muuto 15.07 18.44 14.80 17.20 17.35 14.14 15.78 17.49 18.06 15.97 14.59 16.03  0.00 15.91
    Kommu 15.56 17.75 16.73 15.59 18.87 14.04 12.88 15.13 14.53 16.25 17.03 13.27 15.91  0.00


## Thank You

Please let me know if you have any comments, or find any bugs!
