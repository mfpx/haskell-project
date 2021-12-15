module Main where

import System.IO
import Types
import Fetch
import Parse
import Database

main :: IO ()
main = do
    putStrLn "---------------------------------"
    putStrLn "  Welcome to the Covid data app  "
    putStrLn "  (1) Download data              "
    putStrLn "  (2) All entries by country     "
    putStrLn "  (3) Total cases by country     "
    putStrLn "  (4) Quit                       "
    putStrLn "---------------------------------"
    conn <- initialiseDB
    hSetBuffering stdout NoBuffering
    putStr "Choose an option > "
    option <- readLn :: IO Int
    case option of
        1 -> do
            let url = "https://opendata.ecdc.europa.eu/covid19/casedistribution/json/"
            print "Downloading..."
            json <- download url
            print "Parsing..."
            case (parseRecords json) of
                Left err -> print err
                Right recs -> do
                    print "Saving on DB..."
                    saveRecords conn (records recs)
                    print "Saved!"
                    main
        2 -> do
            entries <- queryCountryAllEntries conn
            mapM_ print entries
            main
        3 -> do
            queryCountryTotalCases conn
            main
        4 -> print "Hope you've enjoyed using the app!"
        otherwise -> print "Invalid option"
