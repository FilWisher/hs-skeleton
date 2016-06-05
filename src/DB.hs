module DB
    ( someFunc
    ) where
    
import Database.HDBC
import Database.HDBC.Sqlite3

someFunc :: IO ()
someFunc = putStrLn "someFunc"

