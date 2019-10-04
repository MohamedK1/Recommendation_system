module DataFile where

import System.Random
import System.IO.Unsafe

users = ["user1", "user2", "user3", "user4"]
items = ["item1", "item2", "item3", "item4", "item5", "item6"]
purchasesHistory =  [
                        ("user1", [["item1", "item2", "item3"], ["item1", "item2", "item4"]]),
                        ("user2", [["item2", "item5"], ["item4", "item5"]]),
                        ("user3", [["item3", "item2"]]),
                        ("user4", [])
                    ]

randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))
