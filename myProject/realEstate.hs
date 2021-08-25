module myPlutus.myProject where

data Building = Building { streetAddress :: String  
                     , postcode :: String  
                     , landRegistryID :: String  
                     } deriving (Show)

