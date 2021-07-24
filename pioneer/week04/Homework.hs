{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text, unpack)
import GHC.Generics          (Generic)
import Ledger
import Wallet.Emulator    --- had to add this in
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

payContractWithErrHandler :: Contract () PaySchema Text ()
payContractWithErrHandler = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    payContract


-- HW1: A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace a b = do
    h <- activateContractWallet (Wallet 1) payContractWithErrHandler

    let params1 = PayParams
              { ppRecipient = (pubKeyHash $ walletPubKey $ Wallet 2)
              , ppLovelace  = a
              }

    let params2 = PayParams
              { ppRecipient = (pubKeyHash $ walletPubKey $ Wallet 2)
              , ppLovelace  = b
              }

    callEndpoint @"pay" h params1
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h params2
    void $ Emulator.waitNSlots 1


payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
