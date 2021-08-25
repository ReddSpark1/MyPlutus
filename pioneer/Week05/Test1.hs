{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Solution1 where

--import           Control.Monad          hiding (fmap)
--import           Data.Aeson             (ToJSON, FromJSON)
--import           Data.Default           (Default (..))
-- import           Data.Text              (Text)
-- import           Data.Void              (Void)
--import           GHC.Generics           (Generic)
-- import           Plutus.Contract        as Contract
-- import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
-- import           Ledger.Constraints     as Constraints
-- import           Ledger.TimeSlot
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
-- import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
-- import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
-- import           Playground.Types       (KnownCurrency (..))
-- import           Prelude                (IO, Semigroup (..), Show (..), String)
-- import           Text.Printf            (printf)
-- import           Wallet.Emulator.Wallet

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkPolicy pkh deadline () ctx =
    traceIfFalse "signature missing" (txSignedBy info pkh) &&
    traceIfFalse "deadline missed"   (to deadline `contains` txInfoValidRange info)
  where
    info = scriptContextTxInfo ctx

policy :: PubKeyHash -> POSIXTime -> Scripts.MintingPolicy
policy pkh deadline = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \pkh' deadline' -> Scripts.wrapMintingPolicy $ mkPolicy pkh' deadline' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh
    `PlutusTx.applyCode`
    PlutusTx.liftCode deadline

------------------------

mkPolicy2 :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy2 oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

policy2 :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy2 oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy2 oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
