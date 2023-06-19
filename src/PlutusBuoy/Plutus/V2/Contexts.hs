{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- {-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- {-# OPTIONS_GHC -fobject-code #-}
-- {-# OPTIONS_GHC -fno-specialise #-}

module PlutusBuoy.Plutus.V2.Contexts where

import PlutusBuoy.Plutus.Common
import PlutusBuoy.Plutus.Sort
import PlutusBuoy.Plutus.V2.Common

import Control.Lens (view)
import Control.Monad (void, when, join, (<=<))
import Control.Monad.Error.Lens (throwing)

import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty, NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Coerce (coerce)

-- import Ledger -- (Address, POSIXTime, POSIXTimeRange, Validator, PubKeyHash(..), pubKeyAddress, pubKeyHashAddress, getTxId)
import Ledger.Ada qualified as Ada
import Ledger.Address
import Ledger.Credential (Credential(..))
import Ledger.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain ()
import Ledger.Interval qualified as Interval
import Ledger.Value (Value)
import Ledger.Value -- qualified as Value
import Ledger.TimeSlot qualified as TimeSlot
-- import Ledger.Tx qualified as Tx

import Playground.Contract

-- import Plutus.V1.Ledger.Contexts (ScriptContext (..), TxInfo (..), TxInInfo(..), ownCurrencySymbol, valuePaidTo, valueProduced,
--                         getContinuingOutputs, scriptOutputsAt, findOwnInput, ownHash)
-- import Plutus.V1.Ledger.Contexts qualified as Validation
-- import Plutus.V1.Ledger.Api -- (Datum, DatumHash, getDatum, mkMintingPolicyScript, mkValidatorScript)
-- import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as Scripts
-- import Plutus.Contract
-- import Plutus.Contract.Test hiding (not)
-- -- import Plutus.Contract.Typed.Tx qualified as Typed
-- import PlutusTx qualified
-- import PlutusTx.Base (snd)
-- import PlutusTx.Prelude hiding (Semigroup (..))
-- import PlutusTx.Prelude qualified as PP
-- import PlutusTx.AssocMap qualified as PMap
-- import PlutusTx.Builtins
-- import PlutusTx.Foldable (foldl, foldMap)
-- import PlutusTx.Maybe (maybe)

import PlutusBuoy.Plutus.List
import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Tx
import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Crypto
import Plutus.V1.Ledger.Scripts

import PlutusTx.AssocMap qualified as PMap

import Prelude qualified as Haskell
import Schema (ToSchema)
import Control.Arrow ((&&&))

{-# INLINABLE consumedUtxoForName #-}
consumedUtxoForName outRefId tokenName =
    consumedUTxO $ (unTokenName tokenName ==) . outRefId . txInInfoOutRef

{-# INLINABLE consumedUTxO #-}
consumedUTxO :: (TxInInfo -> Bool) -> TxInfo -> Bool
consumedUTxO pred txIn = length (inputOutsThat pred txIn) > 0 -- pred `any` txInfoInputs txIn

{-# INLINABLE spentTxOutRef #-}
spentTxOutRef :: TxOutRef -> TxInfo -> Bool
spentTxOutRef ref txIn = ((ref ==) . txInInfoOutRef) `any` txInfoInputs txIn

{-# INLINABLE txOutsSpentFromScript #-}
txOutsSpentFromScript :: TxInfo -> ValidatorHash -> [TxOut]
txOutsSpentFromScript TxInfo {..} hash =
    filter (\ TxOut{..} -> (Just hash ==) $ toValidatorHash $ txOutAddress ) $ map txInInfoResolved txInfoInputs

{-# INLINABLE txOutsSpentFrom #-}
txOutsSpentFrom :: TxInfo -> Credential -> [TxOut]
txOutsSpentFrom TxInfo {..} cred =
    filter (\ TxOut{..} -> cred == addressCredential txOutAddress ) $ map txInInfoResolved txInfoInputs

{-# INLINABLE valueSpentFromScript' #-}
valueSpentFromScript' :: TxInfo -> ValidatorHash -> Maybe Value
valueSpentFromScript' txIn = foldlNonEmpty (<>) mempty . fmap txOutValue . txOutsSpentFromScript txIn

{-# INLINABLE valueSpentFrom #-}
valueSpentFrom :: TxInfo -> Credential -> Maybe Value
valueSpentFrom txIn = foldlNonEmpty (<>) mempty . fmap txOutValue . txOutsSpentFrom txIn

{-# INLINABLE pubKeyInputsAt #-}
-- | Get the values paid to a public key address by a pending transaction.
pubKeyInputsAt :: PubKeyHash -> TxInfo -> [Value]
pubKeyInputsAt pk p =
    let flt TxOut{txOutAddress = Address (PubKeyCredential pk') _, txOutValue} | pk == pk' = Just txOutValue
        flt _                                                                              = Nothing
    in mapMaybe (flt . txInInfoResolved) (txInfoInputs p)

{-# INLINABLE valueSpentBy #-}
-- | Get the total value paid to a public key address by a pending transaction.
valueSpentBy :: TxInfo -> PubKeyHash -> Value
valueSpentBy ptx pkh = mconcat (pubKeyInputsAt pkh ptx)

{-# INLINABLE publicKeyOutputs #-}
publicKeyOutputs :: TxInfo -> [(PubKeyHash, Value)]
publicKeyOutputs TxInfo {..} =
    [(hash, txOutValue) | TxOut{txOutAddress=Address (PubKeyCredential hash) _, txOutValue} <- txInfoOutputs ]

-- {-# INLINABLE scriptTxInsThat #-}
-- scriptTxInsThat :: TxInfo -> (ValidatorHash -> Bool) -> [TxInInfo]
-- scriptTxInsThat TxInfo {..} pred =
--     [ txIn | txIn@TxInInfo{txInInfoResolved = TxOut{txOutDatumHash=Just datumhash, txOutAddress=Address (ScriptCredential hash) _, txOutValue}}
--         <- txInfoInputs, pred hash ]

{-# INLINABLE scriptOutputsThat #-}
scriptOutputsThat :: (TxOut -> Bool) -> TxInfo -> [TxOut]
scriptOutputsThat pred TxInfo {..} =
    -- [txOut | txOut@TxOut{txOutAddress=Address (ScriptCredential hash) _} <- txInfoOutputs, pred hash ]
    -- [txOut | txOut <- txInfoOutputs, pred txOut ]
    [txOut | txOut@TxOut{txOutAddress=Address (ScriptCredential _) _} <- txInfoOutputs, pred txOut ]

-- Defined in Plutus.V2.Ledger.Contexts
{-# INLINABLE scriptOutputsAt #-}
scriptOutputsAt :: ValidatorHash -> TxInfo -> [TxOut]
scriptOutputsAt = scriptOutputsThat . atScriptAddress

{-# INLINABLE scriptOutputs #-}
scriptOutputs :: TxInfo -> [TxOut]
scriptOutputs = scriptOutputsThat (const True)

{-# INLINABLE inputsThat #-}
inputsThat :: (TxInInfo -> Bool) -> TxInfo -> [TxInInfo]
inputsThat pred txIn = [ i | i <- txInfoInputs txIn, pred i ]

{-# INLINABLE inputThat #-}
inputThat :: (TxInInfo -> Bool) -> TxInfo -> Maybe TxInInfo
inputThat pred txIn = find pred (txInfoInputs txIn)

{-# INLINABLE inputOutsThat #-}
inputOutsThat :: (TxInInfo -> Bool) -> TxInfo -> [TxOut]
inputOutsThat pred txIn = [ txInInfoResolved i | i <- txInfoInputs txIn, pred i ]

{-# INLINABLE inputOutThat #-}
inputOutThat :: (TxInInfo -> Bool) -> TxInfo -> Maybe TxOut
inputOutThat pred txIn = txInInfoResolved <$> find pred (txInfoInputs txIn)

{-# INLINABLE refInputOutsThat #-}
refInputOutsThat :: (TxInInfo -> Bool) -> TxInfo -> [TxOut]
refInputOutsThat pred txIn = [ txInInfoResolved i | i <- txInfoReferenceInputs txIn, pred i ]

{-# INLINABLE refInputOutThat #-}
refInputOutThat :: (TxInInfo -> Bool) -> TxInfo -> Maybe TxOut
refInputOutThat pred txIn = txInInfoResolved <$> find pred (txInfoReferenceInputs txIn)

{-# INLINABLE outputsThat #-}
outputsThat :: (TxOut -> Bool) -> TxInfo -> [TxOut]
outputsThat pred txIn = [ i | i <- txInfoOutputs txIn, pred i ]

-- scriptInputOutsAt :: ValidatorHash -> TxInfo -> [TxOut]
-- scriptInputOutsAt h = inputOutsThat ((ScriptCredential h == ) . addressCredential . txOutAddress . txInInfoResolved)

{-# INLINABLE scriptInputOutsThat #-}
scriptInputOutsThat :: (TxOut -> Bool) -> TxInfo -> [TxOut]
scriptInputOutsThat pred TxInfo {..} =
    [txOut | TxInInfo { txInInfoResolved = txOut@TxOut{txOutAddress=Address (ScriptCredential _) _} } <- txInfoInputs, pred txOut ]

{-# INLINABLE scriptInputsThat #-}
scriptInputsThat :: (TxInInfo -> Bool) -> TxInfo -> [TxInInfo]
scriptInputsThat pred TxInfo {..} =
    -- [txOut | TxInInfo { txInInfoResolved = txOut@TxOut{txOutAddress=Address (ScriptCredential hash) _} } <- txInfoInputs, pred hash ]
    -- [txInInfoResolved | TxInInfo {..} <- txInfoInputs, pred txInInfoResolved ]
    [txIn | txIn@TxInInfo { txInInfoResolved = TxOut{txOutAddress=Address (ScriptCredential _) _} } <- txInfoInputs, pred txIn ]

{-# INLINABLE scriptInputsAt #-}
scriptInputsAt :: ValidatorHash -> TxInfo -> [TxInInfo]
scriptInputsAt = scriptInputsThat . inputAtScriptAddress

{-# INLINABLE scriptInputOutsAt #-}
scriptInputOutsAt :: ValidatorHash -> TxInfo -> [TxOut]
scriptInputOutsAt = scriptInputOutsThat . atScriptAddress

{-# INLINABLE scriptInputs #-}
scriptInputs :: TxInfo -> [TxOut]
scriptInputs = scriptInputOutsThat (const True)

{-# INLINABLE spentScriptInput #-}
spentScriptInput :: ValidatorHash -> TxInfo -> Bool
spentScriptInput h txIn = length (scriptInputOutsAt h txIn) > 0

{-# INLINABLE valueSpentFromScript #-}
-- | Get the total value spent by the given validator in this transaction.
valueSpentFromScript :: ValidatorHash -> TxInfo -> Value
valueSpentFromScript h ptx =
    let outputs = map txOutValue $ scriptInputOutsAt h ptx
    in mconcat outputs

{-# INLINABLE valueReferenced #-}
valueReferenced :: TxInfo -> Value
valueReferenced = foldMap (txOutValue . txInInfoResolved) . txInfoReferenceInputs

{-# INLINABLE txInCred #-}
txInCred :: TxInInfo -> Credential
txInCred = addressCredential . txOutAddress . txInInfoResolved

-- {-# INLINABLE sameScriptInput #-}
-- sameScriptInput :: TxInInfo -> TxInInfo -> Bool
-- sameScriptInput target = on (==) txInCred target -- (txInCred target == ) . txInCred 

{-# INLINABLE atScriptAddress #-}
atScriptAddress :: ValidatorHash -> TxOut -> Bool
atScriptAddress hash = (ScriptCredential hash ==) . addressCredential . txOutAddress

{-# INLINABLE inputAtScriptAddress #-}
inputAtScriptAddress :: ValidatorHash -> TxInInfo -> Bool
inputAtScriptAddress hash = (ScriptCredential hash ==) . addressCredential . txOutAddress . txInInfoResolved

{-# INLINABLE thatScriptAddress #-}
thatScriptAddress :: (ValidatorHash -> Bool) -> TxOut -> Bool
thatScriptAddress pred =
    (\case
        ScriptCredential hash -> pred hash
        _ -> False
    ) . addressCredential . txOutAddress

-- does given script have a single output with specified datum
{-# INLINABLE hasOnlyDatumAt #-}
hasOnlyDatumAt :: PlutusTx.FromData a => TxInfo -> (a -> Bool) -> ValidatorHash -> Bool
hasOnlyDatumAt txIn pred hash = or . fmap pred $ (lookupDatum txIn . txOutDatum) =<<
    -- onlyAndSatisfies (atScriptAddress hash) (scriptOutputs txIn)
    only (scriptOutputsThat (atScriptAddress hash) txIn)

-- does given script have a single output with the same datum as current UTxO - so datum is continuing
{-# INLINABLE hasOnlyOwnDatum #-}
hasOnlyOwnDatum :: ScriptContext -> Bool
hasOnlyOwnDatum ctx@ScriptContext{scriptContextTxInfo=txIn} =
    isJust $ only (scriptOutputsThat ((ownDatum ==) . txOutDatum) txIn)
    where
        ownDatum = txOutDatum $ txInInfoResolved $ fromMaybeTraceError "not script" $ findOwnInput ctx

-- Has a single output of given script that satisfies pred
{-# INLINABLE hasOnlyScriptOutThat #-}
hasOnlyScriptOutThat :: ValidatorHash -> (TxOut -> Bool) -> TxInfo -> Bool
hasOnlyScriptOutThat hash pred txIn = isJust $ onlyAndSatisfies pred (PlutusBuoy.Plutus.V2.Contexts.scriptOutputsAt hash txIn)

-- Has a single script input that satisfies pred
{-# INLINABLE hasOnlyScriptIn #-}
hasOnlyScriptIn :: (TxOut -> Bool) -> TxInfo -> Bool
hasOnlyScriptIn pred = isJust . onlyAndSatisfies pred . scriptInputs

{-# INLINABLE hasOnlyScriptInAt #-}
hasOnlyScriptInAt :: ValidatorHash -> TxInfo -> Bool
hasOnlyScriptInAt hash = hasOnlyScriptIn (atScriptAddress hash)

{-# INLINABLE onlyPaidTo #-}
onlyPaidTo :: TxInfo -> PubKeyHash -> Bool
onlyPaidTo txIn hash = valuePaidTo txIn hash == foldMap snd (publicKeyOutputs txIn)

{-# INLINABLE getPubKeyOutValues #-}
getPubKeyOutValues :: TxInfo -> [(PubKeyHash, Value)]
getPubKeyOutValues TxInfo { .. } =
    let pubKeyOuts = mapMaybe (\TxOut {..} -> (, txOutValue) <$> toPubKeyHash txOutAddress) txInfoOutputs
    in map (foldl1 $ biliftA2 const (<>)) $ groupOn fst pubKeyOuts

{-# INLINABLE valueTransferredToScript #-}
valueTransferredToScript :: ValidatorHash -> TxInfo -> Value
valueTransferredToScript script txIn = valueLockedBy txIn script - valueSpentFromScript script txIn

-- -- Compile value transfers in transaction, optionally filter payment credentials of interest
-- {-# INLINABLE txValueTransfers #-}
-- txValueTransfersFiltered :: TxInfo -> [Address] -> (Credential, Value)
-- txValueTransfersFiltered txIn addrs =
--     let
--         addr
--         creds = map (addressCredential . txOutAddress) addrs
--         ins  = [ out | out <- txInInfoResolved `map` txInfoInputs, addressCredential (txOutAddress out) `elem` creds ]
--         outs = [ out | out <- txInfoOutputs, addressCredential (txOutAddress out) `elem` creds ]

{-# INLINABLE assetTransferredTo #-}
assetTransferredTo :: CurrencySymbol -> TokenName -> ValidatorHash -> TxInfo -> Integer
assetTransferredTo currency token = (valueOf' currency token . ) . valueTransferredToScript

{-# INLINABLE aclTransferredTo #-}
aclTransferredTo :: AssetClass -> ValidatorHash -> TxInfo -> Integer
aclTransferredTo (AssetClass (currency, token)) = (valueOf' currency token . ) . valueTransferredToScript

{-# INLINABLE valueSpentFromHolder #-}
valueSpentFromHolder :: CurrencySymbol -> TxInfo -> Value
valueSpentFromHolder currency = ((mconcat . map txOutValue) .) . inputOutsThat $ txInHasCurrency currency

{-# INLINABLE valueLockedByHolder #-}
-- | Get the total value locked by the UTxOs containing the given currency symbol in this transaction.
valueLockedByHolder :: CurrencySymbol -> TxInfo -> Value
valueLockedByHolder currency =
    ((mconcat . map txOutValue) .) . scriptOutputsThat $ txOutHasCurrency currency

{-# INLINABLE valueTransferredToHolder #-}
valueTransferredToHolder :: CurrencySymbol -> TxInfo -> Value
valueTransferredToHolder currency txIn = valueLockedByHolder currency txIn - valueSpentFromHolder currency txIn

{-# INLINABLE assetTransferredToHolder #-}
assetTransferredToHolder :: CurrencySymbol -> TokenName -> CurrencySymbol -> TxInfo -> Integer
assetTransferredToHolder currency token = (valueOf' currency token . ) . valueTransferredToHolder

{-# INLINABLE aclTransferredToHolder #-}
aclTransferredToHolder :: AssetClass -> CurrencySymbol -> TxInfo -> Integer
aclTransferredToHolder (AssetClass (currency, token)) = (valueOf' currency token . ) . valueTransferredToHolder

{-# INLINABLE signedByUTxO #-}
signedByUTxO :: TxInfo -> Maybe TxOut -> Bool
signedByUTxO txIn = (Just True ==) . fmap (txSignedByPubKey txIn . txOutAddress)

{-# INLINABLE findPolicyHolder #-}
findPolicyHolder :: TxInfo -> CurrencySymbol -> Maybe TxOut
findPolicyHolder txIn currency = inputOutThat (txInHasCurrency currency) txIn

{-# INLINABLE findPolicyHolderRef #-}
findPolicyHolderRef :: TxInfo -> CurrencySymbol -> Maybe TxOut
findPolicyHolderRef txIn currency = refInputOutThat (txInHasCurrency currency) txIn

{-# INLINABLE signedByPolicyHolder #-}
signedByPolicyHolder :: TxInfo -> CurrencySymbol -> Bool
signedByPolicyHolder txIn = signedByUTxO txIn . findPolicyHolder txIn

{-# INLINABLE signedByPolicyHolderRef #-}
signedByPolicyHolderRef :: TxInfo -> CurrencySymbol -> Bool
signedByPolicyHolderRef txIn = signedByUTxO txIn . findPolicyHolderRef txIn

inlineDatum :: FromData a => OutputDatum -> Maybe a
inlineDatum (OutputDatum d) = fromDatum d
inlineDatum _ = Nothing

outputDatums :: FromData a => ValidatorHash -> TxInfo -> [Maybe a]
outputDatums hash txIn = map (readTxOutDatum txIn) $ scriptOutputsThat (atScriptAddress hash) txIn

-- TODO: check if fromMaybe check is ever needed
requireOutputDatums :: FromData a => ValidatorHash -> TxInfo -> [a]
requireOutputDatums hash txIn = map
    (fromMaybeTraceError "Datum not present in atleast some of UTxOs" . readTxOutDatum txIn)
    (scriptOutputsThat (atScriptAddress hash) txIn)

{-# INLINABLE getOwnTokens #-}
getOwnTokens :: ScriptContext -> Value -> [(TokenName, Integer)]
getOwnTokens ctx = concat . fmap PMap.toList . PMap.lookup (ownCurrencySymbol ctx) . getValue

{-# INLINABLE getOwnCurrencyValue #-}
getOwnCurrencyValue :: ScriptContext -> Value -> Value
getOwnCurrencyValue ctx = maybe mempty (Value . PMap.singleton (ownCurrencySymbol ctx)) . PMap.lookup (ownCurrencySymbol ctx) . getValue

-- Was such Value spent or referenced from signing PubKeyHash
{-# INLINABLE spentOrWitnessedValue #-}
spentOrWitnessedValue :: TxInfo -> (Value -> Bool) -> Bool
spentOrWitnessedValue txIn pred = if spentValue txIn pred then True else referenced
    where
        referenced = (witnessed . txInInfoResolved) `any` txInfoReferenceInputs txIn
        witnessed txOut = if pred $ txOutValue $ txOut
            then txSignedByPubKey txIn $ txOutAddress txOut
            else False

-- Was such Value spent or referenced
{-# INLINABLE spentOrReferencedValue #-}
spentOrReferencedValue :: TxInfo -> (Value -> Bool) -> Bool
spentOrReferencedValue txIn pred = if spentValue txIn pred then True else referencedValue txIn pred

{-# INLINABLE spentAndReferencedValue #-}
spentAndReferencedValue :: TxInfo -> Value
spentAndReferencedValue TxInfo{..} = foldMap (txOutValue . txInInfoResolved) $ txInfoReferenceInputs <> txInfoInputs

{-# INLINABLE spentValue #-}
spentValue :: TxInfo -> (Value -> Bool) -> Bool
spentValue txIn pred = (pred . txOutValue . txInInfoResolved) `any` txInfoInputs txIn

{-# INLINABLE referencedValue #-}
referencedValue :: TxInfo -> (Value -> Bool) -> Bool
referencedValue txIn pred = (pred . txOutValue . txInInfoResolved) `any` txInfoReferenceInputs txIn

{-# INLINABLE txSignedByPubKey #-}
txSignedByPubKey :: TxInfo -> Address -> Bool
txSignedByPubKey txIn Address { addressCredential = PubKeyCredential hash } = txSignedBy txIn hash
txSignedByPubKey _ _ = False

{-# INLINABLE referenceAt #-}
referenceAt :: TxOutRef -> TxInfo -> Maybe TxOut
referenceAt ref txIn = txInInfoResolved <$> find ((ref ==) . txInInfoOutRef) (txInfoReferenceInputs txIn)

-- newtype SortableCredential = { getSortableCredential :: Credential }

-- newtype SortableCredential = { getSortableCredential :: Credential }
-- instance Ord SortableCredential where
--     SortableCredential{getSortableCredential = PubKeyCredential a} `compare` SortableCredential{getSortableCredential = ScriptCredential b} = LT
--     SortableCredential{getSortableCredential = ScriptCredential a} `compare` SortableCredential{getSortableCredential = PubKeyCredential b} = GT
--     SortableCredential{getSortableCredential = a} `compare` SortableCredential{getSortableCredential = b} =
instance Ord Credential

{-# INLINABLE addressesValue #-}
addressesValue :: [TxOut] -> [(Credential, Value)]
addressesValue = monoidSortAssocsWith (addressCredential . txOutAddress) txOutValue

{-# INLINABLE addressesSpentFilter #-}
addressesSpentFilter :: (Credential -> Bool) -> TxInfo -> [(Credential, Value)]
addressesSpentFilter pred = addressesValue . filter (pred . addressCredential . txOutAddress) . map txInInfoResolved . txInfoInputs

{-# INLINABLE addressesPaidFilter #-}
addressesPaidFilter :: TxInfo -> [(Credential, Value)]
addressesPaidFilter = addressesValue . txInfoOutputs

{-# INLINABLE requireOwnInput #-}
requireOwnInput :: ScriptContext -> TxInInfo
requireOwnInput = fromJust . findOwnInput

{-# INLINABLE ownInputs #-}
ownInputs :: ScriptContext -> [TxInInfo]
ownInputs ctx@ScriptContext{scriptContextTxInfo=txIn} = scriptInputsAt (ownHash ctx) txIn

{-# INLINABLE ownInputOuts #-}
ownInputOuts :: ScriptContext -> [TxOut]
ownInputOuts ctx@ScriptContext{scriptContextTxInfo=txIn} = scriptInputOutsAt (ownHash ctx) txIn

{-# INLINABLE requireSpendingRef #-}
requireSpendingRef :: ScriptContext -> TxOutRef
requireSpendingRef ScriptContext { scriptContextPurpose = Spending ref } = ref
requireSpendingRef _ = traceError "requireSpendingRef not spending"