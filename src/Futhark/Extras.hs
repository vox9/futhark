module Futhark.Extras (extras)
where

import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.AutoImp
import Futhark.Util.Pretty (docString, Pretty (pretty))
import Language.Futhark.Prop (Prog)
import Language.Futhark.Syntax

-- TODO: Support nested opaque types
extras :: Prog -> ImplementM ()
extras m = forM_ (progDecs m) onDec

onDec :: DecBase Info VName -> ImplementM ()
onDec (ValDec (ValBind { valBindEntryPoint = Just (Info {unInfo = ep}) })) =
  let it = map entryParamType $ entryParams ep
      ot = entryReturn ep
    in forM_ (ot : it) onType
onDec _ = pure ()

typeName :: EntryType -> String
typeName (EntryType { entryAscribed = Just a }) = docString $ pretty a
typeName (EntryType { entryType = t }) = docString $ pretty t

deArray :: EntryType -> EntryType
deArray e = EntryType (deArraySt $ entryType e) (deArrayTe <$> entryAscribed e)

deArraySt :: StructType -> StructType
deArraySt (Array _ _ t) = Scalar t
deArraySt t = t

deArrayTe :: TypeExp a VName -> TypeExp a VName
deArrayTe (TEArray _ t _) = deArrayTe t
deArrayTe t = t

onType :: EntryType -> ImplementM ()
onType t@(EntryType { entryType = Array _ d _ }) =
  let st = deArray t
   in arrayFuns st (length d)
onType _ = pure ()

arrayFuns :: EntryType -> Int -> ImplementM ()
arrayFuns scalarType rank = do
  declare (newFn scalarType rank)
  declare (updateFn scalarType rank)

newFn :: EntryType -> Int -> T.Text
newFn scalarType rank = newFn'
  where
    flattenD = case rank of
      1 -> "id"
      2 -> "flatten"
      _ -> "flatten_" ++ show rank ++ "d"
    scalarName = typeName scalarType
    genericDims = foldl (++) "" $ map (("[dim"++) . (++"]") . show) $ [1..rank]
    arrayName = genericDims ++ scalarName
    scalarFields = fields $ entryType scalarType
    scalarFieldsSs = intercalate " " $ map (++"s") scalarFields
    scalarFieldsSsF = "(" ++ (intercalate ") (" $ map (((flattenD ++ " ")++) . (++"s")) scalarFields) ++ ")"
    scalarFieldSs = intercalate " " scalarFields
    scalarFieldCs = intercalate "," scalarFields
    scalarFieldInit = "{" ++ scalarFieldCs ++ "}"
    nFields = length scalarFields
    newFn' = T.pack $ "entry " ++ scalarName ++ "_new_" ++ show rank ++ "d " ++ genericDims ++ " " ++ scalarFieldsSs ++ ": " ++ arrayName ++ " = un" ++ flattenD ++ " (map" ++ show nFields ++ "(\\" ++ scalarFieldSs ++ " -> " ++ scalarFieldInit ++ ") " ++ scalarFieldsSsF ++ ")"

updateFn :: EntryType -> Int -> T.Text
updateFn scalarType rank = updateFn'
  where
    scalarName = typeName scalarType
    genericDims = foldl (++) "" $ map (("[dim"++) . (++"]") . show) $ [1..rank]
    arrayName = genericDims ++ scalarName
    indices = map (("idx"++) . show) $ [1..rank]
    indexPs = intercalate " " $ map (("("++) . (++": i64)")) indices
    indexCs = intercalate "," indices
    updateFn' = T.pack $ "entry " ++ scalarName ++ "_update_" ++ show rank ++ "d " ++ genericDims ++ " (A: *" ++ arrayName ++ ") " ++ indexPs ++ " (v: " ++ scalarName ++ "): " ++ arrayName ++ " = A with [" ++ indexCs ++ "] = v"

fields :: StructType -> [String]
fields (Scalar (Record t)) = map nameToString $ M.keys t
fields _ = error "TODO: Hopefully impossible (982qwduioj)"
