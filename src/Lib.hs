module Lib where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Text.Printf
--import qualified Data.ByteString.Char8 as BS2
--import Data.Hashable.Class.Hashable

type Token = String  -- BS2.ByteString
type Dictionary = S.Set Token
type Sentence = [Token]
type SentencePair = (Sentence,Sentence)
type Corpus = [SentencePair]
type Probs = HM.HashMap (Token, Token) Double
type Counts = HM.HashMap (Token, Token) Double
type Totals = HM.HashMap Token Double

readCorpus :: String -> String -> IO (Corpus)
readCorpus filef filee = do
  fr <- readFile filef
  en <- readFile filee
  return $ map (\(x,y) -> ( "NULL":(words x), words y)) $ zipWith (\x y -> (x,y)) (lines fr) (lines en)
--  return $ map (\(x,y) -> (map BS2.pack $ "NULL":(words x), map BS2.pack $ words y)) $ zipWith (\x y -> (x,y)) (lines fr) (lines en)

initProbs :: Corpus -> Probs
initProbs corpus = foldl initializer HM.empty corpus
  where initializer = foldPair insertProbs
        insertProbs _ e f ac = HM.insert (e,f) probs ac
        probs = 1 / ( fromIntegral . S.size . snd . extractDict $ corpus)

extractDict :: Corpus -> (Dictionary, Dictionary)
extractDict = foldr (\(f, e) (dice, dicf) -> (foldr S.insert dice e , foldr S.insert dicf f)) (S.empty, S.empty)

foldPair :: (SentencePair -> Token -> Token -> a -> a) -> a -> SentencePair -> a
foldPair insfun cnt k@(f,e) = foldr (\x acc -> foldr (\y ac -> insfun k x y ac ) acc f) cnt e

step :: Corpus -> Probs -> Probs
step corpus probs = {-# SCC "step"#-} HM.mapWithKey (\k@(_,f) _ -> val k f) $ probs
  where val k f = (/) ((HM.!) counts k) ((HM.!) totals f)
        (counts,totals) = foldl counter (HM.empty, HM.empty) corpus
        counter = foldPair inserter
        inserter k x y ac = (insertCount k x y ac, insertTotal k x y ac)
        insertCount k x y ac =  HM.insertWith
                                       (+) -- operation
                                       (x,y) -- key
                                       (((HM.!) probs (x,y)) / ((HM.!) (normalization k) x)) -- value to add
                                       $ fst ac -- map
        insertTotal k x y ac =  HM.insertWith
                                       (+)
                                       y
                                       (((HM.!) probs (x,y)) / ((HM.!) (normalization k) x))
                                       $ snd ac
        normalization  = foldPair (\_ x y acc -> HM.insertWith (+) x ((HM.!) probs (x,y)) acc) HM.empty

loop corpus = (step corpus):(loop corpus)

iterations n corpus = foldr (.) id $ take n (loop corpus)

showProbs :: Probs -> String
showProbs = HM.foldrWithKey (\(e,f) a acc -> if a>0.001 then (unwords [f,e, (printf "%.4f\n" a)])++acc else acc) []
--showProbs = HM.foldrWithKey (\(e,f) a acc -> if a>0.001 then (unwords [BS2.unpack f,BS2.unpack e, (printf "%.4f\n" a)])++acc else acc) []

perplexity :: Corpus -> Probs -> Double
perplexity corpus probs = foldr (\k acc -> acc - (logBase 2 (prob k))) 0 corpus
  where prob (f,e) = foldr (\(x, y) ac -> ac * (probs HM.! (y,x))) 1 $ zip f e
