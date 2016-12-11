module Lib where

import qualified Data.HashMap as HM
import qualified Data.Set as S
--import Data.Hashable.Class.Hashable

type Token = String
type Dictionary = S.Set Token
type Sentence = [Token]
type SentencePair = (Sentence,Sentence)
type Corpus = [SentencePair]
--example [(["das","Haus"], ["the","House"]), (["das","Buch"],["the","book"]), (["ein","Buch"], ["a","book"])]
type Probs = HM.Map (Token, Token) Float
type Counts = HM.Map (Token, Token) Float
type Totals = HM.Map Token Float


initProbs :: Corpus -> Probs
initProbs corpus = foldl initializer HM.empty corpus
  where initializer = foldPairToMap insertProbs
        insertProbs _ e f ac = HM.insert (e,f) probs ac
        probs = 1 / ( fromIntegral . S.size . snd . extractDict $ corpus)

extractDict :: Corpus -> (Dictionary, Dictionary)
extractDict = foldr (\(f, e) (dice, dicf) -> (foldr S.insert dice e , foldr S.insert dicf f)) (S.empty, S.empty)



--stotal :: Token -> Sentence -> Probs -> Float
--stotal e f mp = foldr (\x acc -> acc + ((HM.!) mp (e,x))) 0 f

--counter :: Token -> Sentence -> Probs -> (Counts, Totals)

foldPairToMap insfun cnt k@(f,e) = foldr (\x acc -> foldr (\y ac -> insfun k x y ac ) acc f) cnt e

--counts :: Probs -> Corpus -> Counts
--counts probs = foldl counter HM.empty
--  where counter = foldPairToMap insertCount
--        insertCount (f, e) x y ac =  HM.insertWith (+) (x,y) (((HM.!) probs (x,y)) / ((HM.!) (normalization e f) x)) ac
--        normalization e f = foldr (\x acc -> foldr (\y ac -> HM.insertWith (+) x ((HM.!) probs (x,y)) ac) acc f) HM.empty e

combinator f x y = f x (f x y)


step :: Corpus -> Probs -> Probs
step corpus probs = HM.mapWithKey (\k@(_,f) _ -> (/) ((HM.!) counts k) ((HM.!) totals f)) probs
  where counts = foldl counter HM.empty corpus
        totals = foldl totalcount HM.empty corpus :: Totals
        counter = foldPairToMap insertCount
        totalcount = foldPairToMap insertTotal
        insertCount (f, e) x y ac =  HM.insertWith (+) (x,y) (((HM.!) probs (x,y)) / ((HM.!) (normalization e f) x)) ac
        insertTotal (f, e) x y ac =  HM.insertWith (+) y (((HM.!) probs (x,y)) / ((HM.!) (normalization e f) x)) ac
        normalization e f = foldr (\x acc -> foldr (\y ac -> HM.insertWith (+) x ((HM.!) probs (x,y)) ac) acc f) HM.empty e
