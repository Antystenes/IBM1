module Lib where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Text.Printf
--import Data.Hashable.Class.Hashable

type Token = String
type Dictionary = S.Set Token
type Sentence = [Token]
type SentencePair = (Sentence,Sentence)
type Corpus = [SentencePair]
type Probs = HM.HashMap (Token, Token) Float
type Counts = HM.HashMap (Token, Token) Float
type Totals = HM.HashMap Token Float

testCorpus :: Corpus
testCorpus =  [(["das","Haus"], ["the","House"]), (["das","Buch"],["the","book"]), (["ein","Buch"], ["a","book"])]
--testCorpus = map (\(f, e) -> (words f, words e)) [("Chcę wiedzieć", "Ne ne já bych to chtěl vědět"),
-- ("Ty zaczęłaś", "Ty jsi s tím vyrukovala"),
-- ("Kiedy przyjdzie do domu", "Kdypak přijde"),
-- ("Powiedziałam nieważne", "Povídám , dej s tím pokoj"),
-- ("Przepraszam , że z tym zaczęłam Z nim Nie z tym", "Mrzí mě , že jsem s tím vyrukovala"),
-- ("Ty go wychowałaś , mniej lub bardziej", "S ním , ne s tím S ním jsi vyrukovala"),
-- ("Kiedy przyjdzie mały gnój", "Kdypak se tady ten hajzlík objeví , co"),
-- ("Czy jutro nie są jego urodziny", "Nemá mít náhodou zítra narozeniny"),
-- ("Nie chcę o tym mówić", "Já o tom nechci mluvit"),
-- ("Ona nie chce o tym mówić , o nim", "To věřím , že nechceš")]

readCorpus :: String -> String -> IO (Corpus)
readCorpus filef filee = do
  fr <- readFile filef
  en <- readFile filee
  return $ map (\(x,y) -> ("NULL":(words x), words y)) $ zipWith (\x y -> (x,y)) (lines fr) (lines en)

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
                                       (((HM.!) probs (x,y)) / ((HM.!) (normalization k) x)) -- value to insert
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
showProbs = HM.foldrWithKey (\(e,f) a acc -> if a>0.001 then (unwords [f, e, (printf "%.4f\n" a)])++acc else acc) []

perplexity :: Corpus -> Probs -> Float
perplexity corpus probs = foldr (\k acc -> acc - (logBase 2 (prob k))) 0 corpus
  where prob (f,e) = foldr (\(x, y) ac -> ac * (probs HM.! (y,x))) 1 $ zip f e
