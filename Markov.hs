{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Markov
module Plugin.Markov (theModule) where

import Plugin
import qualified Message as Msg (nick, Nick, Message, showNick, readNick, lambdabotName, nName)
import qualified NickEq as E
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as P

import Text.Printf
import System.Random as Random
import Control.Monad.Random as MonadRandom

$(plugin "Markov")


type MarkovState = P.ByteString
type Markov m a = ModuleT MarkovState m a

instance Module MarkovModule MarkovState where

    moduleCmds _ = ["markov"]
    moduleHelp _ "markov" = "markov <number of letters> [start phrase]"

    moduleDefState  _ = return P.empty
    moduleSerialize _ = Just stdSerial

    process      _ _ _ "markov" rest = listMarkov memsize startphrase
        where w = words rest
              memsize = read . head $ w
              startphrase = intercalate " " (tail w)

    contextual   _ msg _ text = do
      changeMarkov text
      return []

------------------------------------------------------------------------


buildProbs :: Int -> P.ByteString -> Map.Map String String -> Map.Map String String
buildProbs n xs m = if P.length xs < n+1 
                    then m 
                    else buildProbs n (P.tail xs) (Map.insertWith (++) k v m)
                        where subseq = P.take (n+1) xs
                              k = P.unpack . P.init $ subseq
                              v = [P.last subseq]


uniformPick :: Random.RandomGen g => [a] -> g -> (a, g)
uniformPick xs g = MonadRandom.runRand (MonadRandom.fromList . flip zip (repeat 1) $ xs) g


pickFromValues :: (Random.RandomGen g, Ord k) => Map.Map k [v] -> g -> k -> Maybe (v, g)
pickFromValues m g k = case Map.lookup k m of 
                         Nothing -> Nothing
                         Just nextElemsChoices -> Just (nextElem, g')
                             where (nextElem, g') = uniformPick nextElemsChoices g

generateNext :: (Random.RandomGen g, Ord a, Integral b) => Map.Map [a] [a] -> g -> [a] -> b -> [a]
generateNext _ _ _         0 = []
generateNext m g lastElems n = case pickFromValues m g lastElems of
                                 Nothing -> []
                                 Just (nextElem, g') -> [nextElem] ++ (generateNext m g' nextElems (n-1)) 
                                     where nextElems = tail lastElems ++ [nextElem]
                               
        
listMarkov :: Int -> String -> Markov LB [String]
listMarkov n startPhrase = do
    ks <- readMS
    let m = buildProbs n ks Map.empty
        g = Random.mkStdGen (P.length ks)
        (firstKey, g') = case startPhrase of 
                           [] -> uniformPick (Map.keys m) g
                           _  -> (startPhrase, g)
    return $ [firstKey ++ (generateNext m g' firstKey 150)]

changeMarkov :: String -> Markov LB [String]
changeMarkov msg = withMS $ \fm write -> do
                     let fm' = P.append fm (P.pack (" " ++ msg))
                     write fm'
                     return ["wrote message: " ++ msg]
