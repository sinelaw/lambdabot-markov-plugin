{-# LANGUAGE GADTs, TemplateHaskell, MultiParamTypeClasses, TypeSynonymInstances #-}
-- | Markov
module Plugin.Markov (markovPlugin) where

import Lambdabot.Plugin
--import qualified Message as Msg (nick, Nick, Message, showNick, readNick, lambdabotName, nName, names, channels)
--import qualified NickEq as E
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as P
import Data.List(intersperse)

import System.Random as Random
import Control.Monad.Random as MonadRandom


markovPlugin :: Module MarkovState --(Map.Map P.ByteString [P.ByteString])
markovPlugin = newModule
    { moduleDefState  = return P.empty
    , moduleSerialize = Just stdSerial
    , contextual = changeMarkov
    , moduleCmds = return
        [ (command "markov")
            { aliases = []
            , help = say "markov [number of letters | start phrase]"
            , process = listArkov listMarkov length  -- markov
            }
        , (command "warkov")
            { aliases = []
            , help = say "warkov [number of words | start phrase]"
            , process = listArkov listMarkov (length . words)  -- markov
            }
        ]
    }


type MarkovState = P.ByteString

listToMaybe :: [a] -> Maybe a
listToMaybe []        =  Nothing
listToMaybe (a:_)     =  Just a

readMaybe :: (Read a) => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

-- msgChans :: Msg.Message a => a -> [String]
-- msgChans msg = map Msg.nName $ Msg.channels msg

-- instance Module MarkovModule MarkovState where

--     moduleCmds _ = ["markov", "warkov", "comprext"]
--     moduleHelp _ "markov" = "markov [<number of letters> | <start phrase>]"
--     moduleHelp _ "warkov" = "warkov [<number of words> | <start phrase>]"
--     moduleHelp _ "comprext" = "comprext <phrase>"

--     moduleDefState  _ = return P.empty
--     moduleSerialize _ = Just stdSerial

--     process      _ msg _ "comprext" rest = do 
--       ks <- readMS
--       return [listComprext ks rest]
--     process      _ msg _ "markov" rest = listArkov listMarkov length rest
--     process      _ msg _ "warkov" rest = listArkov listWarkov (length . words) rest
--       -- where fixedReply = subRegex regex' markovReply "\\0\0"
--           --regex' = mkRegex $ "(" ++ (intercalate "|" nicks) ++ ")"
--           --nicks = Msg.names "freenode" (msgChans msg)
              

--     contextual   _ msg _ text = do
--       changeMarkov text
--       return []

------------------------------------------------------------------------
      
listArkov lister lenFun rest = do
  ks <- readMS
  let markovReply = lister ks memsize startphrase
      w = words rest
      memsize' = (readMaybe . head $ w) :: Maybe Int
      (memsize, startphrase) = case memsize' of
                                 Nothing -> (lenFun rest, rest)
                                 Just x -> (x, [])
  say markovReply
  --return [markovReply] -- following comments are failed attempt to retrieve list of nicks in channel and prevent highlighting them by adding a suffix


buildChains :: Ord a => Int -> [a] -> Map.Map [a] [a] -> Map.Map [a] [a]
buildChains n xs m = if length xs < n+1 
                    then m 
                    else buildChains n (tail xs) (Map.insertWith (++) k v m)
                        where subseq = take (n+1) xs
                              k = init $ subseq
                              v = [last subseq]

buildChainsB :: Int -> P.ByteString -> Map.Map String String -> Map.Map String String
buildChainsB n xs m = if P.length xs < n+1 
                    then m 
                    else buildChainsB n (P.tail xs) (Map.insertWith (++) k v m)
                        where subseq = P.take (n+1) xs
                              k = P.unpack . P.init $ subseq
                              v = [P.last $ subseq]

buildProbs :: Ord a => Int -> [a] -> Map.Map [a] Int -> Map.Map [a] Int
buildProbs n xs m = if length xs < n+1 
                    then m 
                    else buildProbs n (tail xs) (Map.insertWith (+) k 1 m)
                        where k = take n xs

comprext :: Map.Map [String] Int -> Int -> [String] -> String
comprext _ _     []     = []
comprext m thres (x:xs) = let nums = case Map.lookup [x] m of 
                                       Nothing -> 0
                                       Just x -> x
                          in if nums < thres 
                             then x ++ " " ++ (comprext m thres $ xs)
                             else comprext m thres xs -- improbable word
  
listComprext :: MarkovState -> String -> String
listComprext ks rest = comprext (buildProbs 1 (words . P.unpack $ ks) Map.empty) 4 (words rest)

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
                               
        
listMarkov :: MarkovState -> Int -> String -> String
listMarkov ks n startPhrase = firstKey ++ (generateNext m g' firstKey 150)
    where m = buildChainsB n ks Map.empty
          g = Random.mkStdGen (P.length ks)
          (firstKey, g') = case startPhrase of 
                             [] -> uniformPick (Map.keys m) g
                             _  -> (startPhrase, g)
    
listWarkov :: MarkovState -> Int -> String -> String
listWarkov ks n startPhrase = concat . intersperse " " $ firstKey ++ (generateNext m g' firstKey 20)
    where m = buildChains n (words . P.unpack $ ks) Map.empty
          g = Random.mkStdGen (P.length ks)
          (firstKey, g') = case startPhrase of 
                             [] -> uniformPick (Map.keys m) g
                             _  -> (words startPhrase, g)
    

--changeMarkov :: String -> Markov LB [String]
changeMarkov msg = withMS $ \fm write -> do
                     let fm' = P.append fm (P.pack (" " ++ msg))
                     write fm'
                     return () -- ["wrote message: " ++ msg]
