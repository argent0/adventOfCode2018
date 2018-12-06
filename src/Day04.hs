{-# LANGUAGE OverloadedStrings #-}
{- Abusing maps -}
module Day04 (
	solve_1
) where

import qualified Data.Attoparsec.Text as P
import qualified Data.Time as T
import qualified Data.Text as Tx
import Control.Applicative
import qualified System.IO as SysIO
import qualified Data.List as DL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Either (partitionEithers)
import Debug.Trace
import Control.Lens
import Data.Function (on)
import Control.Arrow ((&&&), second)
import Control.Monad (join)

newtype Id = Id Integer deriving (Eq, Ord, Show)

parseId :: P.Parser Id
parseId = P.char '#' >> (Id <$> P.decimal)

data Event = Wakes | Falls | Shift Id deriving (Show, Eq)

parseEvent :: P.Parser Event
parseEvent = do
	keyword <- P.string "falls" <|>
		P.string "wakes" <|>
		P.string "Guard"
	case keyword of
		"falls" -> P.string " asleep" >> pure Falls
		"wakes" -> P.string " up" >> pure Wakes
		"Guard" -> do
			_ <- P.char ' '
			id <- parseId
			_ <- P.string " begins shift"
			pure $ Shift id

parseTime :: P.Parser T.LocalTime
parseTime = do
	_ <- P.char '['
	dayStr <- P.takeWhile1 (/= ']')
	_ <- P.char ']'
	(T.parseTimeM False T.defaultTimeLocale "%Y-%m-%d %H:%M" . Tx.unpack) dayStr

data Record = Record T.LocalTime Event deriving (Show, Eq)

parseRecord :: P.Parser Record
parseRecord = do
	time <- parseTime
	_ <- P.char ' '
	event <- parseEvent
	pure $ Record time event

instance Ord Record where
	compare (Record t _) (Record tt _) = compare t tt

data GuardEvent = GEWake T.LocalTime | GEFalls T.LocalTime deriving (Show, Eq)

geTime :: Lens' GuardEvent T.LocalTime
geTime = lens g s
	where
	g (GEWake lt) = lt
	g (GEFalls lt) = lt
	s (GEWake _) lt = GEWake lt
	s (GEFalls _) lt = GEFalls lt

instance Ord GuardEvent where
	compare = compare `on` (^. geTime)

data DayRecord = DayRecord Id T.LocalTime [GuardEvent] deriving Show

drGid :: Lens' DayRecord Id
drGid = lens g s
	where
	g (DayRecord id _ _ ) = id
	s (DayRecord _ lt evs) id = DayRecord id lt evs

drTime :: Lens' DayRecord T.LocalTime
drTime = lens g s
	where
	g (DayRecord _ lt _ ) = lt
	s (DayRecord id _ evs) lt = DayRecord id lt evs

sleepMinutes :: Getter DayRecord Integer
sleepMinutes = to g
	where
	g :: DayRecord -> Integer
	g (DayRecord _ lt evs) = DL.foldl' folder 0 $ pairs evs
	folder :: Integer -> (GuardEvent, GuardEvent) -> Integer
	folder acc (GEFalls ft, GEWake wt) = acc + (((-) `on` toMin) wt ft)
	toMin :: T.LocalTime -> Integer
	toMin = fromIntegral . T.todMin . T.localTimeOfDay

sleepPattern :: Getter DayRecord [Bool] -- True: is sleeping
sleepPattern = to g
	where
	g :: DayRecord -> [Bool]
	g (DayRecord _ lt evs) = go [] False 0 evs
	go acc curr m [] = (replicate (60-m) curr) ++ acc
	go acc curr m fel@(GEWake et:evs) = if m < toMin et
		then go (curr:acc) curr (m+1) fel
		else go (False:acc) False (m+1) evs
	go acc curr m fel@(GEFalls et:evs) = if m < toMin et
		then go (curr:acc) curr (m+1) fel
		else go (True:acc) True (m+1) evs
	toMin :: T.LocalTime -> Int
	toMin = fromIntegral . T.todMin . T.localTimeOfDay


pairs :: [a] -> [(a,a)]
pairs = go []
	where
	go :: [(a,a)] -> [a] -> [(a,a)]
	go acc [] = acc
	go acc (a:b:as) = go ((a,b):acc) as

consGuardEvent :: GuardEvent -> DayRecord -> DayRecord
consGuardEvent ge (DayRecord gid lt gevs) = DayRecord gid lt (ge:gevs)

sortDayRecord :: DayRecord -> DayRecord
sortDayRecord (DayRecord gid lt evs) = DayRecord gid lt $ DL.sort evs

dayRecords :: [Record] -> [DayRecord]
dayRecords = 
	(sortDayRecord <$>) . DL.foldl' (flip folder) []
	where
	folder :: Record -> [DayRecord] -> [DayRecord]
	folder (Record lt (Shift gid)) acc = (DayRecord gid lt []):acc
	folder (Record lt ev) (dr:acc) = case ev of
		Wakes -> (consGuardEvent (GEWake lt) dr):acc
		Falls -> (consGuardEvent (GEFalls lt) dr):acc

showSleepPattern :: [Bool] -> String
showSleepPattern = map mapper
	where
	mapper True = '#'
	mapper False = '.'

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

solve_1 :: IO ()
solve_1 = 
	SysIO.withFile "inputs/day04b" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		let day_records =
			DL.sortBy (compare `on` (^. drTime)) $
			(dayRecords $ DL.sort $
			snd $ partitionEithers $
			fmap (P.parseOnly parseRecord . Tx.pack) file_lines)
		let worst_guard =
			DL.maximumBy (compare `on` snd) $ Map.assocs $ Map.fromListWith (+) $
			(((^. drGid) &&& (^. sleepMinutes)) <$>) $ day_records
		print worst_guard
		print $
			DL.maximumBy (compare `on` snd) $
			Map.assocs $ Map.fromListWith (+) $
			fmap (second (tr)) $
			join $ fmap (enumerate) $
			fmap (reverse . (^. sleepPattern)) $
			filter ((== (fst worst_guard)) . (^. drGid)) day_records
	where
	tr :: Bool -> Integer
	tr True = 1
	tr False = 0

solve_2 :: IO ()
solve_2 = 
	SysIO.withFile "inputs/day04" SysIO.ReadMode $ \input_fh ->
		(lines <$> SysIO.hGetContents input_fh) >>= \file_lines -> do
		let day_records =
			DL.sortBy (compare `on` (^. drTime)) $
			(dayRecords $ DL.sort $
			snd $ partitionEithers $
			fmap (P.parseOnly parseRecord . Tx.pack) file_lines)
		print $
			DL.maximumBy (compare `on` (snd . snd)) $
			Map.assocs $
			Map.map (DL.maximumBy (compare `on` snd) . Map.assocs . Map.fromListWith (+)) $
			Map.fromListWith (++) $
			fmap ((^. drGid) &&& (enumerate . reverse . fmap tr . (^. sleepPattern))) $
			day_records
	where
	tr :: Bool -> Integer
	tr True = 1
	tr False = 0
