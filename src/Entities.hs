{-# LANGUAGE RecordWildCards #-}
module Entities where


    import Data.Map (Map)
    import qualified Data.Map as M
    
    import Data.Maybe (mapMaybe)


    data Meter = Meter
        { value     :: Int
        , max_value :: Int
        } deriving (Show)

    -- maximise meter
    (>^) :: Meter -> Meter
    (>^) Meter{..} = Meter max_value max_value 

    -- alter meter
    (>+) :: Meter -> Int -> Meter
    (>+) Meter{..} v = Meter (value + v) max_value
    
    type Meters = Map String Meter

    -- create a meter set from a list
    ms_fromList :: [(String, Int)] -> Meters
    ms_fromList = foldl (\m (n,v) -> M.insert n (Meter v v) m) M.empty

    data Status = Status
        { status_id :: String
        , timer     :: Int
        } deriving (Show)

    step_status :: Status -> Maybe Status
    step_status Status{..}
        | timer == 0 = Nothing
        | otherwise  = Just $ Status status_id (timer - 1)

    --Entities can be grouped by initiative
    data Entity = Single
        { entity_id :: String
        , meters    :: Map String Meter
        , statuses  :: [Status]
        }
        | Group [Entity] 
        deriving (Show)

    step_entity :: Entity -> Entity
    step_entity (Group es) = Group $ map step_entity es
    step_entity Single{..} = Single
        entity_id
        meters
        (mapMaybe step_status statuses)


    data Initiative = Initiative
        { counter  :: Int
        , entities :: Map Int Entity
        , rounds   :: Int
        } deriving (Show)


