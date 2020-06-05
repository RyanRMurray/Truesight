{-# LANGUAGE RecordWildCards #-}
module Entities where


    import Data.Map (Map)
    import qualified Data.Map as M

    import Data.Maybe (mapMaybe, listToMaybe)


    data Meter = Uncapped
        { value     :: Int
        }
        | Capped
        { value     :: Int
        , max_value :: Int
        }

    instance Show Meter where
        show (Uncapped m) = show m
        show (Capped v m) = show v ++ "/" ++ show m

    -- maximise meter
    (>^) :: Meter -> Meter
    (>^) Uncapped{..} = Uncapped value
    (>^) Capped{..}   = Capped max_value max_value

    -- alter meter
    (>+) :: Meter -> Int -> Meter
    (>+) Uncapped{..} v = Uncapped (value + v)
    (>+) Capped{..} v
        | value + v > max_value = Capped max_value   max_value
        | otherwise             = Capped (value + v) max_value


    type Meters = Map String Meter

    -- create a meter set from a list
    ms_fromList :: [(String, Maybe Int)] -> Meters
    ms_fromList =
        let insert' m (str, Just n)  = M.insert str (Capped n n) m
            insert' m (str, Nothing) = M.insert str (Uncapped 0) m
        in
            foldl insert' M.empty

    -- Statuses either decay over time or are removed manually.
    data Status = Decaying
        { status_id :: String
        , timer     :: Int
        }
        | Permanent {status_id :: String}

    instance Show Status where
        show Decaying{..}  = status_id ++ ": " ++ show timer ++ " rounds"
        show Permanent{..} = status_id

    -- Decay a status if it can decay. Remove fully decayed statuses automatically.
    decay_status :: Status -> Maybe Status
    decay_status Decaying{..}
        | timer == 0 = Nothing
        | otherwise  = Just $ Decaying status_id (timer - 1)
    decay_status p = Just p

    -- Entities can be grouped by initiative score for simplicity.
    data Entity = Single
        { entity_id :: String
        , meters    :: Meters
        , statuses  :: [Status]
        }
        | Group [Entity]

    instance Show Entity where
        show Single{..} =
            entity_id ++ ":\n" ++
            "\tMeters:\n" ++
            (concat $ M.mapWithKey (\k a -> "\t\t" ++ k ++ ": " ++ show a ++ "\n") meters) ++
            "\tStatuses:\n" ++
            (concatMap (\s -> "\t\t" ++ show s ++ "\n") statuses)

        show (Group es) = concatMap show es

    -- decay any timed statuses
    step_entity :: Entity -> Entity
    step_entity (Group es) = Group $ map step_entity es
    step_entity Single{..} = Single
        entity_id
        meters
        (mapMaybe decay_status statuses)

    ms_adjust :: Entity -> String -> (Meter -> Meter) -> Entity
    ms_adjust Single{..} m adj = Single
        entity_id
        (M.adjust adj m meters)
        statuses
    ms_adjust _ _ _ = undefined

    {--
        Initiative is the state of a given combat. There is an initiative counter that counts down
        from the highest rolled number to the lowest, plus any possible interruptions (TODO)


    --}
    data Initiative = Initiative
        { counter  :: Float
        , entities :: Map Float Entity
        , rounds   :: Int
        }

    instance Show Initiative where
        show Initiative{..} =
            let target = entity_id $ entities M.! counter
                (o1,o2) = M.partitionWithKey (\k _ -> k <= counter) entities
                display_init m = concatMap (\(k,e) -> "\t" ++ show k ++ " - " ++ entity_id e ++ "\n") $ M.toDescList m
            in
                "ROUND " ++ show rounds ++ ", " ++ target ++ "'s TURN:\n" ++
                display_init o1 ++
                display_init o2


    step_initiative :: Initiative -> Initiative
    step_initiative Initiative{..} =
        let next =  listToMaybe $ dropWhile (>= counter) $ reverse $ M.keys entities
        in
            case next of
                Just n -> Initiative
                    n
                    (M.adjust step_entity counter entities)
                    rounds
                Nothing -> Initiative
                    (last $ M.keys entities)
                    (M.adjust step_entity counter entities)
                    (rounds + 1)







