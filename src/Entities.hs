{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Entities where


    import Data.Map (Map)
    import qualified Data.Map as M
    import Data.Set (Set)
    import qualified Data.Set as S

    import Data.List  (find, sortOn)
    import Data.Maybe (mapMaybe, fromMaybe)
    import Data.Ord   (Down(Down))
    
    import Lens.Micro.Platform




    type InitiativeScore = Rational
    type EntityID = Int
    

    {--
        Flags are generated when an event occurs that may affect something in the current combat,
        such as a character's turn ending. They may optionally carry extra data, such as whose turn
        has ended.
    --}

    data Flag = 
          TurnStart EntityID
        | TurnEnd   EntityID
        | RoundEnd
        deriving (Eq, Ord)



    -- Meters store arbitrary integer values, optionally with a maximum value
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
    (>^) u@Uncapped{..} = u
    (>^) Capped{..}     = Capped max_value max_value

    -- alter meter
    (>+) :: Meter -> Int -> Meter
    (>+) Uncapped{..} v = Uncapped (value + v)
    (>+) Capped{..}   v = Capped (min max_value (value + v)) max_value


    type Meters = Map String Meter

    -- create a meter set from a list
    ms_fromList :: [(String, Maybe Int)] -> Meters
    ms_fromList =
        let insert' m (str, Just n)  = M.insert str (Capped n n) m
            insert' m (str, Nothing) = M.insert str (Uncapped 0) m
        in
            foldl insert' M.empty

    -- Statuses either decay when a specified flag occurs or are removed manually.
    data Status = Decaying
        { status_name :: String
        , timer       :: Int
        , decay_flags :: Set Flag
        }
        | Permanent {status_name :: String}

    type StatusBucket = [Status]

    instance Show Status where
        show Decaying{..}  = status_name ++ ": " ++ show timer ++ " steps"
        show Permanent{..} = status_name


    -- Decay a status if it can decay. Remove fully decayed statuses automatically.
    decay_status ::  Flag -> Status -> Maybe Status
    decay_status f d@Decaying{..}
        | S.notMember f decay_flags = Just d
        | timer == 0                = Nothing
        | otherwise                 = Just $ Decaying status_name (timer - 1) decay_flags
    decay_status _ p = Just p



    {-- 
        An entity is any element that is considered when resolving a round.
        Single: A single character, player or NPC.
        Group:  A group of entities that can have their actions resolve in any order.
                Split into "resolved" and "unresolved" for when turn starts and endings are important
        Interruption: An event that occurs at a given point in a round, such as "On initiative count 20..." events.
    --}
    data Entity = Single
        { entity_id        :: EntityID
        , score            :: InitiativeScore
        , entity_name      :: String
        , meters           :: Meters
        }
        | Group
        { entity_id  :: EntityID
        , score      :: InitiativeScore
        , unresolved :: [Entity]
        , resolved   :: [Entity]
        }
        | Interruption
        { entity_id   :: EntityID
        , score       :: InitiativeScore
        , entity_name :: String
        , description :: String
        }

    instance Show Entity where
        show Single{..} =
            entity_name ++ ":\n" ++
            "\tMeters:\n" ++
            (concat $ M.mapWithKey (\k a -> "\t\t" ++ k ++ ": " ++ show a ++ "\n") meters)

        show Group{..} = concatMap show $ unresolved ++ resolved

        show Interruption{..} = entity_name ++ ": " ++ description

    get_id :: Entity -> EntityID
    get_id Single{..}       = entity_id
    get_id Group{..}        = entity_id
    get_id Interruption{..} = entity_id


    --Adjust a specified meter in the entity
    ms_adjust :: Entity -> String -> (Meter -> Meter) -> Entity
    ms_adjust Single{..} m adj = Single
        entity_id
        score
        entity_name
        (M.adjust adj m meters)
    ms_adjust _ _ _ = undefined

    --Group two entity types together under an ID. Automatically added to "unresolved" sub-group
    entity_union :: EntityID -> InitiativeScore -> Entity -> Entity -> Entity
    entity_union i sc (Group _ _ ur1 re1) (Group _ _ ur2 re2) = Group i sc (ur1 ++ ur2) (re1 ++ re2)
    entity_union i sc e1                  (Group _ _ ur re)   = Group i sc ur           (e1:re)
    entity_union i sc es@(Group _ _ _ _)  e1                  = entity_union i sc e1 es
    entity_union i sc e1                  e2                  = Group i sc []           [e1,e2]



    {--
        Initiative is the state of a given combat. There is an initiative counter that counts down
        from the highest rolled number to the lowest, plus any possible interruptions

    --}

    type EntityPool = Map EntityID Entity
    type StatusPool = Map EntityID StatusBucket

    data Initiative = Initiative
        { _counter    :: InitiativeScore
        , _rounds     :: Int
        , _entities   :: EntityPool
        , _statuses   :: StatusPool
        , _turn_queue :: [(InitiativeScore, EntityID)]
        , _id_pool    :: [EntityID]
        } deriving (Show)

    $(makeLenses ''Initiative)

    -- Decay statuses related to the current flag
    resolve_flag :: Flag -> Initiative -> Initiative
    resolve_flag f i =
        let 
            update_bucket = mapMaybe (decay_status f)
            updated = M.map update_bucket $ _statuses i
        in
            set statuses updated i

    -- Construct the queue from a counter onwards
    construct_queue :: Maybe InitiativeScore -> Initiative -> [(InitiativeScore, EntityID)]
    construct_queue limit i = 
        let
            eipair Single{..}       = (score, entity_id)
            eipair Group{..}        = (score, entity_id)
            eipair Interruption{..} = (score, entity_id)
            limit' = fromMaybe 99999999 limit
        in
            dropWhile (\(s,_) -> s > limit')
            $ sortOn (Down . fst)
            $ map eipair
            $ M.elems (_entities i)

    -- Add entity to initiative. Automatically merges on shared initiatives, reconstructs the queue
    -- to allow for updates
    add_entity :: Entity -> Initiative -> Initiative
    add_entity e i@Initiative{..} =
        let
            mergeID = fmap snd $ find (\(s,_) -> s == score e) $ construct_queue Nothing i
            i' = case mergeID of
                Nothing  -> over entities (M.insert (get_id e) e)      i
                Just mID -> over entities (M.insert mID        merged) i
                    where
                        merged = entity_union mID (score e) e $ _entities M.! mID
        in
            set turn_queue (construct_queue (Just _counter) i') i'



    {--
        Functions for controlling the flow of initiative. 
        Split into turn ends, turn starts and round starts to allow for reactions, ect.
    --}

    end_turn :: Initiative -> Initiative
    end_turn i@Initiative{..} =
        let 
            ((_,last_e):q) = _turn_queue
        in
            set turn_queue q
            $ resolve_flag (TurnEnd last_e) i
    
    --intended to queue every entity. bit hacky but it'll doooo
    next_round :: Initiative -> Initiative
    next_round i@Initiative{..} = 
        let
            i' = resolve_flag RoundEnd i
        in
            set turn_queue (construct_queue Nothing i') i'

    next_turn :: Initiative -> Initiative
    next_turn i@Initiative{..} =
        let
            (next_c, next_e) = head _turn_queue
        in
            set counter next_c
            $ resolve_flag (TurnStart next_e) i