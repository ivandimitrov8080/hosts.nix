{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive (mk)
import Data.Char (isDigit)
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Time
import Data.Time.Clock.POSIX
import Data.Version (makeVersion)
import System.Exit (ExitCode (..), die, exitWith)
import System.IO (stderr)
import Text.ICalendar.Parser (DecodingFunctions (..), parseICalendar)
import Text.ICalendar.Printer (EncodingFunctions (..), printICalendar)
import Text.ICalendar.Types
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- Section 1: Type Definitions
-------------------------------------------------------------------------------

data Task = Task
  { taskUuid :: Text,
    taskDescription :: Text,
    taskStatus :: TaskStatus,
    taskEntry :: Maybe UTCTime,
    taskStart :: Maybe UTCTime,
    taskDue :: Maybe UTCTime,
    taskModified :: Maybe UTCTime,
    taskPriority :: Maybe TaskPriority,
    taskProject :: Maybe Text,
    taskTags :: [Text],
    taskAnnotations :: [Annotation],
    taskRecur :: Maybe Recurrence,
    taskDepends :: [Text]
  }
  deriving (Show, Eq)

data TaskStatus = TaskPending | TaskCompleted | TaskDeleted
  deriving (Show, Eq)

data TaskPriority = PriorityHigh | PriorityMedium | PriorityLow
  deriving (Show, Eq)

data Annotation = Annotation
  { annotEntry :: UTCTime,
    annotDescription :: Text
  }
  deriving (Show, Eq)

data Recurrence = Recurrence
  { recurFrequency :: RecurFrequency,
    recurInterval :: Int
  }
  deriving (Show, Eq)

data RecurFrequency = RecurDaily | RecurWeekly | RecurMonthly | RecurYearly
  deriving (Show, Eq)

data Format = JSONFormat | ICalFormat
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Section 2: JSON Instances (Aeson)
-------------------------------------------------------------------------------

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> do
    taskUuid <- v .: "uuid"
    taskDescription <- v .: "description"
    taskStatus <- v .:? "status" .!= TaskPending
    taskEntry <- (v .:? "entry") >>= mapM parseFlexibleTimestamp
    taskStart <- (v .:? "start") >>= mapM parseFlexibleTimestamp
    taskDue <- (v .:? "due") >>= mapM parseFlexibleTimestamp
    taskModified <- (v .:? "modified") >>= mapM parseFlexibleTimestamp
    taskPriority <- v .:? "priority"
    taskProject <- v .:? "project"
    taskTags <- v .:? "tags" .!= []
    taskAnnotations <- v .:? "annotations" .!= []
    taskRecur <- (v .:? "recur") >>= mapM parseRecurrence
    taskDepends <- v .:? "depends" .!= []
    return Task {..}

instance ToJSON Task where
  toJSON Task {..} =
    object $
      catMaybes
        [ Just ("uuid" .= taskUuid),
          Just ("description" .= taskDescription),
          Just ("status" .= taskStatus),
          ("entry" .=) . formatTimestamp <$> taskEntry,
          ("start" .=) . formatTimestamp <$> taskStart,
          ("due" .=) . formatTimestamp <$> taskDue,
          ("modified" .=) . formatTimestamp <$> taskModified,
          ("priority" .=) <$> taskPriority,
          ("project" .=) <$> taskProject,
          if null taskTags then Nothing else Just ("tags" .= taskTags),
          if null taskAnnotations then Nothing else Just ("annotations" .= taskAnnotations),
          ("recur" .=) . formatRecurrence <$> taskRecur,
          if null taskDepends then Nothing else Just ("depends" .= taskDepends)
        ]

instance FromJSON TaskStatus where
  parseJSON = withText "TaskStatus" $ \t -> case t of
    "pending" -> return TaskPending
    "completed" -> return TaskCompleted
    "deleted" -> return TaskDeleted
    "recurring" -> return TaskPending -- Recurring task template, treat as pending
    _ -> fail $ "Unknown status: " ++ T.unpack t

instance ToJSON TaskStatus where
  toJSON TaskPending = "pending"
  toJSON TaskCompleted = "completed"
  toJSON TaskDeleted = "deleted"

instance FromJSON TaskPriority where
  parseJSON = withText "Priority" $ \t -> case t of
    "H" -> return PriorityHigh
    "M" -> return PriorityMedium
    "L" -> return PriorityLow
    _ -> fail $ "Unknown priority: " ++ T.unpack t

instance ToJSON TaskPriority where
  toJSON PriorityHigh = "H"
  toJSON PriorityMedium = "M"
  toJSON PriorityLow = "L"

instance FromJSON Annotation where
  parseJSON = withObject "Annotation" $ \v -> do
    entry <- v .: "entry"
    desc <- v .: "description"
    time <- parseFlexibleTimestamp entry
    return $ Annotation time desc

instance ToJSON Annotation where
  toJSON (Annotation time desc) =
    object
      [ "entry" .= formatTimestamp time,
        "description" .= desc
      ]

-- Parse timestamp that can be either Integer (unix timestamp) or String (ISO8601)
parseFlexibleTimestamp :: Value -> Parser UTCTime
parseFlexibleTimestamp (Number n) = parseTimestamp (round n)
parseFlexibleTimestamp (String s) =
  case parseTimeM True defaultTimeLocale "%Y%m%dT%H%M%SZ" (T.unpack s) of
    Just t -> return t
    Nothing -> fail $ "Unable to parse timestamp: " ++ T.unpack s
parseFlexibleTimestamp _ = fail "Timestamp must be a number or string"

parseTimestamp :: Integer -> Parser UTCTime
parseTimestamp ts = return $ posixSecondsToUTCTime (fromIntegral ts)

formatTimestamp :: UTCTime -> Integer
formatTimestamp = round . utcTimeToPOSIXSeconds

parseRecurrence :: Text -> Parser Recurrence
parseRecurrence t
  | t == "daily" = return $ Recurrence RecurDaily 1
  | t == "weekly" = return $ Recurrence RecurWeekly 1
  | t == "monthly" = return $ Recurrence RecurMonthly 1
  | t == "yearly" = return $ Recurrence RecurYearly 1
  | otherwise = case parseIntervalRecurrence t of
      Just r -> return r
      Nothing -> fail $ "Unknown recurrence pattern: " ++ T.unpack t

parseIntervalRecurrence :: Text -> Maybe Recurrence
parseIntervalRecurrence t = do
  let (numStr, unit) = T.span isDigit t
  num <- readMaybe (T.unpack numStr) :: Maybe Int
  freq <- case unit of
    "days" -> Just RecurDaily
    "day" -> Just RecurDaily
    "weeks" -> Just RecurWeekly
    "week" -> Just RecurWeekly
    "months" -> Just RecurMonthly
    "month" -> Just RecurMonthly
    "years" -> Just RecurYearly
    "year" -> Just RecurYearly
    _ -> Nothing
  return $ Recurrence freq num

formatRecurrence :: Recurrence -> Text
formatRecurrence (Recurrence freq 1) = case freq of
  RecurDaily -> "daily"
  RecurWeekly -> "weekly"
  RecurMonthly -> "monthly"
  RecurYearly -> "yearly"
formatRecurrence (Recurrence freq n) = T.pack (show n) <> unit
  where
    unit = case freq of
      RecurDaily -> "days"
      RecurWeekly -> "weeks"
      RecurMonthly -> "months"
      RecurYearly -> "years"

-------------------------------------------------------------------------------
-- Section 3: vTodo Parsing
-------------------------------------------------------------------------------

parseTaskVTodo :: BS.ByteString -> Either String Task
parseTaskVTodo input = do
  -- Parse iCalendar
  let decoded = TLE.decodeUtf8 (BL.fromStrict input)
      decodingFunctions =
        DecodingFunctions
          { dfBS2Text = TLE.decodeUtf8,
            dfBS2IText = mk . TLE.decodeUtf8
          }
  (vcals, warnings) <- parseICalendar decodingFunctions "input.ics" (BL.fromStrict input)

  -- Check for single calendar
  when (length vcals > 1) $
    Left "Multiple calendars found. Expected a single VCALENDAR."

  vcal <- case vcals of
    [] -> Left "No VCALENDAR found in input"
    [cal] -> Right cal
    _ -> Left "Multiple calendars found"

  -- Extract VTODOs
  let vtodos = Map.elems (vcTodos vcal)

  -- Check for single VTODO
  when (length vtodos > 1) $
    Left "Error: Multiple tasks detected. This script processes only a single task."

  vtodo <- case vtodos of
    [] -> Left "No VTODO found in VCALENDAR"
    [v] -> Right v
    _ -> Left "Multiple VTODOs found"

  -- Extract fields
  let uid = TL.toStrict $ uidValue $ vtUID vtodo
  let summary = case vtSummary vtodo of
        Nothing -> Left "Error: Missing required property 'SUMMARY' in VTODO"
        Just s -> Right (TL.toStrict $ summaryValue s)

  summary' <- summary

  let status = case vtStatus vtodo of
        Nothing -> TaskPending
        Just s -> case s of
          TodoNeedsAction _ -> TaskPending
          CompletedTodo _ -> TaskCompleted
          InProcessTodo _ -> TaskPending
          CancelledTodo _ -> TaskDeleted

  let created = createdToUTC <$> vtCreated vtodo
  let dtstart = dtStartToUTC <$> vtDTStart vtodo
  let due = extractDueDate vtodo
  let modified = lastModifiedToUTC <$> vtLastMod vtodo

  let priority = extractPriority (vtPriority vtodo)

  let (project, tags) = extractCategories vtodo

  let annotations = parseAnnotationsFromDescription vtodo

  let recur = parseRRuleToRecurrence vtodo

  let depends = extractDependencies vtodo

  return
    Task
      { taskUuid = uid,
        taskDescription = summary',
        taskStatus = status,
        taskEntry = created,
        taskStart = dtstart,
        taskDue = due,
        taskModified = modified,
        taskPriority = priority,
        taskProject = project,
        taskTags = tags,
        taskAnnotations = annotations,
        taskRecur = recur,
        taskDepends = depends
      }

dtStampToUTC :: DTStamp -> UTCTime
dtStampToUTC (DTStamp ut _) = ut

dtStartToUTC :: DTStart -> UTCTime
dtStartToUTC (DTStartDateTime dt _) = dateTimeToUTC dt
dtStartToUTC (DTStartDate _ _) = UTCTime (ModifiedJulianDay 0) 0 -- Fallback for Date

lastModifiedToUTC :: LastModified -> UTCTime
lastModifiedToUTC (LastModified ut _) = ut

createdToUTC :: Created -> UTCTime
createdToUTC (Created ut _) = ut

dateTimeToUTC :: DateTime -> UTCTime
dateTimeToUTC dt = case dt of
  FloatingDateTime lt -> localTimeToUTC utc lt
  UTCDateTime ut -> ut
  ZonedDateTime lt tz -> localTimeToUTC utc lt -- Simplified: ignoring actual TZ

extractDueDate :: VTodo -> Maybe UTCTime
extractDueDate vtodo = case vtDueDuration vtodo of
  Nothing -> Nothing
  Just (Left due) -> Just $ dueToUTC due
  Just (Right _) -> Nothing -- Duration not supported

dueToUTC :: Due -> UTCTime
dueToUTC (DueDateTime dt _) = dateTimeToUTC dt
dueToUTC (DueDate _ _) = UTCTime (ModifiedJulianDay 0) 0 -- Fallback for Date

extractPriority :: Priority -> Maybe TaskPriority
extractPriority (Priority n _)
  | n <= 1 = Just PriorityHigh
  | n <= 5 = Just PriorityMedium
  | n <= 9 = Just PriorityLow
  | otherwise = Nothing

extractCategories :: VTodo -> (Maybe Text, [Text])
extractCategories vtodo =
  let catSets = Set.toList $ vtCategories vtodo
      allCats = concatMap (\(Categories vals _ _) -> map TL.toStrict $ Set.toList vals) catSets
   in case allCats of
        [] -> (Nothing, [])
        (p : ts) -> (Just p, ts)

parseAnnotationsFromDescription :: VTodo -> [Annotation]
parseAnnotationsFromDescription vtodo =
  case vtDescription vtodo of
    Nothing -> []
    Just (Description desc _ _ _) -> parseAnnotationLines (TL.toStrict desc)

parseAnnotationLines :: Text -> [Annotation]
parseAnnotationLines text =
  let lines = T.lines text
   in mapMaybe parseAnnotationLine lines

parseAnnotationLine :: Text -> Maybe Annotation
parseAnnotationLine line
  | T.isPrefixOf "[" line && "]" `T.isInfixOf` line = do
      let (tsPart, rest) = T.breakOn "]" line
      let tsText = T.drop 1 tsPart -- Remove '['
      let desc = T.strip (T.drop 1 rest) -- Remove ']' and spaces
      time <- parseISO8601 tsText
      return $ Annotation time desc
  | otherwise = Nothing

parseISO8601 :: Text -> Maybe UTCTime
parseISO8601 t = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" (T.unpack t)

parseRRuleToRecurrence :: VTodo -> Maybe Recurrence
parseRRuleToRecurrence vtodo =
  case Set.toList (vtRRule vtodo) of
    [] -> Nothing
    (RRule recur _ : _) -> rruleToRecurrence recur

rruleToRecurrence :: Recur -> Maybe Recurrence
rruleToRecurrence Recur {..} =
  let freq = case recurFreq of
        Secondly -> Nothing
        Minutely -> Nothing
        Hourly -> Nothing
        Daily -> Just RecurDaily
        Weekly -> Just RecurWeekly
        Monthly -> Just RecurMonthly
        Yearly -> Just RecurYearly
      interval = recurInterval
   in (\f -> Recurrence f interval) <$> freq

extractDependencies :: VTodo -> [Text]
extractDependencies vtodo =
  map (TL.toStrict . relatedToValue) $ Set.toList (vtRelated vtodo)

-------------------------------------------------------------------------------
-- Section 4: vTodo Generation
-------------------------------------------------------------------------------

generateVTodo :: Task -> BS.ByteString
generateVTodo task =
  let vcal =
        VCalendar
          { vcProdId = ProdId (TL.pack "-//twvtodo//EN") (OtherParams Set.empty),
            vcVersion = MaxICalVersion (makeVersion [2, 0]) (OtherParams Set.empty),
            vcScale = Scale (mk (TL.pack "GREGORIAN")) (OtherParams Set.empty),
            vcMethod = Nothing,
            vcOther = Set.empty,
            vcTimeZones = Map.empty,
            vcEvents = Map.empty,
            vcTodos = Map.singleton (TL.fromStrict (taskUuid task), Nothing) (taskToVTodo task),
            vcJournals = Map.empty,
            vcFreeBusys = Map.empty,
            vcOtherComps = Set.empty
          }
      encodingFunctions =
        EncodingFunctions
          { efChar2Bu = BB.charUtf8,
            efChar2Len = const 1
          }
   in BS.toStrict $ printICalendar encodingFunctions vcal

taskToVTodo :: Task -> VTodo
taskToVTodo Task {..} =
  VTodo
    { vtDTStamp = DTStamp (fromMaybe currentTime taskEntry) (OtherParams Set.empty),
      vtUID = UID (TL.fromStrict taskUuid) (OtherParams Set.empty),
      vtClass = Class Public (OtherParams Set.empty),
      vtDTStart = utcToDTStart <$> taskStart,
      vtCompleted = Nothing,
      vtCreated = (\t -> Created t (OtherParams Set.empty)) <$> taskEntry,
      vtDescription =
        if null taskAnnotations
          then Nothing
          else Just $ Description (TL.fromStrict (formatAnnotations taskAnnotations)) Nothing Nothing (OtherParams Set.empty),
      vtGeo = Nothing,
      vtLastMod = (\t -> LastModified t (OtherParams Set.empty)) <$> taskModified,
      vtLocation = Nothing,
      vtOrganizer = Nothing,
      vtPercent = Nothing,
      vtPriority = priorityToValue taskPriority,
      vtRecurId = Nothing,
      vtSeq = Sequence 0 (OtherParams Set.empty),
      vtStatus = Just $ statusToVTodoStatus taskStatus,
      vtSummary = Just $ Summary (TL.fromStrict taskDescription) Nothing Nothing (OtherParams Set.empty),
      vtUrl = Nothing,
      vtDueDuration = Left . utcToDue <$> taskDue,
      vtAttach = Set.empty,
      vtAttendee = Set.empty,
      vtCategories = categoriesToSet taskProject taskTags,
      vtComment = Set.empty,
      vtContact = Set.empty,
      vtExDate = Set.empty,
      vtRStatus = Set.empty,
      vtRelated = dependenciesToRelated taskDepends,
      vtResources = Set.empty,
      vtRDate = Set.empty,
      vtRRule = recurrenceToRRule taskRecur,
      vtAlarms = Set.empty,
      vtOther = Set.empty
    }
  where
    currentTime = UTCTime (ModifiedJulianDay 0) 0

utcToDTStart :: UTCTime -> DTStart
utcToDTStart t = DTStartDateTime (UTCDateTime t) (OtherParams Set.empty)

utcToDue :: UTCTime -> Due
utcToDue t = DueDateTime (UTCDateTime t) (OtherParams Set.empty)

statusToVTodoStatus :: TaskStatus -> TodoStatus
statusToVTodoStatus TaskPending = TodoNeedsAction (OtherParams Set.empty)
statusToVTodoStatus TaskCompleted = CompletedTodo (OtherParams Set.empty)
statusToVTodoStatus TaskDeleted = CancelledTodo (OtherParams Set.empty)

priorityToValue :: Maybe TaskPriority -> Priority
priorityToValue Nothing = Priority 0 (OtherParams Set.empty)
priorityToValue (Just PriorityHigh) = Priority 1 (OtherParams Set.empty)
priorityToValue (Just PriorityMedium) = Priority 5 (OtherParams Set.empty)
priorityToValue (Just PriorityLow) = Priority 9 (OtherParams Set.empty)

categoriesToSet :: Maybe Text -> [Text] -> Set.Set Categories
categoriesToSet project tags =
  let allCats = maybe tags (: tags) project
   in if null allCats
        then Set.empty
        else Set.singleton $ Categories (Set.fromList $ map TL.fromStrict allCats) Nothing (OtherParams Set.empty)

formatAnnotations :: [Annotation] -> Text
formatAnnotations = T.intercalate "\n" . map formatAnnotation

formatAnnotation :: Annotation -> Text
formatAnnotation (Annotation time desc) =
  "[" <> formatISO8601 time <> "] " <> desc

formatISO8601 :: UTCTime -> Text
formatISO8601 = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

recurrenceToRRule :: Maybe Recurrence -> Set.Set RRule
recurrenceToRRule Nothing = Set.empty
recurrenceToRRule (Just (Recurrence freq interval)) =
  Set.singleton $
    RRule
      ( Recur
          { recurFreq = frequencyToICalFreq freq,
            recurUntilCount = Nothing,
            recurInterval = interval,
            recurBySecond = [],
            recurByMinute = [],
            recurByHour = [],
            recurByDay = [],
            recurByMonthDay = [],
            recurByYearDay = [],
            recurByWeekNo = [],
            recurByMonth = [],
            recurBySetPos = [],
            recurWkSt = Text.ICalendar.Types.Monday
          }
      )
      (OtherParams Set.empty)

frequencyToICalFreq :: RecurFrequency -> Frequency
frequencyToICalFreq RecurDaily = Daily
frequencyToICalFreq RecurWeekly = Weekly
frequencyToICalFreq RecurMonthly = Monthly
frequencyToICalFreq RecurYearly = Yearly

dependenciesToRelated :: [Text] -> Set.Set RelatedTo
dependenciesToRelated = Set.fromList . map (\uid -> RelatedTo (TL.fromStrict uid) Parent (OtherParams Set.empty))

-------------------------------------------------------------------------------
-- Section 5: Validation
-------------------------------------------------------------------------------

validateTask :: Task -> Either String ()
validateTask Task {..}
  | T.null taskUuid = Left "Error: Validation: Missing required field 'uuid'\nExpected: Non-empty UUID\nGot: empty string"
  | T.null taskDescription = Left "Error: Validation: Missing required field 'description'\nExpected: Non-empty description\nGot: empty string"
  | otherwise = Right ()

validateSingleTaskJSON :: BL.ByteString -> Either String ()
validateSingleTaskJSON input = do
  val <- eitherDecode input :: Either String Value
  case val of
    Array _ -> Left "Error: Multiple tasks detected. This script processes only a single task."
    Object _ -> Right ()
    _ -> Left "Error: Invalid JSON format. Expected object."

-------------------------------------------------------------------------------
-- Section 6: Format Detection
-------------------------------------------------------------------------------

detectFormat :: BS.ByteString -> Either String Format
detectFormat input =
  let text = TE.decodeUtf8 input
      trimmed = T.strip text
   in if T.isPrefixOf "{" trimmed
        then
          Right JSONFormat
        else
          if "BEGIN:VCALENDAR" `T.isInfixOf` trimmed
            then
              Right ICalFormat
            else
              Left "Error: Unknown format. Expected JSON object or VCALENDAR with VTODO."

-------------------------------------------------------------------------------
-- Section 7: Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- BS.getContents

  -- Detect format
  format <- case detectFormat input of
    Left err -> die err
    Right f -> return f

  -- Check for multiple tasks in JSON
  when (format == JSONFormat) $ do
    case validateSingleTaskJSON (BL.fromStrict input) of
      Left err -> die err
      Right () -> return ()

  -- Parse to internal Task
  task <- case format of
    JSONFormat -> case eitherDecodeStrict input of
      Left err -> die $ "Parse Error: " ++ err
      Right t -> return t
    ICalFormat -> case parseTaskVTodo input of
      Left err -> die err
      Right t -> return t

  -- Validate
  case validateTask task of
    Left err -> exitWith (ExitFailure 2)
    Right () -> return ()

  -- Convert and output
  case format of
    JSONFormat -> BS.putStr (generateVTodo task)
    ICalFormat -> BL.putStr (encode task)
