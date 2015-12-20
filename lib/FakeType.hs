{-# LANGUAGE
ForeignFunctionInterface,
NondecreasingIndentation,
RecordWildCards,
NoImplicitPrelude
  #-}


module FakeType
(
  sendString,
  sendStringWithDelay,
)
where


import BasePrelude
import Numeric
import Foreign
import Foreign.C.Types
import Graphics.X11


foreign import ccall unsafe "HsXlib.h XGetKeyboardMapping"
  xGetKeyboardMapping
    :: Display
    -> KeyCode                         -- First keycode
    -> CInt                            -- Amount of keycodes
    -> Ptr CInt                        -- Keysyms per keycode
    -> IO (Ptr KeySym)                 -- Keysyms

foreign import ccall unsafe "HsXlib.h XChangeKeyboardMapping"
  xChangeKeyboardMapping
    :: Display
    -> KeyCode                         -- First keycode
    -> CInt                            -- Keysyms per keycode
    -> Ptr KeySym                      -- Array of keysyms
    -> CInt                            -- Amount of keycodes
    -> IO ()

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
  xFakeKeyEvent :: Display -> KeyCode -> Bool -> CULong -> IO Status

data Mapping = Mapping {
  minKey    :: KeyCode,
  keyCount  :: CInt,
  symArray  :: Ptr KeySym,
  symPerKey :: CInt }

getKeyboardMapping
  :: Display
  -> Maybe (KeyCode, CInt)  -- ^ First key + amount of keys,
                            --   or 'Nothing' if it's “all”
  -> IO Mapping
getKeyboardMapping display mb = do
  let (minKey, keyCount) = case mb of
        Just x  -> x
        Nothing -> let (a, b) = displayKeycodes display
                   in  (fromIntegral a, b-a+1)
  alloca $ \symPerKey_return -> do
  symArray <- xGetKeyboardMapping
                display
                minKey
                keyCount
                symPerKey_return
  symPerKey <- peek symPerKey_return
  return Mapping{..}

symIndex :: KeyCode -> Int -> Mapping -> Int
symIndex key pos Mapping{..} =
  fromIntegral (key-minKey) * fromIntegral symPerKey + pos

changeKeyboardMapping
  :: Display
  -> Maybe (KeyCode, CInt)  -- ^ First key + amount of keys,
                            --   or 'Nothing' if it's “all”
  -> Mapping
  -> IO ()
changeKeyboardMapping display mb mapping@Mapping{..} =
  case mb of
    Nothing ->
      xChangeKeyboardMapping
        display
        minKey
        symPerKey
        symArray
        keyCount
    Just (key, amount) ->
      xChangeKeyboardMapping
        display
        key
        symPerKey
        (advancePtr symArray (symIndex key 0 mapping))
        amount

getSymbol
  :: KeyCode     -- ^ Key
  -> Int         -- ^ Position (usually 0..3)
  -> Mapping
  -> IO KeySym
getSymbol key pos mapping =
  peekElemOff (symArray mapping) (symIndex key pos mapping)

changeSymbol
  :: Display
  -> KeyCode     -- ^ Key
  -> Int         -- ^ Position (usually 0..3)
  -> KeySym      -- ^ Symbol
  -> Mapping
  -> IO ()
changeSymbol display key pos sym mapping = do
  pokeElemOff (symArray mapping) (symIndex key pos mapping) sym
  changeKeyboardMapping display (Just (key, 1)) mapping

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
  r <- p x
  if r then return (Just x) else findM p xs

-- Find a key that has an empty default position (so that we'd be able to
-- simulate a keypress without having to use any key modifiers). If there's
-- no empty key, throw an exception.
findFreeKey :: Mapping -> IO KeyCode
findFreeKey mapping@Mapping{..} = do
  let isEmptyKey key = (== noSymbol) <$> getSymbol key 0 mapping
  mbFreeKey <- findM isEmptyKey (take (fromIntegral keyCount) [minKey..])
  case mbFreeKey of
    Nothing -> error "findFreeKey: couldn't find a free key"
    Just k  -> return k

sendString :: String -> IO ()
sendString = sendStringWithDelay 30

sendStringWithDelay
  :: Int               -- ^ Delay in milliseconds
  -> String
  -> IO ()
sendStringWithDelay delay string = do
  display <- openDisplay ":0.0"
  mapping <- getKeyboardMapping display Nothing
  freeKey <- findFreeKey mapping
  let syncAndFlush = sync display False >> flush display
  forM_ string $ \char -> do
    let sym = stringToKeysym ('U' : showHex (fromEnum char) "")
    changeSymbol display freeKey 0 sym mapping
    syncAndFlush
    --
    xFakeKeyEvent display freeKey True 0
    syncAndFlush
    threadDelay (delay * 500)
    --
    xFakeKeyEvent display freeKey False 0
    syncAndFlush
    threadDelay (delay * 500)
  changeSymbol display freeKey 0 noSymbol mapping
  syncAndFlush
  closeDisplay display
