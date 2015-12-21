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
import Data.List.Split (chunksOf)


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

charSym :: Char -> KeySym
charSym char = stringToKeysym ('U' : showHex (fromEnum char) "")

-- Find unused keys (so that we'd be able to simulate a keypress without
-- having to use any key modifiers). An important thing is that the keys have
-- to be completely unused – keys -that have Alt or Super assigned to them-
-- will behave weirdly if you try to use them (even tho they don't have any
-- symbols in default positions).
findFreeKeys :: Mapping -> IO [KeyCode]
findFreeKeys mapping@Mapping{..} = do
  let isEmptyKey key = do
        -- Get a pointer to the part of symArray that corresponds to the key.
        let keyPtr = advancePtr symArray (symIndex key 0 mapping)
        -- Get the symbols (there's symPerKey of them).
        syms <- peekArray (fromIntegral symPerKey) keyPtr
        -- Check that they all are noSymbol.
        return (all (== noSymbol) syms)
  filterM isEmptyKey (take (fromIntegral keyCount) [minKey..])

sendString :: String -> IO ()
sendString = sendStringWithDelay 10 6

sendStringWithDelay
  :: Int               -- ^ Delay after changing the layout (in ms)
  -> Int               -- ^ Delay after pressing\/releasing a key
  -> String
  -> IO ()
sendStringWithDelay mappingDelay pressDelay string = do
  display <- openDisplay ":0.0"
  mapping <- getKeyboardMapping display Nothing
  freeKeys <- findFreeKeys mapping
  when (null freeKeys) $
    error "sendStringWithDelay: couldn't find a free key"
  let syncAndFlush = sync display False >> flush display
  -- This function assigns given symbols to freeKeys.
  let assignSyms syms = do
        for_ (zip syms freeKeys) $ \(sym, key) ->
          pokeElemOff (symArray mapping) (symIndex key 0 mapping) sym
        changeKeyboardMapping display Nothing mapping
        syncAndFlush
        threadDelay (mappingDelay * 1000)
  -- We break the string into chunks, and for each chunk we first assign all
  -- characters to free keys and then press those keys; it's done like this
  -- because changeKeyboardMapping is pretty slow and doing it for *each* key
  -- results in glitches when typing.
  for_ (chunksOf (length freeKeys) string) $ \chunk -> do
    let len = length chunk
    assignSyms (map charSym chunk)
    for_ (take len freeKeys) $ \key -> do
      xFakeKeyEvent display key True 0
      syncAndFlush
      threadDelay (pressDelay * 1000)
      xFakeKeyEvent display key False 0
      syncAndFlush
      threadDelay (pressDelay * 1000)
  -- Now we have to make the keys free again.
  assignSyms (repeat noSymbol)
  closeDisplay display
