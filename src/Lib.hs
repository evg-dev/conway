module Lib
    ( game
    ) where

import UI.NCurses

data State
  = Game
  | Init
  | Pause

delay :: Integer
delay = 500 -- milliseconds

getDelay :: Bool -> Maybe Integer
getDelay d
    | d == False = Nothing
    | d == True = Just delay

game :: IO ()
game = runCurses $ do
    (hw, ww) <- screenSize -- (height window,  width window)
    setEcho False
    setCursorMode CursorInvisible -- hide cursor
    w <- defaultWindow
    let с = 0
    loop w Init hw ww с False

-- main game loop
loop :: Window -> State -> Integer -> Integer ->  Integer -> Bool -> Curses ()
loop w Init hw ww generation is_cycle = do
    updateWindow w $ do
        renderBorder hw ww
        moveCursor 0 0
        drawString $ show generation
    render
    waitForNext w Init hw ww generation is_cycle



waitForNext :: Window -> State -> Integer -> Integer ->  Integer -> Bool -> Curses ()
waitForNext w Init hw ww generation is_cycle = do
    let current_delay = getDelay is_cycle
    event <- getEvent w current_delay
    let action      | (event ==  Just (EventCharacter 'q')) = do      -- exit
                        updateWindow w $ do
                            moveCursor 0 0
                            drawString $ show "q"
                        render
                        return ()

                    | (event ==  Just (EventCharacter 'p')) && (is_cycle == True) = do     -- pause
                        updateWindow w $ do
                            moveCursor 0 0
                            drawString $ show "pause"
                        render
                        loop w Init hw ww (generation) False

                    | (event ==  Just (EventCharacter 'p')) && (is_cycle == False) = do     -- resume
                        updateWindow w $ do
                            moveCursor 0 0
                            drawString $ show "resume"
                        render
                        loop w Init hw ww (generation + 1) True

                    | (event ==  Just (EventCharacter 'n')) && (is_cycle == False) = do      -- manual next
                        loop w Init hw ww (generation + 1) False

                    | is_cycle == False = do                                                 -- no cycle, no events - stop
                        loop w Init hw ww (generation) False

                    | otherwise = do                                                         -- timeout
                        loop w Init hw ww (generation + 1) True
    action

renderBorder :: Integer -> Integer -> Update ()
renderBorder  hw ww = do
    drawLineH ( Just glyphStipple) ww
    drawLineV ( Just glyphStipple) hw
    moveCursor (hw - 1)  0
    drawLineH ( Just glyphStipple) ww
    moveCursor 0 (ww - 1)
    drawLineV ( Just glyphStipple) hw
