module Lib
    ( game
    ) where


import UI.NCurses

data State
  = Game
  | Init
  | Pause

game :: IO ()
game = runCurses $ do
    (hw, ww) <- screenSize -- (height window,  width window)
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w Init hw ww


loop :: Window -> State -> Integer -> Integer -> Curses ()
loop w Init hw ww = do
    updateWindow w $ do
        renderBorder hw ww
    render
    waitForExit w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')


renderBorder :: Integer -> Integer -> Update ()
renderBorder  hw ww = do
    drawLineH ( Just glyphStipple) ww
    drawLineV ( Just glyphStipple) hw
    moveCursor (hw - 1)  0
    drawLineH ( Just glyphStipple) ww
    moveCursor 0 (ww - 1)
    drawLineV ( Just glyphStipple) hw


waitForExit :: Window -> (Event -> Bool) -> Curses ()
waitForExit w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop
