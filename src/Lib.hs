module Lib
    ( game
    ) where

import UI.NCurses

isLive = '█'
isDead = ' '

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
    let с = 0                -- screenSize return Integer, convert to Int
        grid = cellInitList (fromInteger(hw - 2)::Int, fromInteger(ww - 2)::Int) -- init grid, size exclude border
    loop w grid hw ww с False False


-- main game loop
loop :: Window -> [[Char]] -> Integer -> Integer ->  Integer -> Bool -> Bool -> Curses ()
loop w grid hw ww generation is_cycle is_next = do
    greenAndBlack <- newColorID ColorGreen ColorBlack 1 -- create color

    let nGreed = newGrid is_next grid
    updateWindow w $ do
        setColor greenAndBlack
        renderBorder hw ww
        renderGrid hw ww nGreed
    render

    waitInput w nGreed hw ww generation is_cycle is_next


waitInput :: Window -> [[Char]] -> Integer -> Integer ->  Integer -> Bool -> Bool -> Curses ()
waitInput w grid hw ww generation is_cycle is_next = do
    let current_delay = getDelay is_cycle
    event <- getEvent w current_delay -- set delay for next step or await keypress
    let action | (event ==  Just (EventCharacter 'q')) = do                        -- exit
                    return ()

               | (event ==  Just (EventCharacter 'p')) && (is_cycle == True) = do -- pause
                   loop w grid hw ww (generation) False True

               | (event ==  Just (EventCharacter 'p')) && (is_cycle == False) = do -- resume auto
                   loop w grid hw ww (generation + 1) True True

               | (event ==  Just (EventCharacter 'n')) && (is_cycle == False) = do -- manual next
                   loop w grid hw ww (generation + 1) False True

               | is_cycle == False = do                                            -- no cycle, no events - stop
                   loop w grid hw ww (generation) False False

               | otherwise = do                                                    -- timeout, next step auto
                   loop w grid hw ww (generation + 1) True True
    action


renderBorder :: Integer -> Integer -> Update ()
renderBorder  hw ww = do
    drawLineH ( Just glyphStipple) ww
    drawLineV ( Just glyphStipple) hw
    moveCursor (hw - 1)  0
    drawLineH ( Just glyphStipple) ww
    moveCursor 0 (ww - 1)
    drawLineV ( Just glyphStipple) hw


renderGrid :: Integer -> Integer -> [[Char]] ->  Update ()
renderGrid hw ww g = do
    mapM_ (\n -> renderRow n) g


renderRow :: String -> Update ()
renderRow s = do
    (r, c) <- cursorPosition
            -- row + 1, from 1 column
    moveCursor (r + 1) 1
    drawString s

-- render next step or not
newGrid ::  Bool -> [[Char]] -> [[Char]]
newGrid True g = cellList g
newGrid False g = g


countNeighbours :: Int -> Int -> [[Char]] -> Int
countNeighbours  ix iy g =
    let n = [ g!!y!!x | x <- [ix-1, ix, ix+1],
                                y <- [iy-1, iy, iy+1],
                                x < (length $ g!!0),
                                x >= 0,
                                y < length g,
                                y >= 0,
                                let z = iy == y && ix == x, -- exclude same cell
                                z == False ]
    in length $ filter(==isLive) n


-- init glider
cellInitList :: (Int, Int) -> [[Char]]
cellInitList (x, y) = [[ if (h <= 7 && w <= 7) && (h >= 4 && w >= 4) then isLive else isDead | h <- [1..y]] | w <- [1..x]]


cellList :: [[Char]] -> [[Char]]
cellList g = map (\(w,x) -> map (\(h,y) -> isLiveCell w h g) $ zip [0..] x)  $ zip [0..] g


isLiveCell :: Int -> Int -> [[Char]] -> Char
isLiveCell x y g
    | isCurrentLive == isDead = if n == 3 then isLive else isDead
    | isCurrentLive == isLive = if n == 2 || n == 3 then isLive else isDead
    | otherwise = isDead
    where
        n = countNeighbours x y g
        isCurrentLive = g!!x!!y