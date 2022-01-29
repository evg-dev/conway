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
        grid = cellInitList (toInt(hw - 2), toInt(ww - 2)) -- init grid, size exclude border
        c_x = 1 -- cursor x
        c_y = 1 -- cursor y
    loop w grid hw ww с False False c_x c_y


-- main game loop
loop :: Window -> [[Char]] -> Integer -> Integer ->  Integer -> Bool -> Bool -> Integer ->  Integer -> Curses ()
loop w grid hw ww generation is_cycle is_next c_x c_y = do
    greenAndBlack <- newColorID ColorGreen ColorBlack 1 -- create color

    let nGreed = newGrid is_next grid
    updateWindow w $ do
        setColor greenAndBlack
        renderBorder hw ww
        renderGrid hw ww nGreed
        renderMenu hw ww generation
        renderCursor c_x c_y generation is_cycle nGreed
    render

    waitInput w nGreed hw ww generation is_cycle is_next c_x c_y


waitInput :: Window -> [[Char]] -> Integer -> Integer ->  Integer -> Bool -> Bool -> Integer -> Integer -> Curses ()
waitInput w grid hw ww generation is_cycle is_next c_x c_y = do
    let current_delay = getDelay is_cycle
    event <- getEvent w current_delay -- set delay for next step or await keypress
    let action | (event ==  Just (EventCharacter 'q')) = do                        -- exit
                    return ()

               | (event ==  Just (EventCharacter 'p')) && (is_cycle == True) = do -- pause
                   loop w grid hw ww (generation) False True c_x c_y

               | (event ==  Just (EventCharacter 'p')) && (is_cycle == False) = do -- resume auto
                   loop w grid hw ww (generation + 1) True True c_x c_y

               | (event ==  Just (EventCharacter ' ')) && (is_cycle == False) = do -- manual next
                   loop w grid hw ww (generation + 1) False True c_x c_y

               | (event ==  Just (EventSpecialKey KeyLeftArrow)) && (is_cycle == False) = do -- Left
                   let nc_x = getRangeCursorCoord ww (c_x - 1)
                   loop w grid hw ww (generation) False False nc_x c_y

               | (event ==  Just (EventSpecialKey KeyRightArrow)) && (is_cycle == False) = do -- Right
                let nc_x = getRangeCursorCoord ww (c_x + 1)
                loop w grid hw ww (generation) False False nc_x c_y

               | (event ==  Just (EventSpecialKey KeyDownArrow)) && (is_cycle == False) = do -- Down
                   let nc_y = getRangeCursorCoord hw (c_y + 1)
                   loop w grid hw ww (generation) False False c_x nc_y

               | (event ==  Just (EventSpecialKey KeyUpArrow)) && (is_cycle == False) = do -- Up
                   let nc_y = getRangeCursorCoord hw (c_y - 1)
                   loop w grid hw ww (generation) False False c_x nc_y

               | (event ==  Just (EventCharacter '\n')) && (is_cycle == False) = do -- Enter
                   let replaced_grid = replaceCursorCell c_x c_y grid              -- set cell state
                   loop w replaced_grid hw ww (generation) False False c_x c_y

               | is_cycle == False = do                                            -- no cycle, no events - stop
                   loop w grid hw ww (generation) False False c_x c_y

               | otherwise = do                                                    -- timeout, next step auto
                   loop w grid hw ww (generation + 1) True True c_x c_y
    action


renderMenu :: Integer -> Integer -> Integer -> Update ()
renderMenu  hw ww g
    | (g == 0) = do
        moveCursor 4   10
        drawString "#########################################"
        moveCursor 5   10
        drawString "# p - Pause/Resume                      #"
        moveCursor 6   10
        drawString "# q - Exit                              #"
        moveCursor 7   10
        drawString "# SPACE - Manual next step on pause     #"
        moveCursor 8   10
        drawString "# ↑ - UP, ← - LEFT, → - RIGHT, ↓ - DOWN #"
        moveCursor 9   10
        drawString "# ENTER - change cell state             #"
        moveCursor 10   10
        drawString "#########################################"
    | otherwise = do moveCursor 0 0


renderCursor :: Integer -> Integer -> Integer -> Bool -> [[Char]] -> Update ()
renderCursor c_x c_y generation is_cycle g
    | (generation > 0) && (is_cycle == False) = do
        moveCursor c_y c_x
        let s = if g!!(toInt c_y - 1)!!(toInt c_x - 1) == isLive then "X" else "@"
        drawString s
    | otherwise = return ()


toInt :: Integer -> Int
toInt x = fromInteger(x)::Int

getRangeCursorCoord :: Integer -> Integer -> Integer
getRangeCursorCoord mc c
    | c < 1 = 1
    | c > (mc - 2) = (mc - 2)
    | otherwise = c


replace p f xs = [ if i == p then f x else x | (x, i) <- zip xs [0..] ]
replace2D v (x,y) = replace y (replace x (const v))


replaceCursorCell :: Integer -> Integer -> [[Char]] -> [[Char]]
replaceCursorCell c_x c_y g =
    let c = if g!!(toInt c_y - 1)!!(toInt c_x - 1) == isLive then isDead else isLive
    in replace2D c (c_x - 1, c_y - 1) g


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
    let n = [ g!!x!!y | x <- [ix-1, ix, ix+1],
                x < length g,
                x >= 0,
                y <- [iy-1, iy, iy+1],
                y < (length $ g!!0),
                y >= 0,
                let z = iy == y && ix == x, -- exclude same cell
                z == False ]
    in length $ filter(==isLive) n


-- init example square 2 * 2
cellInitList :: (Int, Int) -> [[Char]]
cellInitList (x, y) = [[ if (h <= 7 && w <= 2) && (h >= 6 && w >= 1) then isLive else isDead | h <- [1..y]] | w <- [1..x]]


cellList :: [[Char]] -> [[Char]]
cellList g = map (\(w,x) -> map (\(h,y) -> isLiveCell w h g) $ zip [0..] x)  $ zip [0..] g


isLiveCell :: Int -> Int -> [[Char]] -> Char
isLiveCell x y g -- = countNeighbours $ x y g
    | isCurrentLive == isDead = if n == 3 then isLive else isDead
    | isCurrentLive == isLive = if n == 2 || n == 3 then isLive else isDead
    | otherwise = isDead
    where
        n = countNeighbours x y g
        isCurrentLive = g!!x!!y