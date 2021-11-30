import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
--
-- Enkele definities voor de types van het bord voor te stellen
--
type Row   = [Int]
type Col   = [Int]
type Board = [[Int]]

width, height, empty :: Int
width  = 4
height = 4
empty  = 0

--
-- Genereert een leeg bord
--
emptyBoard :: Board
emptyBoard = replicate height $ replicate width empty 

--
-- Transponeert een matrix
--
transpose :: Board -> Board
transpose ([]:_) = []
transpose b = map head b : transpose (map tail b)

--
-- Cruncht een enkele rij zodat alles naar links verschoven wordt
--
crunch :: Row -> Row
crunch r = subcrunch $ filter (/=0) r ++ filter (==0) r
            where subcrunch (y:z:ys) | y == z    = y*2 : subcrunch ys ++ [0]
                                     | otherwise = y : subcrunch (z:ys)
                  subcrunch x                    = x

--
-- Varianten van crunch voor alle richtingen
--
crunchLeft :: Board -> Board
crunchLeft = map crunch

crunchRight :: Board -> Board
crunchRight = map (reverse . crunch . reverse)

crunchUp :: Board -> Board
crunchUp = transpose . crunchLeft . transpose

crunchDown :: Board -> Board
crunchDown = transpose . crunchRight . transpose

--
-- Maakt een lijst van indexen die aangeven welke plaatsen
-- van de matrix leeg zijn.
--
emptySpots :: Board -> [(Int,Int)]
emptySpots b = [(x,y) | x <- [0..height-1], y <- [0..width-1], (b!!x)!!y == empty]

--
-- Vervangt de waarde van een rij op een gegeven index i door een gegeven waarde
--
replaceRow :: a -> Int -> [a] -> [a]
replaceRow n i l = take i l ++ [n] ++ drop (i+1) l

--
-- Vervangt de waarde van een bord op een gegeven index (i,j) door een gegeven waarde
--
replace :: (Int,Int) -> Board -> Int -> Board
replace (i,j) b n = replaceRow (replaceRow n j (b!!i)) i b 

--
-- Berekent de score voor een bord
--
score :: Board -> Int
score b = sum [subscore ((b!!x)!!y) | x <- [0..height-1], y <- [0..width-1]]
            where subscore 0 = 0
                  subscore 2 = 0
                  subscore x = x + 2 * subscore (div x 2)

--
-- GUI interface
--

-- Telt hoeveel cijfers een getal bevat
count :: Int -> Int
count = length . show

--
-- Maakt een figuur voor een enkele tile op index x y met waarde v
--
renderTile :: Int -> Int -> Int -> Picture
renderTile i j v = Translate x y $ Pictures [square,number]
             where square = rectangleWire 50 50
                   number = Translate (-12) (-14) $ Scale s 0.3 $ Text $ show v
                   s      = (/) 0.3 $ fromIntegral $ count v 
                   x      = fromIntegral $ 50*j-75
                   y      = fromIntegral $ 75-50*i

--
-- Maakt een figuur voor het hele bord
--
render :: Board -> Picture
render b = Pictures $ scoreTile : [renderTile x y $ (b!!x)!!y | x <- [0..height-1], y <- [0..width-1]]
                  where scoreTile = Translate ((-12)*fromIntegral (count $ score b)) 105 $ Scale 0.3 0.3 $ Text $ show $ score b
--
-- Plaatst een gegeven (random) getal op een lege plaats van het bord
--
setRandom :: Int -> Board -> Board
setRandom r b = replace (e !! index) b r
               where e         = emptySpots b
                     (index,_) = randomR (0,length e - 1) (mkStdGen 4) -- geen echte random index, steeds dezelfde voor gelijke lengte van e

--
-- Linkt de specialKey aan de juiste crunch
--
handleCrunch :: SpecialKey -> Board -> Board
handleCrunch k = case k of        
                  KeyUp    -> crunchUp
                  KeyDown  -> crunchDown
                  KeyLeft  -> crunchLeft
                  KeyRight -> crunchRight

--
-- Verwerkt het indrukken van een toets voor een bord en een nieuw (random) getal
-- Maakt geen wijzigingen in het bord als er na het indrukken van de toets geen lege plaatsen meer zijn 
--
handleInput :: SpecialKey -> Board -> Int -> Board
handleInput k b r = if fullboard then b else setRandom r newboard
                      where newboard  = handleCrunch k b
                            fullboard = null $ emptySpots newboard 

--
-- Maakt het scherm voor het spel
--
window :: Display
window = InWindow "2048" (200,300) (200,200)

--
-- playPureRandom
--
playPureRandom :: Display -> Color -> Int -> world ->
                 (world -> Picture) -> (SpecialKey -> world -> Int -> world) -> IO ()
playPureRandom window color fps world dw he =
   playIO window color fps world dfIO heIO tsIO
     where dfIO x   = return $ dw x
           heIO (EventKey (SpecialKey s) Down _ _) b =
             do index <- randomRIO(0,9) :: IO Int
                let num = (4 : replicate 9 2) !! index -- Het nieuwe cijfer is een 2 met 90% kans en een 4 met 10% kans
                return $ he s b num
           heIO _ b = return b
           tsIO t w = return w

--
-- Main function
--
main :: IO ()
main = playPureRandom window white 3 (setRandom 2 emptyBoard) render handleInput -- Start het bord met een 2
