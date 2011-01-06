--------------------------------------------------------------------------------------------------------------
--General module for drawing basic elements of the specified data structure. Note on defined variables:     --
--due to GL representation of variables, xyScale has to be set manually to the same value as xS/yS, since   --
--GLsizei is not automatically converted to GLfloat when needed.                                            --
--------------------------------------------------------------------------------------------------------------
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
-- import ArchElements as AE
import LayoutManager as LM
import WireFuncs
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import LongestPath

import ParseClash as AE

sc = (1/3)


main :: IO ()
main = do
    getArgsAndInitialize
    createWindow "CLASH Visualisation Tool, pre-alpha"
    let
        xS = 800
        yS = 800
        xyScale = (800/800)
    windowSize $= (Size xS yS)
    scale sc (sc*xyScale) (1::GLfloat)
    displayCallback $= display
    --keyboardMouseCallback $= (Just ( keyboard1 ))
    mainLoop
 
type Coord = (GLfloat, GLfloat)

----------------------------------------------------------------------------------------------------------------------------
-- Main Display function. Does the following: Clear the screen, set the color to white, calculate the grid, set the scale --
-- of the grid to proper levels, draw the elements and commit to the screen (flush).                                      --
----------------------------------------------------------------------------------------------------------------------------
display :: IO ()
display = do
    func <- parseClashFile "Plus1.hs"
    clear [ColorBuffer]
    color $ Color3 1 1 (1::GLfloat)
    putStrLn (show $ simplifyWires $ calcRoutes (extractWires (offsetElements func)))
    putStrLn $ show $ resolveCollisions $ simplifyWires $ calcRoutes $ extractWires $ offsetElements func
    drawElems [offsetElements func]
    color $ Color3 0 0 (1::GLfloat)
    drawWires $ makeArrows $ resolveCollisions $ simplifyWires $ calcRoutes $ extractWires $ offsetElements func
    color $ Color3 0 1 (0::GLfloat)
    renderPrimitive Points $ makeVertexes points
    keyboardMouseCallback $= (Just ( keyboard func))
    flush
        where
            points = [(x,y,0) | x <-[-10..10] , y<-[-10..10]]
            -- circuitOffset = offsetElements func




keyboard func (Char '\27') Down _ _ = exitWith ExitSuccess  --press "esc" to quit
keyboard func (Char '+') Down _ _   = print "zoom in"
keyboard func (Char '-') Down _ _   = print "zoom out"
keyboard func (Char 'r') Down _ _   = do
    clear [ColorBuffer]
    color $ Color3 1 1 (1::GLfloat)
    drawElems [offsetElements func]
    color $ Color3 0 0 (1::GLfloat)
    drawWires $ makeArrows $ resolveCollisions $ simplifyWires $ calcRoutes $ extractWires $ offsetElements func
    --color $ Color3 0 1 (0::GLfloat)
    renderPrimitive Points $ makeVertexes points
    print (resolveCollisions $ simplifyWires $ calcRoutes $ extractWires $ offsetElements func)
    flush
        where
            points = [(x,y,0) | x <-[-10..10] , y<-[-10..10]]


keyboard func (Char 'l') Down _ _   = do 
	print "longestPath"
	--longestPath
	let path = (makeWiresH $ lp1 func)
	--allWires
	let wires = extractWires $ offsetElements func
	--conversion step, because the result of longestpath, is a path based on the elementNames instead of the portIds (has to be changed)
	--result is the longesPath with portIds instead of ElementIds
	let wiresToPaint = removeWires wires path (toList2 func)
	--wires of the longestPath to their position in the wire list
	let wiresOfPath = (pathToPositions wires wiresToPaint)
	--calculateWires
	let completeWires = makeArrows $ resolveCollisions $ simplifyWires $ calcRoutes $ wires
	--paint the wires (the wires on the positions in the list wiresOfPath are drawn in red)
	drawHelper completeWires wiresOfPath
	
keyboard func (Char 'p') Down _ _   = do 	
	--longestPathBackupVersion
	let path = (makeWiresH $ lp1 func)
	--allWires
	let wires = extractWires $ offsetElements func

	let wiresToPaint = removeWires wires path (toList2 func)
	let wiresOfPath = (pathToPositions wires wiresToPaint)
	color $ Color3 1 0 (0::GLfloat)
	--draws all the wires of the longest Path in red (but just those, so it is probably not on top of all the others old wires)
	drawWires $ makeArrows $ resolveCollisions $ simplifyWires $ calcRoutes $ wiresToPaint
	
keyboard func _ _ _ _               = return ()	
	



	
	
	
	


keyboard1 (Char '\27') Down _ _ = exitWith ExitSuccess  --press "esc" to quit
keyboard1 (Char '+') Down _ _   = print "zoom in"
keyboard1 (Char '-') Down _ _   = print "zoom out"
keyboard1 _ _ _ _               = return ()

---------------------------------------------------------------------------------------------------
-- Functions directing the atomic draw operations given a list of elements with absolute offsets --
---------------------------------------------------------------------------------------------------

drawElems :: [ArchElem Offset] -> IO ()
drawElems elems 
    | elems == [] = return ()
    | otherwise   = do drawElem (head elems) 
                       drawElems (tail elems) 

drawElem :: ArchElem Offset -> IO ()
drawElem (Function _ _ _ _ (innerElems , _ ) (x,y)) = do drawFunction innerElems (toOffset (x,y)) 
                                                         drawElems innerElems
drawElem (Operator _ name _ _ (x,y)) = drawOperator name (toOffset (x,y))
drawElem (Literal _ name _ (x,y))    = drawLiteral name (toOffset (x,y))
drawElem (Mux _ _ _ _ (x,y))         = drawMux (toOffset (x,y))
drawElem (Register _ _ _ (x,y))      = drawRegister (toOffset (x,y))

-----------------------------------------------------------------------------------------------------
-- Drawing Functions for all atomic elements (Function, Operator, Literal, Mux, Register and wires --
-----------------------------------------------------------------------------------------------------
--Needs: A unique identifier, possibly a label, a list of in ports, an out port, a list of elements (such as operators), a list of wires, a variable data entity (possibly empty) and Coordinates.
drawFunction :: [ArchElem Offset] -> Coord -> IO ()
drawFunction innerElems (x,y) = do color $ Color3 1 1 (1::GLfloat)
                                   rect (Vertex2 (realToFrac x-0.3) ((realToFrac y+0.3)::GLfloat)) 
                                        (Vertex2 (x+x_+0.3) (y-y_-0.3))
                                   where
                                     (xl,yl) = calcFuncSize innerElems (0,0)
                                     x_      = (realToFrac xl)::GLfloat
                                     y_      = (realToFrac yl)::GLfloat

drawOperator :: AE.Name -> Coord -> IO ()
drawOperator name (x,y) = do   color $ Color3 1 0 (0::GLfloat)
                               translate $ Vector3 (x+0.5) (y-0.5) (0::GLfloat)
                               fillCircle 0.3
                               color $ Color3 0 0 (0::GLfloat)
                               renderCircle 0.3
                               loadIdentity
                               let
                                  fact = realToFrac (length name)
                                  sca = (0.006*sc)/fact
                               color $ Color3 0 0 (0::GLfloat)
                               scale sca sca (1::GLfloat)
                               translate (Vector3  ((x/sca*sc)+(0.2/sca*sc)+(0.03*fact/sca*sc)) 
                                                   ((y/sca*sc)-(0.85/sca*sc)+(0.03*fact/sca*sc)) 
                                                   (0::GLfloat))
                               renderString Roman name
                               loadIdentity
                               scale sc sc (1::GLfloat)
                                            
drawLiteral :: AE.Name -> Coord -> IO ()
drawLiteral name (x,y) = do color $ Color3 0 1 (0::GLfloat)
                            rect (Vertex2 (x+0.4) (y-0.2)) (Vertex2 (x+0.6) ((y-0.8)::GLfloat))
                            loadIdentity
                            let
                                fact = realToFrac (length name)
                                sca = (0.002*sc)/fact
                            color $ Color3 0 0 (0::GLfloat)
                            scale sca sca (1::GLfloat)
                            translate (Vector3  ((x/sca*sc)+(0.45/sca*sc)+(0.01*fact/sca*sc)) 
                                                 ((y/sca*sc)-(0.75/sca*sc)+(0.05*fact/sca*sc)) 
                                                 (0::GLfloat))
                            renderString Roman name
                            loadIdentity
                            scale sc sc (1::GLfloat)

drawMux :: Coord -> IO ()
drawMux (x,y) = do color $ Color3 1 0 (0::GLfloat)
                   rect (Vertex2 (x+0.2) (y-0.2)) (Vertex2 (x+0.8) ((y-0.8)::GLfloat))
                   color $ Color3 0 0 (0::GLfloat)
                   displayPoints [(x+0.1, y-0.5, 0), (x+0.5, y-0.5,0)] LineStrip

drawRegister :: Coord -> IO ()
drawRegister (x,y) = do  color $ Color3 0 1 (1::GLfloat)
                         rect (Vertex2 (x+0.3) (y-0.2)) (Vertex2 (x+0.7) ((y-0.8)::GLfloat))
                         color $ Color3 0 0 (0::GLfloat)
                         displayPoints  [ (x+0.3, y-0.7, 0),
                                          (x+0.4, y-0.6, 0),
                                          (x+0.3, y-0.5, 0)
                                        ]
                                        LineStrip

------------------------------------------------------------------------------------------------------
-- Support function to calculate the total number of elements to be rendered- this function is used --
-- to make sure all functions are neatly rendered.                                                  --
------------------------------------------------------------------------------------------------------
--calculateFunctionSize :: [ArchElem a] -> ((Int,Int) , (Int,Int))
--calculateFunctionSize innerElems
--    | innerElems == [] = ((0,0), (0,0))
--    | otherwise        = cFS innerElems ((0,0), (0,0))
--    
--cFS :: [ArchElem (Int,Int)] -> ((Int, Int),(Int, Int)) -> ((Int, Int),(Int,Int))
--
--cFS [] a = a
--
--cFS innerElems@((Function _ _ _ _ (innerElems2 , _) _):innerElems') ((xl, yt),(xr,yb)) = 
--   cFS innerElems' (((min xl2 xl),(max yt2 yt)),
--                     ((max xr2 xr),(min yb2 yb)))
--    where 
--      ((xl2,yt2),(xr2,yb2)) = (calculateFunctionSize innerElems2)
--    
--cFS ((ArchElem (x,y)):innerElems) ((xl,yt),(xr,yb)) = 
--      cFS innerElems (((min x xl),(max y yt)),
--                      ((max x xr),(min y yb)))
--
--cFS _ a = a

-----------------------------------------------------
-- Support functions for drawing a circle in GLUT. --
-----------------------------------------------------

--displayPoints :: [(GLfloat,GLfloat,GLfloat)] -> PrimitiveMode -> IO ()
--displayPoints points primitiveShape = do
--  renderAs primitiveShape points
--  flush

--renderAs :: PrimitiveMode -> [(GLfloat,GLfloat,GLfloat)] -> IO ()
--renderAs figure ps = renderPrimitive figure $ makeVertexes ps

circlePoints :: Floating a => a -> Int -> [(a,a,a)]
circlePoints radius number =
  [let alpha = 2 * pi * fromIntegral i / fromIntegral number
   in (radius * sin alpha, radius * cos alpha, 0)
  | i <- [1, 2 .. number]]

circle :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
circle radius = circlePoints radius 100

fillCircle :: GLfloat -> IO ()
fillCircle r = displayPoints (circle r) Polygon

renderCircle :: GLfloat -> IO ()
renderCircle r = displayPoints (circle r) LineLoop

--makeVertexes :: [(GLfloat, GLfloat,GLfloat)] -> IO ()
--makeVertexes = mapM_ (\(x,y,z)->vertex $ Vertex3 x y z)


----------------
-- Support function for the layout manager to convert to offset instead of coordinates. the first set of coordinates is the top left of
-- the function in which one looks, the second set (in ints) is the x and y offset.
---------------

toOffset :: (Int, Int) -> (GLfloat,GLfloat)
toOffset (xOffset, yOffset) = ((realToFrac xOffset), -(realToFrac yOffset))
