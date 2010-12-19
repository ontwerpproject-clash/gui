
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
--import Graphics.Rendering.OpenGL.GL


a = MenuEntry "test" close
m = Menu [a]
lb = MouseButton LeftButton


keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess  --press "esc" to quit
keyboard (Char '+') Down _ _ = print "zoom in"
keyboard (Char '-') Down _ _ = print "zoom out"
keyboard lb Down _ (Position x y) = mouseStuff x y
keyboard (MouseButton WheelUp) _ _ _ = exitWith ExitSuccess --mousewheel does not work
keyboard _ _ _ _ = return ()

--few test checks to do things according to the position of the click
--topleft of a window has the position 0/0 for mouseclicks
--pointerX == x pixel position of the mouseclick
--pointerY == y pixel position of the mouseclick
--windowXSize == the horizontal size of the window in pixels
--windowYSize == the vertical size of the window in pixels
--click in topleft closes the program, most other clicks produce squares and clicks on the right hand side of the window produce lines with origin close to the top right
mouseCheck pointerX pointerY windowXSize windowYSize 
	| pointerX<50 && pointerY < 50 = exitWith ExitSuccess
	| test pointerX = paintSquare (fst (mouseXYPos pointerX pointerY windowXSize windowYSize)) (snd (mouseXYPos pointerX pointerY windowXSize windowYSize))
	| otherwise = do
		color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
		lineWidth $= 1.0
		let xyPos = (mouseXYPos pointerX pointerY windowXSize windowYSize)
		renderPrimitive LineStrip $ mapM_ vertex [ Vertex3 (0.9 :: GLfloat) (0.75) 0.0, Vertex3 (fst xyPos) (snd xyPos) 0.0]
		flush
	
	
test x = if ( x < 300 ) then True else False
	
	

mouseStuff x y = do
	(position, size) <- get viewport
	mouseCheck x y (xSize size) (ySize size)
	

--might have to cut on precision or to account for some error margin if trying to match a position of an element and the click position
mouseXYPos xPixel yPixel xWindowSize yWindowSize= do
		--0/0
		let xCenter = intToFloat xWindowSize / 2 
		let yCenter = intToFloat yWindowSize / 2
		--difference in relation to 0/0
		let relX = intToFloat xPixel - xCenter
		let relY = (intToFloat yPixel - yCenter) * (-1.0) 
		let xPos = relX / xCenter
		let yPos = relY / yCenter

		(xPos, yPos)
		

intToFloat n = fromInteger (toInteger n)	

--returns the number of pixels in x direction
xSize si= xSize_ si
xSize_ p=x
	where 
		(Size x y)=p
--returns the number of pixels in y direction		
ySize si= ySize_ si
ySize_ p=y
	where 
		(Size x y)=p
		

makePoints :: GLfloat->GLfloat->[Vertex3 GLfloat]
makePoints x y = [
  Vertex3 (-x)   y  0.0,
  Vertex3   x    y  0.0,
  Vertex3   x  (-y) 0.0,
  Vertex3 (-x) (-y) 0.0]
 
 
paintSquare x y = do
	clear [ ColorBuffer ]
	color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
	lineWidth $= 1.0
	let p = (makePoints x y)
	print p
	renderPrimitive Polygon $	mapM_ vertex (makePoints x y)
	flush


  
display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   flush

   
   
reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   matrixMode $= Modelview 0

--gives warning   
changeMen= do
	attachMenu RightButton m
close = exitWith ExitSuccess



	
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 400 150
   initialWindowPosition $= Position 100 100
   createWindow progName
   -- attach a menu to the right mousebutton
   attachMenu RightButton (Menu [MenuEntry "test!" changeMen,SubMenu "Sub" (Menu [MenuEntry "testSub" close])])
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= Just keyboard
   mainLoop

