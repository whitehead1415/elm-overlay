import Graphics.Element (..)
import Text 
import Color 
import Signal (..)
import Window
import Graphics.Input
import Easing as E
import Time (Time, second, fps, every, timestamp)
    
    
type alias From = Float
type alias To = Float
    
type Tray = ShowTray 
          | HideTray
          | StickTray To

trayChan : Channel Tray
trayChan = channel HideTray

menuContainer : (Int,Int) -> Element
menuContainer (w,h) = container w h middle empty

topBar : Int -> Element
topBar w = color (Color.rgb 150 150 150)
           <| container w 40 midLeft
           <| flow left [ spacer 10 1
                        , Graphics.Input.button (send trayChan ShowTray) "&#9776;"
                        , spacer 10 1
                        ]
           
trayElement : (Int,Int) -> Element
trayElement (w,h) = color (Color.rgb 50 50 50) <| container 300 h middle <| Text.plainText "foo"
           
tray : (Int, Int) -> Float -> ((Int,Int) -> Element) -> Position -> Element
tray (w, h) trayPos e position = layers [ color (Color.rgba 10 10 10 1.0) <| container w h middle empty 
                                              , container w h (topLeftAt (relative trayPos) (absolute 0)) <| container w h position (e (w, h))
                                             ]

menuWidget : (Int,Int) -> Float -> Element
menuWidget (w,h) trayPos =
    layers [ flow down [ topBar w
                       , menuContainer (w,h-40)
                       ]
           , tray (w, h) trayPos trayElement midLeft
           ]
    
trayAnimation : (Time, Tray) -> Time -> Float -> Float
trayAnimation (t, tray) b old = 
    let ease = E.ease E.easeOutQuad
        speed = (0.3 * second)
    in case tray of
         ShowTray  -> ease E.float old 0.0 speed (b - t)
         HideTray -> ease E.float old -1.0 speed (b - t)
         StickTray pos -> pos
                          
trayPos = foldp (<|) -1.0 (trayAnimation <~ (timestamp (subscribe trayChan)) ~ (every 16))
    
main : Signal Element
main = menuWidget <~ Window.dimensions ~ trayPos