{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.Exit
  
import Data.Int

import Control.Exception
import Control.Monad

import Foreign (peekByteOff)
import Foreign.Storable
import Foreign.Ptr

import Graphics.Win32
import System.Win32.DLL (getModuleHandle)

import Graphics.D3D11Binding

foreign import stdcall "PostQuitMessage" postQuitMessage :: Int32 -> IO ()

main :: IO ()
main = do
  hWnd <- createDefaultWindow 800 600 wndProc
  useDevice hWnd $ \swapChain device deviceContext renderTargetView -> do
    vb <- compileShaderFromFile "Triangle.fx" "VS" "vs_4_0"
    (il, vs) <- use vb $ \vsBlob -> do
      pointer <- getBufferPointer vsBlob
      size <- getBufferSize vsBlob
      Right vertexShader <- createVertexShader 
                              device
                              pointer
                              size
                              nullPtr
      
      Right inputLayout <- createInputLayout
                              device
                              [D3D11InputElementDesc 
                                  "POSITION" 
                                  (fromIntegral 0)
                                  DxgiFormatR32G32B32Float
                                  (fromIntegral 0)
                                  (fromIntegral 0)
                                  D3D11InputPerVertexData
                                  (fromIntegral 0)]
                              pointer
                              size
      iaSetInputLayout deviceContext inputLayout
      return (inputLayout, vertexShader)
    
    pb <- compileShaderFromFile "Triangle.fx" "PS" "ps_4_0"
    ps <- use pb $ \psBlob -> do
      pointer <- getBufferPointer psBlob
      size <- getBufferSize psBlob
      Right pixelShader <- createPixelShader
                            device
                            pointer
                            size
                            nullPtr
      
      let bd = D3D11BufferDesc
                { byteWidth = fromIntegral $ 3 * sizeOf (undefined :: Vertex3)
                , usage = D3D11UsageDefault
                , bindFlags = d3d11BindFlags [D3D11BindVertexBuffer]
                , cpuAccessFlags = 0
                , miscFlags = 0
                , structureByteStride = 0 }
      Right buffer <- createBuffer
                        device
                        bd
                        [ Vertex3 0.0 0.5 0.5
                        , Vertex3 0.5 (-0.5) 0.5
                        , Vertex3 (-0.5) (-0.5) 0.5 ]
      iaSetVertexBuffers deviceContext 0 [(buffer, fromIntegral $ sizeOf (undefined :: Vertex3), 0)]
      iaSetPrimitiveTopology deviceContext D3D11PrimitiveTopologyTrianglelist
      return pixelShader
    
    use il $ \inputLayout -> use vs $ \vertexShader -> use ps $ \pixelShader -> do
      messagePump hWnd deviceContext swapChain renderTargetView vertexShader pixelShader

useDevice hWnd proc = do
  (s, d, dc, r) <- initDevice hWnd
  use s $ \swapChain -> use d $ \device -> use dc $ \deviceContext -> use r $ \renderTargetView ->
    proc swapChain device deviceContext renderTargetView
  
wndProc :: WindowClosure
wndProc hWnd msg wParam lParam
  | msg == wM_DESTROY = postQuitMessage 0 >> return 0
  | otherwise = defWindowProc (Just hWnd) msg wParam lParam

initDevice :: HWND -> IO (Ptr IDxgiSwapChain, Ptr ID3D11Device, Ptr ID3D11DeviceContext, Ptr ID3D11RenderTargetView)
initDevice hWnd = do
  let bd = DxgiModeDesc 
            800 
            600 
            (DxgiRational 60 1) 
            DxgiFormatR8G8B8A8Unorm 
            DxgiModeScanlineOrderUnspecified 
            DxgiModeScalingUnspecified
  
  let sd = DxgiSwapChainDesc
            bd
            (DxgiSampleDesc 1 0)
            dxgiUsageRenderTargetOutput
            1
            hWnd
            True
            DxgiSwapEffectDiscard
            0
  
  Right (swapChain, device, featureLevel, deviceContext) <- d3d11CreateDeviceAndSwapChain
          nullPtr 
          D3DDriverTypeSoftware
          nullPtr
          [D3D11CreateDeviceDebug]
          [D3DFeatureLevel11_0, D3DFeatureLevel10_1, D3DFeatureLevel10_0]
          sd
  
  Right (backBuffer :: Ptr ID3D11Texture2D) <- getBuffer swapChain (fromIntegral 0)
  Right renderTargetView <- use backBuffer $ \b -> createRenderTargetView device b Nothing
  
  omSetRenderTargets deviceContext [renderTargetView] nullPtr
  rsSetViewports deviceContext [D3D11Viewport 0 0 800 600 0 1]
  
  return (swapChain, device, deviceContext, renderTargetView)

compileShaderFromFile :: String -> String -> String -> IO (Ptr ID3DBlob)
compileShaderFromFile fileName entryPoint shaderModel = do
  Right res <- d3dCompileFromFile 
      fileName
      Nothing
      Nothing
      nullPtr
      (Just entryPoint)
      shaderModel
      [d3dCompileEnableStrictness]
      []
  return res

createDefaultWindow :: Int -> Int -> WindowClosure -> IO HWND
createDefaultWindow width height wndProc = do
  let winClass = mkClassName "Triangle Window"
  icon         <- loadIcon   Nothing iDI_APPLICATION
  cursor       <- loadCursor Nothing iDC_ARROW
  bgBrush      <- createSolidBrush (rgb 255 255 255)
  mainInstance <- getModuleHandle Nothing
  registerClass
    ( cS_VREDRAW + cS_HREDRAW
    , mainInstance
    , Just icon
    , Just cursor
    , Just bgBrush
    , Nothing
    , winClass )
  w <- createWindow
       winClass
       "Triangle"
       wS_OVERLAPPEDWINDOW
       Nothing Nothing
       (Just width)
       (Just height)
       Nothing
       Nothing
       mainInstance
       wndProc
  
  showWindow w sW_SHOWNORMAL
  updateWindow w
  return w

pM_NOREMOVE, pM_REMOVE, pM_NOYIELD :: UINT
pM_NOREMOVE = 0x0000
pM_REMOVE = 0x0001
pM_NOYIELD = 0x0002

messagePump 
  :: HWND -> Ptr ID3D11DeviceContext -> 
     Ptr IDxgiSwapChain -> Ptr ID3D11RenderTargetView ->
     Ptr ID3D11VertexShader -> Ptr ID3D11PixelShader -> IO ()
messagePump hwnd deviceContext swapChain renderTargetView vs ps = Graphics.Win32.allocaMessage $ \ msg ->
  let pump = do
        m <- peekByteOff msg 4 :: IO WindowMessage
        when (m /= wM_QUIT) $ do
          r <- c_PeekMessage msg (maybePtr Nothing) 0 0 pM_REMOVE
          if r /= 0
          then do
            translateMessage msg
            dispatchMessage msg
            return ()
          else do
            render deviceContext swapChain renderTargetView vs ps
          pump
  in pump
  
render 
  :: Ptr ID3D11DeviceContext -> Ptr IDxgiSwapChain -> Ptr ID3D11RenderTargetView ->
     Ptr ID3D11VertexShader -> Ptr ID3D11PixelShader -> IO ()
render deviceContext swapChain renderTargetView vs ps= do
  clearRenderTargetView deviceContext renderTargetView $ Color 0.0 0.125 0.3 1.0
  
  vsSetShader deviceContext vs []
  psSetShader deviceContext ps []
  draw deviceContext 3 0
  
  present swapChain 0 0
  return ()