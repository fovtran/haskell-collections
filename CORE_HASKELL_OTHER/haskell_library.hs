{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.C
hsHelloWorld = newCString "Hello from Haskell"
foreign export ccall "hsHelloWorld" hsHelloWorld :: IO CString
main = return ()

/livemounts/haskell/ghc-8.0.1-x86_64/bin/ghc -cpp -optc-std=c++0x -optP-lpthread -O2 -threaded -shared -no-hs-main -fPIC -o hello /opt/projects/helloworld.hs -I. -I/usr/include -L. -lstdc++ -lpthread
/livemounts/haskell/ghc-8.0.1-x86_64/bin/ghc -cpp -dynamic -optc-std=c++0x -optP-lpthread -O2 -threaded -fPIC -o hello /opt/projects/helloworld.hs -I. -I/usr/include -L. -lstdc++ -lpthread -L/livemounts/haskell/ghc-8.0.1-x86_64/lib/ghc-8.0.1/rts/ -lHSrts-ghc8.0.1
/livemounts/haskell/ghc-8.0.1-x86_64/bin/ghc -dynamic -o hello /opt/projects/helloworld.hs -I. -I/usr/include -L.
