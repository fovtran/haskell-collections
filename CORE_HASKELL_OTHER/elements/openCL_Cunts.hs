{-# LANGUAGE QuasiQuotes #-}

import           Control.Parallel.CLUtil         (OpenCLState (OpenCLState),
                                                  bufferToVector, clContext,
                                                  clDevice, clQueue,
                                                  writeVectorToBuffer)
import           Control.Parallel.OpenCL

import           Control.Monad                   (forM_)
import           Data.Vector.Storable            (Vector)
import qualified Data.Vector.Storable            as V
import           Foreign                         (nullPtr, sizeOf)
import           Foreign.C.Types                 (CFloat)
import           Language.C.Quote.OpenCL         (cfun)
import           Text.PrettyPrint.Mainland       (prettyCompact)
import           Text.PrettyPrint.Mainland.Class (ppr)


-- | Summarises the OpenCL Platforms and their Devices.
--
--   The names of Platforms and all Devices belonging to them are printed to
--   stdout.
describePlatforms :: IO ()
describePlatforms = do

    -- fetch the list of OpenCL Platforms
    platformList <- clGetPlatformIDs :: IO [CLPlatformID]

    -- for each platform,
    forM_ platformList $ \platform -> do

        -- fetch the list of OpenCL Devices
        devs <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL :: IO [CLDeviceID]

        -- print the Platform name and Device names
        pname platform
        forM_ devs dname

  where
      putPair name value = putStrLn (name ++ value)
      pname p = clGetPlatformInfo p CL_PLATFORM_NAME >>= putPair "Platform: "
      dname d = clGetDeviceName d                    >>= putPair "  Device: "
	  
	-- (... inside an IO do block ...)

	-- Create a Context, Queue and a CLUtil OpenCLState
	context <- clCreateContextFromType [] [CL_DEVICE_TYPE_CPU] print
	device  <- head <$> clGetContextDevices context
	queue   <- clCreateCommandQueue context device []
	-- NB: OpenCLState is used by CLUtil when manipulating vector buffers
	let state = OpenCLState
			  { clDevice  = device
			  , clContext = context
			  , clQueue   = queue
			  }
			  
	-- | The kernel to execute: the equivalient of 'map (*2)'.
	kernelSource :: String
	kernelSource = prettyCompact . ppr $ [cfun|
		/* This example kernel just does `map (*2)` */
		kernel void doubleArray(
			global float *in,
			global float *out
		) {
			int i = get_global_id(0);
			out[i] = 2 * in[i];
		}
	|]

	-- (... inside an IO do block ...)

	-- Create the Kernel
	program <- clCreateProgramWithSource context kernelSource
	clBuildProgram program [device] ""
	kernel <- clCreateKernel program "doubleArray"

	-- (... inside an IO do block ...)

	-- Set up memory
	let
		inputData :: Vector CFloat
		inputData = V.fromList [(-4) .. 4]

		nElem  = V.length inputData
		nBytes = nElem * sizeOf (undefined :: CFloat)

	-- Buffers for input and output data.
	-- We request OpenCL to create a buffer on the host (CL_MEM_ALLOC_HOST_PTR)
	-- since we're using CPU. The performance here may not be ideal, because
	-- we're copying the buffer. However, it's safe
	bufIn <- clCreateBuffer context
							[CL_MEM_READ_ONLY, CL_MEM_ALLOC_HOST_PTR]
							(nBytes, nullPtr)
	bufOut <- clCreateBuffer context
							 [CL_MEM_WRITE_ONLY, CL_MEM_ALLOC_HOST_PTR]
							 (nBytes, nullPtr)

	-- Copy our input data Vector to the input buffer; blocks until complete
	writeVectorToBuffer state bufIn inputData

	-- (... inside an IO do block ...)

	-- Run the kernel
	clSetKernelArgSto kernel 0 bufIn
	clSetKernelArgSto kernel 1 bufOut
	execEvent <- clEnqueueNDRangeKernel queue kernel [nElem] [] []

	-- (... inside an IO do block ...)

	-- Get the result; blocks until complete
	outputData <- bufferToVector queue
								 bufOut
								 nElem
								 [execEvent]
								 :: IO (Vector CFloat)
								 
								 