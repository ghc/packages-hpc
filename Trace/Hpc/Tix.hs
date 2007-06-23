------------------------------------------------------------
-- Andy Gill and Colin Runciman, June 2006
------------------------------------------------------------

-- | Datatypes and file-access routines for the tick data file 
-- used by HPC. (.tix)
module Trace.Hpc.Tix(Tix(..), TixModule(..), 
		     tixModuleName, tixModuleHash, tixModuleTixs,
		     readTix, writeTix, getTixProgName) where

import Data.List (isSuffixOf)
import Trace.Hpc.Util(Hash)

-- 'Tix ' is the storage format for our dynamic imformation about what
-- boxes are ticked.
data Tix = Tix [TixModule]
	deriving (Read, Show, Eq)

data TixModule = TixModule 
		 String    -- module name
		 Hash	   -- hash number
		 Int 	   -- length of tix list (allows pre-allocation at parse time).
		 [Integer] -- actual ticks
	deriving (Read, Show, Eq)

tixModuleName :: TixModule -> String
tixModuleName (TixModule nm _ _ _) = nm
tixModuleHash :: TixModule -> Hash
tixModuleHash (TixModule _ h  _ _) = h
tixModuleTixs :: TixModule -> [Integer]
tixModuleTixs (TixModule  _ _ _ tixs) = tixs

-- We /always/ read and write Tix from the current working directory.

-- read a Tix File.
readTix :: String -- ^ binary name
	-> IO (Maybe Tix)
readTix pname = 
  catch (do contents <- readFile $ tixName pname 
	    return $ Just $ read contents)
	(\ _ -> return $ Nothing)

-- write a Tix File.
writeTix :: String -- ^ binary name
	 -> Tix 
	 -> IO ()
writeTix pname tix = 
  writeFile (tixName pname) (show tix)

tixName :: String -> String
tixName name = name ++ ".tix"

getTixProgName :: String -> String
getTixProgName str | ".tix" `isSuffixOf` str 
		   = reverse $ drop (length ".tix") $ reverse str
		   | otherwise
		   = str

