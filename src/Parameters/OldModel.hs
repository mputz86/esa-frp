module Parameters.OldModel where


import Parameters.Model
---------------------------------------------------------------------------------
-- old model only for Netw
--------------------------------------------------------------------------------


-- | full configuration of the process
data ProcessingConfig r = ProcessingConfig
    { killProcess :: IO ()
    -- ^ when to stop the process
    , calibrationModel :: CalibrationModel r
    -- ^ the chosen calibration model
    , pullRawParameter :: IO r
    -- ^ how to wait for a row parameter
    , pullCalibrationCoefficient :: IO (CalibrationCoefficient r)
    -- ^ how to wait for a calibration coefficient change
    , pullLimit :: IO (InputLimit r)
    -- ^ how to wait for a single limit change
    , pushResult :: ProcessingOutput r -> IO ()
    -- ^ how to push results, dangerous if slower than pullRawParameter
    }

-- | initial values for the process
data ProcessingInitial r = ProcessingInitial
    { initialCalibrationCoefficient :: CalibrationCoefficient r
    -- ^ initial calibration coefficient value
    , initialLimits :: LimitsMap r
    -- ^ inital map of limits
    }
