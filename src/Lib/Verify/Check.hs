module Lib.Verify.Check (verify) where

import Lib.Model.CTL (CTL)
import Lib.Model.TS (TS)

-- Does the CTL formula hold in the given TS?
verify :: TS -> CTL -> Bool
verify _ _ = False