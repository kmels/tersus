module Tersus.DataTypes.Address where 

import Tersus.DataTypes.Addressable
import Tersus.DataTypes.AppInstance

instance Addressable Address where
    getAppInstance (Address (User _ nickname _ _) (TApp _ id' _ _ _ _ _)) = AppInstance nickname id'