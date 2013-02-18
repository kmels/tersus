module Tersus.DataTypes.Address where 

import Tersus.DataTypes

instance Addressable Address where
    getAppInstance (Address (User _ _ nickname _ _) (TApp _ id' _ _ _ _ _)) = AppInstance nickname id'