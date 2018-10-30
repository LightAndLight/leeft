{-# language DeriveGeneric #-}
{-# language TemplateHaskell #-}
module Lambda.Expr where

import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.TH (makePrisms)
import GHC.Generics (Generic)

data Expr
  = Free String
  | Bound !Int
  | App Expr Expr
  | Lam Expr
  deriving (Eq, Show, Generic)
instance Plated Expr where; plate = gplate
makePrisms ''Expr
