module Holiplan.Error (Error (..)) where
import Hasql.Pool (UsageError)

data Error
  = UsageError UsageError
  | ParseError String
  deriving stock (Show)
