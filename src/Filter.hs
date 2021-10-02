module Filter
  ( done
  , todo
  , doing
  ) where

import qualified Task as T (Status(..), Task(..))

done :: T.Task -> Bool
done (T.Task _ T.Done _ _) = True
done T.Task {} = False

todo :: T.Task -> Bool
todo (T.Task _ T.Todo _ _) = True
todo (T.Task _ T.Doing _ _) = True
todo T.Task {} = False

doing :: T.Task -> Bool
doing (T.Task _ T.Doing _ _) = True
doing T.Task {} = False
