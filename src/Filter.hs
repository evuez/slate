module Filter
  ( done
  , todo
  , doing
  ) where

import Task as T (State(..), Task(..))

done :: T.Task -> Bool
done (T.Task T.Done _ _ _) = True
done (T.Task _ _ _ _) = False

todo :: T.Task -> Bool
todo (T.Task T.Todo _ _ _) = True
todo (T.Task T.Doing _ _ _) = True
todo (T.Task _ _ _ _) = False

doing :: T.Task -> Bool
doing (T.Task T.Doing _ _ _) = True
doing (T.Task _ _ _ _) = False
