module Filter
  ( done
  , todo
  , doing
  ) where

done :: String -> Bool
done (' ':'-':' ':'[':'x':']':_) = True
done ('\x1B':'[':'9':'m':_) = True
done _ = False

todo :: String -> Bool
todo n = (not . done) n

doing :: String -> Bool
doing (' ':'-':' ':'[':' ':']':' ':'…':_) = True
doing ('\x1B':'[':'7':'m':_) = True
doing _ = False
