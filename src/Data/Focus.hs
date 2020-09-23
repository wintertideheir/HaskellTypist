module Data.Focus where

data FocusedList a = FocusedList { before :: [a]
                                 , focus :: a
                                 , after  :: [a]
                                 }

instance Foldable FocusedList where
   foldMap l = foldMap (l.before ++ [l.focus] ++ l.after)

focusUp :: FocusedList a -> FocusedList a
focusDown l = if null l.before
              then l
              else FocusedList { before = init l.before
                               , focus  = last l.before
                               , after  = (l.focus):(l.after)
                               }

focusDown :: FocusedList a -> FocusedList a
focusDown l = if null l.after
              then l
              else FocusedList { before = l.before ++ [l.focus]
                               , focus  = head l.after
                               , after  = tail l.after
                               }
