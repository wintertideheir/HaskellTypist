module Data.Focus where

data FocusedList a = FocusedList { before :: [a]
                                 , focus  :: a
                                 , after  :: [a]
                                 }

focusList :: [a] -> FocusedList a
focusList f = FocusedList { before = []
                          , focus  = head f
                          , after  = tail f
                          }

focusUp :: FocusedList a -> FocusedList a
focusUp l = if null l.before
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
