module Data.Focus where

modifyNth :: [a] -> Int -> (a -> a) -> [a]
modifyNth l i f =  take i l
                ++ [f (l !! i)]
                ++ drop (i + 1) l

data FocusedList a = FocusedList   [a] Int
                   | UnfocusedList [a]

mapFocus :: (a -> a) -> FocusedList a -> FocusedList a
mapFocus f (FocusedList l i) = FocusedList (modifyNth l i f) i
mapFocus _ (UnfocusedList l) = UnfocusedList l

focusI :: FocusedList a -> Maybe Int
focusI (FocusedList _ i) = Just i
focusI (UnfocusedList _) = Nothing

focusA :: FocusedList a -> Maybe a
focusA (FocusedList l i) = Just (l !! i)
focusA (UnfocusedList l) = Nothing

refocus ::  Int -> FocusedList a -> FocusedList a
refocus i l = let l' = unwrap l
              in if (length l' > i)
                 then FocusedList l' i
                 else l

unfocus :: FocusedList a -> FocusedList a
unfocus (FocusedList l _) = UnfocusedList l
unfocus (UnfocusedList l) = UnfocusedList l

unwrap :: FocusedList a -> [a]
unwrap (FocusedList l _) = l
unwrap (UnfocusedList l) = l

wrap :: [a] -> FocusedList a
wrap = UnfocusedList

rewrap :: ([a] -> [a]) -> FocusedList a -> FocusedList a
rewrap f (FocusedList l i) = refocus i $ FocusedList   (f l) i
rewrap f (UnfocusedList l) = refocus i $ UnfocusedList (f l)
