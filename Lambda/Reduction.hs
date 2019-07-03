module Lambda.Reduction (reduce) where

import Lambda.Term (NTerm(..), beta)

-- To reduce a term to a normal form (if it has one), we simple apply
-- the stepping function until it can no longer make any progress.
reduce :: NTerm -> NTerm
reduce = loop . pure
  where loop = either id (loop . step)

-- This function performs a single step of a normal-order reduction to
-- the given term. If the term is irreducible, it returns the term
-- wrapped in a `Left`; otherwise, it returns the reduced term in a
-- `Right`.
step :: NTerm -> Either NTerm NTerm
step t@(NVar _ _)          = Left t
step (NAbs n b)            = NAbs n <$$> step b
step t@(NApp (NAbs _ _) _) = Right $ beta t
step (NApp t u)            = case step t of
                               Left t'  -> NApp t <$$> step u
                               Right t' -> Right $ NApp t' u

(<$$>) :: (a -> a) -> Either a a -> Either a a
f <$$> Left x  = Left $ f x
f <$$> Right x = Right $ f x
