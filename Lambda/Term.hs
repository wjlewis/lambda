module Lambda.Term (Term(..), NTerm(..), removeNames, restoreNames, beta) where

import Data.List (findIndex, intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

data Term = Var String
          | Abs String Term
          | App Term Term

-- An `NTerm` (for "Nameless Term") represents a nameless lambda term,
-- as in DeBruijn's formulation. While we can omit the names from
-- variables and abstractions, I've left them in so that they match
-- the original names in the renaming process.
data NTerm = NVar String Index
           | NAbs String NTerm
           | NApp NTerm NTerm
  deriving Show

-- What is the point of this? Some Int's represent DeBruijn indices,
-- and in this way they are special. This alias helps highlight this.
type Index = Int

-- Converting a lambda term to a nameless lambda term is
-- straightforward: we calculate variable indices by looking up their
-- position in the context, and we extend the context with the name of
-- a bound variable whenever we encounter an abstraction.
removeNames :: Term -> NTerm
removeNames t = remove t $ initContext t
  where
    remove (Var v) ctx = NVar v i
      where i = maybe (-1) id $ findIndex (==v) ctx
    remove (Abs n b) ctx = NAbs n (remove b $ n:ctx)
    remove (App t u) ctx = NApp (remove t ctx) (remove u ctx)

-- A `Context` is simply a list of names collected as we traverse down
-- into abstractions. Thus, the context ["a", "b"] indicates that we
-- are inside of 2 binders, the closest of which binds a variable
-- named "a", and the next of which binds a variable named "b". At
-- this point in a computation, any occurrence of a variable named "a"
-- would be given an index of 0, and any occurrence of "b" would be
-- given an index of 1.
type Context = [String]

-- Every term exists in some context (that is, inside of a certain
-- sequence of binders). The initial context for a term is simply the
-- set of free variables, which are not bound by any binder, but must
-- still be given unique indices.
initContext :: Term -> Context
initContext = Set.toList . freeVars

-- Beta reduction is the single inference rule that we define for our
-- reduction strategies. The only tricky part here is we must
-- decrement all free variables in the body after the substitution
-- (since we've stripped off the binder); but in order for the free
-- variables in the argument to make sense after this shift, we must
-- preemptively shift them up by 1.
beta :: NTerm -> NTerm
beta (NApp (NAbs _ b) t) = shiftFree (-1) b'
  where b' = subst 0 (shiftFree 1 t) b
beta t = t

-- At the heart of the β-relation lies the notion of "substitution":
-- replacing all occurrences of a variable in some term with another
-- term. It sounds simple, but most naive formulations are afflicted
-- by a condition in which free variables may be "captured" as a
-- result of the substitution process. In fact, all of our work in
-- converting to nameless terms has been in service of substition.
subst :: Index -> NTerm -> NTerm -> NTerm
subst i s t@(NVar _ j)
  | i == j    = s
  | otherwise = t
subst i s (NAbs n b) = NAbs n b'
  where b' = subst (i+1) (shiftFree 1 s) b
subst i s (NApp t u) = NApp (subst i s t) (subst i s u)

-- Suppose that we are substituting a term T for all occurrences of
-- some variable in a term U. If we were to naively place U within a
-- binder in T, then the indices of any free variables in U would be
-- incorrect (by 1). Instead, we must first shift the free variables
-- in U by a specified amount (+1 for each binder) so that they still
-- "point" to their original locations.
shiftFree :: Int -> NTerm -> NTerm
shiftFree k t = shift k t 0
  where
    shift k (NVar v i) level
      | i < level = NVar v i
      | otherwise = NVar v (i+k)
    shift k (NAbs n b) level = NAbs n b'
      where b' = shift k b (level+1)
    shift k (NApp t u) level = NApp t' u'
      where t' = shift k t level
            u' = shift k u level

-- Transforming a nameless term back into a regular term has one tiny
-- difficulty: if the name bound in an abstraction occurs free in the
-- abstraction's body, then we must rename the bound name so that the
-- free occurrence isn't captured. Additionally, once we rename the
-- bound variable, we must replace all occurrences of the old name
-- with the new one in the body before continuing with the name
-- restoration.
restoreNames :: NTerm -> Term
restoreNames (NVar v _) = Var v
restoreNames t@(NAbs n b)
  | n `Set.member` frees = let n' = freshName n frees
                               b' = subst 0 (NVar n' 0) b
                           in Abs n' (restoreNames b')
  | otherwise = Abs n (restoreNames b)
  where frees = freeVars t
restoreNames (NApp t u) = App (restoreNames t) (restoreNames u)

-- Given a naming hint and a set of off-limits names, we can generate
-- a fresh name by adding quote marks (') to the hint until it does
-- not occur in the off-limits set.
freshName :: String -> Set String -> String
freshName hint offLimits
  | hint `Set.member` offLimits = freshName (hint ++ "'") offLimits 
  | otherwise = hint

-- We need to be able to collect the free variables for both normal
-- lambda terms and nameless terms. This typeclass allows us to give
-- both functions the same name.
class LambdaTerm t where
  freeVars :: t -> Set String

-- To collect the free variables in a lambda term, we simply traverse
-- down the structure, maintaining sets of free and bound variables,
-- which we update as needed.
instance LambdaTerm Term where
  freeVars t = collect t Set.empty Set.empty
    where
      collect (Var v) bound free
        | v `Set.member` bound = free
        | otherwise            = Set.insert v free
      collect (Abs n b) bound free = collect b bound' free
        where bound' = Set.insert n bound
      collect (App t u) bound free = freeT `Set.union` freeU
        where freeT = collect t bound free
              freeU = collect u bound free

-- It is slightly simpler to collect the free variables in a nameless
-- lambda term: instead of maintaining two sets of variables (bound
-- and free), we simply keep track of the current binding level (the
-- number of lambda-binders we have entered) and the set of free
-- variables.
instance LambdaTerm NTerm where
  freeVars t = collect t 0 Set.empty
    where
      collect (NVar v i) level free
        | i < level = free
        | otherwise = Set.insert v free
      collect (NAbs n b) level free = collect b (level+1) free
      collect (NApp t u) level free = freeT `Set.union` freeU
        where freeT = collect t level free
              freeU = collect u level free

instance Show Term where
  show (Var x)   = x

  -- We parenthesize a subterm in an application in 2 cases: when the
  -- first subterm is an abstraction (since the bodies of abstractions
  -- extend as far to the right as possible), and when the second
  -- subterm is an application (since application is left-associative
  -- by default).
  show (App t@(Abs _ _) u) = "(" ++ show t ++ ") " ++ show u
  show (App t u@(App _ _)) = show t ++ " (" ++ show u ++ ")"
  show (App t u)           = show t ++ " " ++ show u

  -- Here we make use of the convention that abstractions with
  -- multiple bound variables represent nested/curried abstractions in
  -- order to show them in a compact format.
  show (Abs n b) = "\\" ++ names ++ " -> " ++ show body
    where
      names = intercalate " " $ (n : collectNames b)
      body  = findBody b
      collectNames t = case t of
                         (Abs n' b') -> n' : collectNames b'
                         t           -> []
      findBody t     = case t of
                         (Abs _ b') -> findBody b'
                         t          -> t
