{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Data.PView
  (PView (..),

  PViewT,

  Source (..),
  Lens (..),
  ParametricLens (..),

  merge,

  read,
  update,
  observe) where

import Control.Monad (foldM, foldM_, when)
import Control.Monad.Trans (lift)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Class (MonadWriter)
import qualified Control.Monad.Writer as W (execWriterT, tell)
import qualified Control.Monad.State as ST (get, modify)
import Control.Applicative ((<$>), (<*>), Applicative)

import Data.Typeable
import Data.Dynamic

import Data.List
import Data.Maybe

import Prelude hiding (read)

-------------------------------------------------------------------------------

instance Show (NRequest m) where
   show (NRequest rid _ dyn) = "\n" ++ show rid ++ ": " ++ show (dynTypeRep dyn)

instance Show NEvent where
   show (NEvent vid _) = "\n" ++ show vid

-------------------------------------------------------------------------------

-- Monad transformer for the notification engine
type PViewT m = StateT [NRequest m] m
  
-- Invalidation function: pure and monadic (utilized in the engine)
type Invalidate phi = phi -> Bool

-- Notification request/event
type ObserveId = String
type ObserveHnd m = m ()

data NEvent = forall phi. Typeable phi => NEvent TypeRep (Invalidate phi)
data NRequest m = NRequest ObserveId (ObserveHnd m) Dynamic

-- Parametric view GADT   
data PView phi m a where
    Source      :: (Applicative m, Monad m, Typeable phi) 
                => Source phi m a                            -> PView phi m a
    			
    Project     :: (Applicative m, Monad m, Typeable phi)
                => PView phi m a -> Lens a b            -> PView phi m b  
    
    Translate   :: (Applicative m, Monad m, Typeable phi)
                => PView phi m a -> (psi -> phi)           -> PView psi m a 
    
    Focus       :: (Applicative m, Monad m, Typeable phi, Typeable psi, Eq phi)
                => PView phi m a -> ParametricLens psi a b -> PView (phi, psi) m b 
    
    Product     :: (Applicative m, Monad m, Typeable phi, Typeable psi)
                => PView phi m a -> PView psi m b      -> PView (phi, psi) m (a, b)
                
data Source phi m a where
    MkSource :: (Applicative m, Monad m, Typeable phi) => (phi -> m a) -> (phi -> a -> m (Invalidate phi)) -> Source phi m a
                
data Lens a b = MkLens { 
  get  :: a      -> b,
  put  :: a -> b -> a
}

data ParametricLens psi a b = MkPLens {
  getf  :: psi -> a -> b,
  putf  :: psi -> a -> b -> (a, Invalidate psi)
}
   
-- Reading a parametric view
read :: (Applicative m, Monad m, Typeable phi) => PView phi m a -> phi -> PViewT m a
read (Source (MkSource sread _)) p = lift (sread p)
read (Project v l) p = get l <$> read v p
read (Translate v tr) p = read v (tr p)
read (Focus v l) (p,q) = getf l q <$> read v p
read (Product vl vr) (p,q) = (,) <$> read vl p <*> read vr q

-- Update a parametric view:
-- 1. do the actual update while generating notification events
-- 2. match notification events with notification requests
update :: (Applicative m, Monad m, Typeable phi) => PView phi m a -> phi -> a -> PViewT m ()
update v p a = W.execWriterT (update' v p a) >>= trigger . reverse

trigger :: (Applicative m, Monad m) => [NEvent] -> PViewT m ()
trigger es = do
  rs <- ST.get
  foldM_ (\skips event -> foldM (match event) skips rs) [] es
  where
    match (NEvent vid inval) skips (NRequest oid ohnd dyn)
      | notElem oid skips && dynTypeRep dyn == vid
         = case fromDynamic dyn of
             Just p -> when (inval p) (lift ohnd) >> return (oid:skips)
             Nothing  -> return skips
    match _ skips _ = return skips

returnE :: (MonadWriter [NEvent] m, Typeable a, Typeable phi)
        => a -> Invalidate phi
        -> m (Invalidate phi)
returnE p inval = W.tell [NEvent (typeOf p) inval] >> return inval
         
update' :: (Applicative m, Monad m, Typeable phi) => PView phi m a -> phi -> a -> WriterT [NEvent] (PViewT m) (Invalidate phi)
update' (Source (MkSource _ supdate)) p a 
  = (lift.lift) (supdate p a) >>= \inval -> returnE p inval
		  
update' (Project v l) p a 
  = lift (read v p) >>= \s -> update' v p (put l s a)

update' (Translate v tr) p a = (.tr) <$> update' v (tr p) a

update' (Focus v l) (p,q) a = do
  s <- lift $ read v p
  let (s', invalr) = putf l q s a
  invall <- update' v p s'
  returnE (p,q) (comp invalr invall)
  where 
    comp invall invalr (p',q')
      | p == p'   = invall q'
      | otherwise = invalr p'
    
update' (Product vl vr) (p,q) (a,b) = do
  invall  <- update' vl p a
  invalr  <- update' vr q b
  return  $ \(p,q) -> invall p || invalr q
        
-- Subscribing for change notification
observe :: (Applicative m, Monad m, Typeable phi) => PView phi m a -> phi -> ObserveId -> ObserveHnd m -> PViewT m ()
observe v p oid ohnd = do
  rs <- genreqs v p
  ST.modify (map (NRequest oid ohnd) rs ++)
  where
    genreqs :: (Applicative m, Monad m, Typeable phi) => PView phi m a -> phi -> PViewT m [Dynamic]
    genreqs (Source _) p = return [toDyn p]
    genreqs (Project v _) p = genreqs v p
    genreqs (Translate v tr) p = genreqs v (tr p)
    genreqs (Focus v _) p = (toDyn p :) <$> genreqs v (fst p)
    genreqs (Product vl vr) (p, q) = (++) <$> genreqs vl p <*> genreqs vr q
    mrg p ls rs = toDyn p : ls ++ rs

merge :: ParametricLens phi s a -> ParametricLens psi s a 
	  -> (phi -> s -> a -> psi) -> (psi -> s -> a -> phi) -> ParametricLens (Either phi psi) s a
merge l r l2r r2l = MkPLens {getf = get, putf = put}
  where
    get (Left p) s  = getf l p s
    get (Right p) s = getf r p s  
	
    put (Left p) s a = (retl, invalm) where
		
		(retl, invall) = putf l p s a 
		invalm (Left p') = invall p'
		invalm (Right p') = snd (putf r (l2r p s a) s a) p'
		
    put (Right p) s a = (retr, invalm) where
		
		(retr, invalr) = putf r p s a 
		invalm (Right p') = invalr p'
		invalm (Left p') = snd (putf l (r2l p s a) s a) p'
