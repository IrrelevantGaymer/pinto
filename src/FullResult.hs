{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module FullResult where {
    import Control.Applicative (Alternative(..));

    data FullResult e f t = Ok t | Soft e | Hard f | Empty deriving(Show);
    type SoftFull t f e = FullResult e f t;
    type HardFull t e f = FullResult e f t;

    infixl 4 <:$:>, <:$, <|$|>, <|$;
    infixl 1 <:&:>, <|&|>;
    -- infixl 1 >>:, >>:=, >>|, >>|=;
    -- infixr 1 =:<<, =|<<;
    -- infixl 4 <:*:>, <:*, *:>, <:**:>, <|*|>, <|*, *|>, <|**|>;

    class FullFunctor ff where {
        sfmap :: (e -> u) -> ff e f t -> ff u f t;
        (<:$) :: u -> ff e f t -> ff u f t;
        (<:$) = sfmap . const;

        hfmap :: (f -> v) -> ff e f t -> ff e v t;
        (<|$) :: v -> ff e f t -> ff e v t;
        (<|$) = hfmap . const;
    };

    (<:$:>) :: FullFunctor ff => (e -> u) -> ff e f t -> ff u f t;
    (<:$:>) = sfmap;

    (<:&:>) :: FullFunctor ff => ff e f t -> (e -> u) -> ff u f t;
    (<:&:>) = flip sfmap;

    (<|$|>) :: FullFunctor ff => (f -> v) -> ff e f t -> ff e v t;
    (<|$|>) = hfmap;

    (<|&|>) :: FullFunctor ff => ff e f t -> (f -> v) -> ff e v t;
    (<|&|>) = flip hfmap;

    class FullFunctor ff => FullApplicative ff where {
        spure :: e -> ff e f t;

        (<:*:>) :: ff (e -> u) f t -> ff e f t -> ff u f t;
        (<:*:>) = sliftA2 id;

        sliftA2 :: (e1 -> e2 -> u) -> ff e1 f t -> ff e2 f t -> ff u f t;
        sliftA2 f x = (<:*:>) (sfmap f x);

        (*:>) :: ff e f t -> ff u f t -> ff u f t;
        u *:> v = (id <:$ u) <:*:> v;

        (<:*) :: ff e f t -> ff u f t -> ff e f t;
        (<:*) = sliftA2 const;

        hpure :: f -> ff e f t;

        (<|*|>) :: ff e (f -> v) t -> ff e f t -> ff e v t;
        (<|*|>) = hliftA2 id;

        hliftA2 :: (f1 -> f2 -> v) -> ff e f1 t -> ff e f2 t -> ff e v t;
        hliftA2 f x = (<|*|>) (hfmap f x);

        (*|>) :: ff e f t -> ff e v t -> ff e v t;
        u *|> v = (id <|$ u) <|*|> v;

        (<|*) :: ff e f t -> ff e v t -> ff e f t;
        (<|*) = hliftA2 const;
    };

    (<:**:>) :: FullApplicative ff => ff e f t -> ff (e -> u) f t -> ff u f t;
    (<:**:>) = sliftA2 (\a sf -> sf a);

    (<|**|>) :: FullApplicative ff => ff e f t -> ff e (f -> v) t -> ff e v t;
    (<|**|>) = hliftA2 (\a hf -> hf a);

    class FullApplicative fm => FullMonad fm where {
        (>>:=) :: forall e u f t. fm e f t -> (e -> fm u f t) -> fm u f t;

        (>>:) :: forall e u f t. fm e f t -> fm u f t -> fm u f t;
        m >>: k = m >>:= const k;

        sreturn :: e -> fm e f t;
        sreturn = spure;

        (>>|=) :: forall e f v t. fm e f t -> (f -> fm e v t) -> fm e v t;

        (>>|) :: forall e f v t. fm e f t -> fm e v t -> fm e v t;
        m >>| k = m >>|= const k;

        hreturn :: f -> fm e f t;
        hreturn = hpure;
    };

    sjoin :: FullMonad fm => fm (fm e f t) f t -> fm e f t;
    sjoin x = x >>:= id;

    (=:<<) :: FullMonad fm => (e -> fm u f t) -> fm e f t -> fm u f t;
    f =:<< x = x >>:= f;

    hjoin :: FullMonad fm => fm e (fm e f t) t -> fm e f t;
    hjoin x = x >>|= id;

    (=|<<) :: FullMonad fm => (f -> fm e v t) -> fm e f t -> fm e v t;
    f =|<< x = x >>|= f;

    instance Functor (FullResult e f) where {
        fmap f (Ok   x) = Ok $ f x;
        fmap _ (Soft x) = Soft x;
        fmap _ (Hard x) = Hard x;
        fmap _ Empty    = Empty;
    };

    instance FullFunctor FullResult where {
        sfmap f (Soft x) = Soft $ f x;
        sfmap _ (Ok   x) = Ok x;
        sfmap _ (Hard x) = Hard x;
        sfmap _ Empty    = Empty;
        hfmap f (Hard x) = Hard $ f x;
        hfmap _ (Soft x) = Soft x;
        hfmap _ (Ok   x) = Ok x;
        hfmap _ Empty    = Empty;
    };

    instance Applicative (FullResult e f) where {
        pure x = Ok x;
        Ok f   <*> x = fmap f x;
        Soft x <*> _ = Soft x;
        Hard x <*> _ = Hard x;
        Empty  <*> _ = Empty;
    };

    instance FullApplicative FullResult where {
        spure = Soft;
        hpure = Hard;

        Soft f <:*:> x = sfmap f x;
        Ok x   <:*:> _ = Ok x;
        Hard x <:*:> _ = Hard x;
        Empty  <:*:> _ = Empty;
        Hard f <|*|> x = hfmap f x;
        Ok x   <|*|> _ = Ok x;
        Soft x <|*|> _ = Soft x;
        Empty  <|*|> _ = Empty;
    };

    instance Alternative (FullResult e f) where {
        empty = Empty;
        Empty  <|> Empty  = Empty;
        Empty  <|> Soft x = Soft x;
        Soft x <|> Empty  = Soft x;
        Soft x <|> Soft _ = Soft x;
        Hard x <|> _      = Hard x;
        _      <|> Hard x = Hard x;
        Ok   x <|> _      = Ok x;
        _      <|> Ok x   = Ok x;
    };

    instance Monad (FullResult e f) where {
        Ok   x >>= f = f x;
        Soft x >>= _ = Soft x;
        Hard x >>= _ = Hard x;
        Empty  >>= _ = Empty;
    };

    instance FullMonad FullResult where {
      Soft x >>:= f = f x;
      Ok x   >>:= _ = Ok x;
      Hard x >>:= _ = Hard x;
      Empty  >>:= _ = Empty;
      Hard x >>|= f = f x;
      Ok x   >>|= _ = Ok x;
      Soft x >>|= _ = Soft x;
      Empty  >>|= _ = Empty;
    };

    ok :: FullResult e f t -> Maybe t;
    ok (Ok x) = Just x;
    ok _      = Nothing;

    soft :: FullResult e f t -> Maybe e;
    soft (Soft x) = Just x;
    soft _        = Nothing;

    hard :: FullResult e f t -> Maybe f;
    hard (Hard x) = Just x;
    hard _        = Nothing;

    swapErrors :: FullResult e f t -> FullResult f e t;
    swapErrors (Ok x) = Ok x;
    swapErrors (Soft x) = Hard x;
    swapErrors (Hard x) = Soft x;
    swapErrors Empty = Empty;

    injectHard :: FullResult e f t -> v -> FullResult e v t;
    injectHard (Ok x) _ = Ok x;
    injectHard _ h = Hard h;

    injectMapHard :: FullResult e f t -> (e -> f) -> FullResult e f t;
    injectMapHard (Ok   x) _ = Ok x;
    injectMapHard (Hard x) _ = Hard x;
    injectMapHard Empty    _ = Empty;
    injectMapHard (Soft x) f = Hard $ f x;

}
