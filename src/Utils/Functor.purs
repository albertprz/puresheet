module App.Utils.Functor where

import Prelude

infixl 8 mapp as <$$>

mapp :: forall a b f g. Functor f => Functor g => (a -> b) -> g (f a) -> g (f b)
mapp f = map (map f)
