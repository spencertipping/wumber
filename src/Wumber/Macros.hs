{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Syntax macros to automate away some boilerplate code.
module Wumber.Macros where


import Language.Haskell.TH


-- | @reinstantiate n <$> [d| instance Foo where ... |]@ will apply @Foo@ to @n@
--   to yield @[d| instance Foo $n where ... |]@.
reinstantiate :: Type -> [Dec] -> Dec
reinstantiate n [InstanceD o c t d] = InstanceD o c (AppT t n) d
