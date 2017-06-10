{-# LANGUAGE TemplateHaskell #-}
module GithubIssue9 where

import Language.Haskell.TH

foo :: Q Exp
foo = [| \f -> f 2 |]
