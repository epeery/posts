{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module CategorizedData where

import Data.Text

data Question
  = Question
      { questionInfo :: Text,
        questionTopic :: Text
      }
  deriving (Show)

type Opinion = Double

type Candidate = [(Topics, Opinion)]

data Environment
  = ParisAgreement
  | GreenNewDeal

data Economics
  = EstateTax
  | SupportNAFTA

data Healthcare
  = SinglePayerSystem
  | PublicHealthInsurance

-- Our custom type
data a + b = InL a | InR b

infixr 8 +

example2 :: Int + String + Bool
example2 = InR (InR True)

type Topics = Environment + Economics + Healthcare

class IsTopic a where
  getQuestion :: a -> Question

instance IsTopic Environment where
  getQuestion ParisAgreement =
    Question "The U.S. should rejoin the Paris Climate Agreement" "Environment"
  getQuestion GreenNewDeal =
    Question "The Green New Deal is a good idea" "Environment"

instance IsTopic Economics where
  getQuestion EstateTax =
    Question "I am in favor of a national estate tax" "Economics"
  getQuestion SupportNAFTA =
    Question "The US should support NAFTA" "Economics"

instance IsTopic Healthcare where
  getQuestion SinglePayerSystem =
    Question "The U.S. should have a single-payer healthcare system" "Healthcare"
  getQuestion PublicHealthInsurance =
    Question "The U.S. should have some form of public health insurance" "Healthcare"

instance (IsTopic a, IsTopic b) => IsTopic (a + b) where
  getQuestion (InL x) = getQuestion x
  getQuestion (InR y) = getQuestion y

class a :<: b where
  inj :: a -> b

instance a :<: a where
  inj = id

instance a :<: (a + b) where
  inj = InL

instance {-# OVERLAPPABLE #-} (a :<: c) => a :<: (b + c) where
  inj = InR . inj

topics :: [Topics]
topics =
  [ inj ParisAgreement
  , inj GreenNewDeal
  , inj EstateTax
  , inj SupportNAFTA
  , inj SinglePayerSystem
  , inj PublicHealthInsurance
  ]

questions :: [Question]
questions = getQuestion <$> topics
