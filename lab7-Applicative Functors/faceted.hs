import Control.Applicative
import qualified Data.Set as Set

type Label = String
type View = Set.Set Label

data FacetedValue a = Raw a
                     | Facet Label (FacetedValue a) (FacetedValue a)
                     
view :: View -> FacetedValue a -> a
view _ (Raw x) = x
view labels (Facet k auth unauth) =
     if Set.member k labels then
       view labels auth
     else
       view labels unauth
       
instance Functor FacetedValue where
   fmap f (Raw a) = (Raw (f a))
   fmap f (Facet label trust notrust) = Facet label (fmap f trust) (fmap f notrust)

testFmap = do
  let creditCard = Facet "visa" (Raw 4111111111111111) (Raw 0)
      ccPlusOne = fmap (+1) creditCard

  -- view (Set.fromList ["visa"]) (fmap (+1) Facet "visa" (Raw 4111111111111111) (Raw 0))
  putStrLn "Credit card views:"
  print $ view (Set.fromList ["visa"]) ccPlusOne -- Should print 4111111111111112
  print $ view (Set.fromList ["mastercard"]) ccPlusOne -- Should print 1

instance Applicative FacetedValue where
   pure = Raw
   -- Case 1: Both are depth 2 (BOA <*> WF)
   Facet x (Raw authx) (Raw unauthx) <*> Facet y (Raw authy) (Raw unauthy) = Facet x (Facet y (Raw (authx authy)) (Raw (authx unauthy))) (Facet y (Raw (unauthx authy)) (Raw (unauthx unauthy)))
   
   -- More cases to consider.. 
   -- Case 2: Left is depth 4+, Right is depth2  ((BOA <*> WF ) <*> Chase)
   -- Case 3: Left is depth 2, Right is depth 4+ (BOA <*> ( WF <*> Chase)
   -- Case 4: Both are depth 4+ (BOA<*>WF<*>Chase) <*> (Name<*>Age<*>SSN)
   
testApplicative = do
   let bofaBalance = Facet "Bank of America" (Raw 44) (Raw 0)
       wellsFargoBalance = Facet "Wells Fargo" (Raw 122) (Raw 0)
       -- chase = Facet "Chase" (Raw 4) (Raw 0)
       combinedBalance = (+) <$> bofaBalance <*> wellsFargoBalance
       -- allBalance = (+) <$> combinedBalance <*> chase
       
   -- view (Set.fromList []) ((+) <$> (Facet "Bank of America" (Raw 44) (Raw 0)) <*> (Facet "Wells Fargo" (Raw 122) (Raw 0)))
   
   print $ view (Set.fromList []) combinedBalance -- Should print 0
   print $ view (Set.fromList ["Bank of America"]) combinedBalance -- Should print 44
   print $ view (Set.fromList ["Wells Fargo"]) combinedBalance -- Should print 122
   print $ view (Set.fromList ["Bank of America", "Wells Fargo"]) combinedBalance -- Should print 166
   -- print $ view (Set.fromList ["Chase"]) allBalance -- Should print 166