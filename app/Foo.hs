import Control.Applicative (Alternative((<|>)))
type Handler = IO ()
type Route = (String, Handler)

foo = print "foo"
bar = print "bar"
baz = print "baz"

r = ("foo", foo)

path = ["foo"]

(-->) a b = (Just a, b)
root b = (Nothing, b)

routes :: [(Maybe [Char], Handler)]
routes =
  [ "foo" --> foo
  , "bar" --> bar
  ]

mapp :: [Char] -> Maybe Handler
mapp path = foldl h Nothing routes
  where
    h (Just app) _ = Just app
    h _ (Nothing, app) = Just app
    h _ (Just pattern, app)
      | pattern == path = Just app
      | otherwise = Nothing


type App = [String] -> IO ()
app path = pure ()

run eff = case eff of
    Nothing -> print "nada"
    Just e  -> e

main :: IO ()
main = do
  run $ mapp "foo"
  run $ mapp "bar"
  run $ mapp "z"
  app path
