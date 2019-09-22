{-|
Stuff I want to do with this
-}

-- Multi-stage wheel drive
wheel :: Cur Double
wheel = do
  cylinder (5cm) (35mm)
  cylinder (2cm) (35mm)
  move $ -35mm z
  return 0.4

drive :: Cur Double
drive = do
  let offset = 20cm x
  prod <$> mapM (+ wheel) [offset, negate offset,
                           offset, negate offset]


-- Arduino car
frame :: Cur ()
frame = do
  rect (6i, 6i, 0.25i)
  (m1, m2) <- at (0, 2i, 0) do
    -- ...


-- TODO: scrap the design-first approach.
-- The first step should be to write a gloss UI that autoloads a haskell DSL and
-- then iterate rapidly. Can we interface with emacs for live editing?
