data RNode t = RValue t
             | RNode (RTree t)

data RTree t = RTree [RNode t]



data RContext t = RContext [(RTree t, RTree t)] (RNode t)

reconstruct :: RContext t -> RTree t
reconstruct (RContext [] n) = RTree [n]
reconstruct (RContext ((l, r):rest) n) = reconstruct (RContext rest (RNode $ rtconcat [l, RTree [n], r]))

rtconcat :: [RTree t] -> RTree t
rtconcat ((RTree r):[]) = RTree r
rtconcat ((RTree r):(RTree s):rs) = rtconcat ((RTree (r ++ s)):rs)

rcup :: RContext t -> RContext t
rcup (RContext [] n) = RContext [] n -- or error?
rcup (RContext ((l, r):rest) n) = RContext rest (RNode $ rtconcat [l, RTree [n], r])

rcedit :: RNode t -> RContext t -> RContext t
rcedit p (RContext r _) = RContext r p
