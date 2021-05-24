# Code stats

## number of lines, words, characters

`wc --help`: "A word is a non-zero-length sequence of
characters delimited by white space"

### hAsteroids
```
~/ba-thesis/hAsteroids$ wc -lwm src/*.hs app/Main.hs
  207   844  6609 src/Collisions.hs
  185   717  5380 src/Components.hs
  135   437  3551 src/Draw.hs
   46   165  1229 src/GameLoop.hs
   64   227  1854 src/Initialize.hs
  132   647  4811 src/Input.hs
  245   803  7054 src/Resources.hs
   57   219  1603 src/SdlWrappers.hs
  175   874  5969 src/Step.hs
   99   356  2583 src/Utility.hs
   67   172  1540 app/Main.hs
 1412  5461 42183 total

# total lines of comments
~/ba-thesis/hAsteroids$ cat src/*.hs app/Main.hs | grep -P "^\s*--.*$" -c
114
```

### pure-asteroids
```
~/ba-thesis/pure-asteroids$ wc -lwm src/*.hs src/Step/*.hs app/Main.hs
  149   635  4327 src/Draw.hs
   95   350  3017 src/EventProcessing.hs
  109   404  3631 src/GameLoop.hs
   58   203  1509 src/Initialize.hs
   85   384  2859 src/Input.hs
  114   381  3334 src/Resources.hs
   56   221  1862 src/Step.hs
  197   578  3975 src/Types.hs
   89   306  2013 src/Utility.hs
   17    41   315 src/Step/Asteroids.hs
  124   571  4467 src/Step/Bullets.hs
   18    87   515 src/Step/Common.hs
   24    63   468 src/Step/Score.hs
  136   667  4754 src/Step/Ship.hs
  109   540  3630 src/Step/Ufos.hs
   50   123  1167 app/Main.hs
 1430  5554 41843 total

# total lines of comments
~/ba-thesis/pure-asteroids$ cat src/*.hs app/Main.hs | grep -P "^\s*--.*$" -c
50
```

### Asteroids by Jason Halverson
```
~/Asteroids$ wc -lwm *.cpp
  195   376  5416 asteroids.cpp
   78   182  2133 bullet.cpp
   45   163  1303 driver.cpp
  141   278  3043 flyingObject.cpp
  531  1549 14674 game.cpp
   67   197  1662 point.cpp
   98   228  2218 ship.cpp
  712  2805 22545 uiDraw.cpp
  331  1355 11533 uiInteract.cpp
   38    80   602 velocity.cpp
 2236  7213 65129 total

~/Asteroids$ wc -lwm *.h
  105   196  2327 asteroids.h
   36    81   790 bullet.h
   60   127  1384 flyingObject.h
  103   261  2938 game.h
   48   159  1302 point.h
   63   126  1227 ship.h
  135   580  5979 uiDraw.h
  133   644  5045 uiInteract.h
   29    62   626 velocity.h
  712  2236 21618 total

~/Asteroids$ wc -lwm *.h *.cpp | grep total
 2948  9449 86747 total

# total lines of comments
~/Asteroids$ cat *.cpp *.h uiDraw/* | grep -P "^\s*(//|\*|/\*).*$" -c
858
```

