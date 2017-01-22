{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Aeson.QQ
import Data.Aeson.Lens
import Control.Lens

-- silly combinators to make writing this easier
first desc a = putStrLn desc >> a >> putStrLn ""
next desc a = putStrLn desc >> a >> putStrLn ""

main :: IO ()
main = do
  first "let's start by getting the href value at the top level of the json" $ do
    print $ preview (key "href") albums

  putStrLn "Notice you **must** use preview rather than view"

  next "we'll be a little more ambitious and descend into the items, to the first item, and get its album type" $ do

    putStrLn "first we'll write it explicitly with lambdas and parens"
    print $ preview (\a -> key "items" (nth 1 (key "album_type" a))) albums

    putStrLn "next I'll show you how it looks with function composition and try to win you over to the pointfree way"
    print $ preview (key "items" . nth 1 . key "album_type") albums
    {-
     The equivalent of the above in python is:
     albums.get(items)[0].get("album_type")
    -}
    putStrLn "to get closer to python in number of characters I'll use the oh so controversial lens symbols ;)"
    print $ albums ^? key "items" . nth 1 . key "album_type"

  -- TODO explain the newly introduced "values"
  next "we'll do something more realistic, like get the names of all the albums" $ do
    print $ albums ^.. key "items" . values . key "album_type"

  next "Yuck, why were all those values prepended with String? Let's take care of that" $ do
    print $ albums ^.. key "items" . values . key "album_type" . _String

  next "Let's be ridiculous... We'll get each image height in all of a given albums images" $ do
    print $ albums ^.. key "items" . values . key "images" . values . key "height"

  next "We can get rid of the Number prepended nearly the same as earlier, hopefully you are seeing the pattern :)" $ do
    print $ albums ^.. key "items" . values . key "images" . values . key "height" . _Number

{-
The equivalent python to the last example above being:
lst = []
for item in albums['items']:
    for image in item['images']:
        lst.append(image['height'])

My python is rusty, so someone friendly pythonista gave me the equivalent list comprehension:

[image['height'] for item in albums['items'] for image in item['images']]

My best WRONG guess was:

[image['height'] for image in item['images'] for item in albums['items']]

comparing them next to eachother:

[image['height'] for item in albums['items'] for image in item['images']]
albums ^.. key "items" . values . key "images" . values . key "height" . _Number

The python reads a little easier in a way I think, but you can see how I messed up the order of the list comprehension.

The Haskell is a little *different*, but the pattern is clear once you understand it. IMO after being shown what my error was I still think the strategy this lens approach is using of 'descend into key with key "somekey", operate on all its values by composing values next, repeat' is simpler cognitively speaking.

Of course we also have type safety in this example. If the height values are heterogenous for some reason, they'll just not be added to the resulting list.

-}

  -- next "We can do something useful now, like only show albums available in the HK and LU markets" $ do
    print $ albums ^.. key "items" . values . filtered (\item -> any (\i -> i == "HK" || i == "LU") (item ^.. key "available_markets" . values . _String)) . key "name" . _String

    print $ albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (\i -> i == "HK" || i == "LU")) . key "name" . _String

   -- whoops we were re-implementing the elem function
    print $ albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (`elem` ["HK", "LU"])) . key "name" . _String

    -- I personally find it kind of confusing, but here is the version using toListOf rather than ^..
    print $ toListOf (key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (`elem` ["HK", "LU"])) . key "name" . _String) (albums)

  {-
An analysis of the above:

First Python. I've annotated which part of the for the individual accessors correlate to, because it was most confusing for me. I'm not sure if this is the case in general though.

[ image['height'] for item in albums['items'] for image in item['images'] if item["available_markets"] in ["HK","LU"] ]
  \___________/       \__/        \________/      \___/    \____________/ \_________________________________________/
    ___|____        ___|______    ____|____     ____|____      ____|____                      ____|____
    |Step #6|       | Step #2|    |Step #1|     |Step #5|      |Step #3|                     |Step #4|
    *********       *********     *********     *********      *********                     *********

Next Haskell:

albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (`elem` ["HK", "LU"])) . key "name" . _String
        |    \  /          \       \___\______|___\_____________________\_|_____|_________|__/  \                   | \                     \
       /      \/            \__________ \     |    \                     \|     |         /\     \__________________/  \_____________________\
   1. toListOf \                       | \    |     \_____________________|     \        /  \           |                      |
               |                       |  \___|          \                 \     \      /    \_______   |            6. Then finally with the resulting filtered items values, give us the name of each filtered items values
               > 2. using key "items"  |     \            \                /\     \    |             \  |
                              _________/ 4. filter any of key available markets values that are String  |
                              |                                                                    ____/ \___________________________________________
                    3. and all of key "items" values                                              /                                                  \
                                                                                               5. with the condition those values are anyOf ("HK","LU")

TODO incorporate feedback from #haskell-beginners
[19:04] <<redacted>> codygman: Lenses aren't inherently left to right
[19:05] <codygman> <redacted>: Can you give me an example, I'll have to include that. I think the intuition works for the subset of "parsing json with lenses" tasks I've been doing thus far, but I don't want to mislead others and harm their larger understanding if they decide to attain it.
[19:06] <codygman> tmciver: I think the lines crossing issue will go away when I convert to graphic form
[19:06] <<redacted>> codygman: You data structure just happens to be nested; which makes your lens composition with (.) feel natural.
[19:09] <codygman> <redacted>: Okay, I think I can get away with "When working with nested structures such as how json tends to be, lens composition will usually feel natural and be left to right."
[19:10] <<redacted>> It's better just not to mention it altogether not to confuse them.
[19:10] <codygman> Gurkenglas: FWIW, when you explained your joke I found it hilarious
[19:14] <codygman> <redacted>: I'm not sure I agree, but to be honest I'll have to think on that one.
  -}


  -- the python one-liner of the above haskell/lens one-liner is:
  -- [item['name'] for item in albums['items'] if "HK" in item['available_markets'] or "LU" in item['available_markets']]
  -- albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (\i -> i == "HK" || i == "LU")) . key "name" . _String
  -- albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (\i -> any (i ==) ["HK", "LU"])) . key "name" . _String
  -- albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (flip any ["HK", "LU"] . (==))) . key "name" . _String
  -- albums ^.. ix "items" . values . filtered (anyOf (ix "available_markets" . values . _String) (flip any ["HK", "LU"] . (==))) . ix "name" . _String
  -- albums ^.. ix "items" . values . filtered (anyOf (ix "available_markets" . values . _String) (flip any ["HK", "LU"] . (==))) . ix "name" . _String
  -- albums ^.. ix "items" . values . filtered (anyOf (ix "available_markets" . values . _String) ((`any` ["HK", "LU"]) . (==))) . ix "name" . _String
  -- albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (`elem` ["HK", "LU"])) . key "name" . _String

  -- of course I would have advised taking the above one step at a time unless this was a one-off thing in the repl
  let items' = key "items" . values
  let availableMarkets = key "available_markets" . values . _String
  let isHkOrLU i = i == "HK" || i == "LU"
  let albumName = key "name" . _String
  print $ albums ^.. items' . filtered (anyOf availableMarkets isHkOrLU) . albumName

  -- as with any language, naming functions well and then creating bigger functions out of them makes a *huge* difference
  -- it is nice that you can just chain everything together as a one-liner in a pinch though imo


  putStrLn "we are at the end!"

-- λ> albums ^.. key "items" . each . key "album_type" 

-- <interactive>:700:26-29: error:
--     • No instance for (Each
--                          aeson-0.11.2.1:Data.Aeson.Types.Internal.Value
--                          aeson-0.11.2.1:Data.Aeson.Types.Internal.Value
--                          b0
--                          b0)
--         arising from a use of ‘each’
--     • In the first argument of ‘(.)’, namely ‘each’
--       In the second argument of ‘(.)’, namely ‘each . key "album_type"’
--       In the second argument of ‘(^..)’, namely
--         ‘key "items" . each . key "album_type"’

-- <interactive>:700:33-48: error:
--     • Ambiguous type variable ‘b0’ arising from a use of ‘key’
--       prevents the constraint ‘(AsValue b0)’ from being solved.
--       Probable fix: use a type annotation to specify what ‘b0’ should be.
--       These potential instances exist:
--         instance AsValue String -- Defined in ‘Data.Aeson.Lens’
--         ...plus five instances involving out-of-scope types
--         (use -fprint-potential-instances to see them all)
--     • In the second argument of ‘(.)’, namely ‘key "album_type"’
--       In the second argument of ‘(.)’, namely ‘each . key "album_type"’
--       In the second argument of ‘(^..)’, namely
--         ‘key "items" . each . key "album_type"’


albums = [aesonQQ|
{

    "href": "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6/albums?offset=0&limit=5&album_type=single&market=ES",
    "items": [
        {
            "album_type": "single",
            "artists": [
                {
                    "external_urls": {
                        "spotify": "https://open.spotify.com/artist/1vCWHaC5f2uS3yhpwWbIA6"
                    },
                    "href": "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6",
                    "id": "1vCWHaC5f2uS3yhpwWbIA6",
                    "name": "Avicii",
                    "type": "artist",
                    "uri": "spotify:artist:1vCWHaC5f2uS3yhpwWbIA6"
                },
                {
                    "external_urls": {
                        "spotify": "https://open.spotify.com/artist/1rw8ZTLnDHd74TWDDukjVi"
                    },
                    "href": "https://api.spotify.com/v1/artists/1rw8ZTLnDHd74TWDDukjVi",
                    "id": "1rw8ZTLnDHd74TWDDukjVi",
                    "name": "Conrad Sewell",
                    "type": "artist",
                    "uri": "spotify:artist:1rw8ZTLnDHd74TWDDukjVi"
                }
            ],
            "available_markets": [
                "AD",
                "AR",
                "AT",
                "AU",
                "BE",
                "BG",
                "BO",
                "BR",
                "CH",
                "CL",
                "CO",
                "CR",
                "CY",
                "CZ",
                "DE",
                "DK",
                "DO",
                "EC",
                "EE",
                "ES",
                "FI",
                "FR",
                "GB",
                "GR",
                "GT",
                "HK",
                "HN",
                "HU",
                "ID",
                "IE",
                "IS",
                "IT",
                "JP",
                "LI",
                "LT",
                "LU",
                "LV",
                "MC",
                "MT",
                "MY",
                "NI",
                "NL",
                "NO",
                "NZ",
                "PA",
                "PE",
                "PH",
                "PL",
                "PT",
                "PY",
                "SE",
                "SG",
                "SK",
                "SV",
                "TR",
                "TW",
                "UY"
            ],
            "external_urls": {
                "spotify": "https://open.spotify.com/album/1aHspryG74tpoMCOFLQGYV"
            },
            "href": "https://api.spotify.com/v1/albums/1aHspryG74tpoMCOFLQGYV",
            "id": "1aHspryG74tpoMCOFLQGYV",
            "images": [
                {
                    "height": 640,
                    "url": "https://i.scdn.co/image/b7a4395d5d49c9f8b5fa917f927629577d78ca9d",
                    "width": 640
                },
                {
                    "height": 300,
                    "url": "https://i.scdn.co/image/52e0136ffdf197c1ea4b0eb31d652b95002e60d9",
                    "width": 300
                },
                {
                    "height": 64,
                    "url": "https://i.scdn.co/image/112a41faf668770024740a1dfd0b920221554c5b",
                    "width": 64
                }
            ],
            "name": "Taste The Feeling (Avicii Vs. Conrad Sewell)",
            "type": "album",
            "uri": "spotify:album:1aHspryG74tpoMCOFLQGYV"
        },
        {
            "album_type": "single",
            "artists": [
                {
                    "external_urls": {
                        "spotify": "https://open.spotify.com/artist/1vCWHaC5f2uS3yhpwWbIA6"
                    },
                    "href": "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6",
                    "id": "1vCWHaC5f2uS3yhpwWbIA6",
                    "name": "Avicii",
                    "type": "artist",
                    "uri": "spotify:artist:1vCWHaC5f2uS3yhpwWbIA6"
                }
            ],
            "available_markets": [
                "AD",
                "AR",
                "AT",
                "AU",
                "BE",
                "BG",
                "BO",
                "BR",
                "CH",
                "CL",
                "CO",
                "CR",
                "CY",
                "CZ",
                "DE",
                "DK",
                "DO",
                "EC",
                "EE",
                "ES",
                "FI",
                "GB",
                "GR",
                "GT",
                "HK",
                "HN",
                "HU",
                "ID",
                "IE",
                "IS",
                "IT",
                "JP",
                "LI",
                "LT",
                "LU",
                "LV",
                "MC",
                "MT",
                "MY",
                "NI",
                "NL",
                "NO",
                "NZ",
                "PA",
                "PE",
                "PH",
                "PL",
                "PT",
                "PY",
                "SE",
                "SG",
                "SK",
                "SV",
                "TR",
                "TW",
                "UY"
            ],
            "external_urls": {
                "spotify": "https://open.spotify.com/album/1lhkoUf2SJxdrREtP72HV9"
            },
            "href": "https://api.spotify.com/v1/albums/1lhkoUf2SJxdrREtP72HV9",
            "id": "1lhkoUf2SJxdrREtP72HV9",
            "images": [
                {
                    "height": 640,
                    "url": "https://i.scdn.co/image/78abe3d06d972e1654ae69d646d3cd0b4d4217de",
                    "width": 640
                },
                {
                    "height": 300,
                    "url": "https://i.scdn.co/image/4bae64b49815c2206848a4f858ff1a1eadcd4e32",
                    "width": 300
                },
                {
                    "height": 64,
                    "url": "https://i.scdn.co/image/53ce2336ecf81eb36480b3d7313eb8cc1f2346ce",
                    "width": 64
                }
            ],
            "name": "Pure Grinding (iSHi Remix)",
            "type": "album",
            "uri": "spotify:album:1lhkoUf2SJxdrREtP72HV9"
        },
        {
            "album_type": "single",
            "artists": [
                {
                    "external_urls": {
                        "spotify": "https://open.spotify.com/artist/1vCWHaC5f2uS3yhpwWbIA6"
                    },
                    "href": "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6",
                    "id": "1vCWHaC5f2uS3yhpwWbIA6",
                    "name": "Avicii",
                    "type": "artist",
                    "uri": "spotify:artist:1vCWHaC5f2uS3yhpwWbIA6"
                }
            ],
            "available_markets": [
                "AD",
                "AR",
                "AT",
                "AU",
                "BE",
                "BG",
                "BO",
                "BR",
                "CH",
                "CL",
                "CO",
                "CR",
                "CY",
                "CZ",
                "DE",
                "DK",
                "DO",
                "EC",
                "EE",
                "ES",
                "FI",
                "FR",
                "GB",
                "GR",
                "GT",
                "HK",
                "HN",
                "HU",
                "ID",
                "IE",
                "IS",
                "IT",
                "JP",
                "LI",
                "LT",
                "LU",
                "LV",
                "MC",
                "MT",
                "MY",
                "NI",
                "NL",
                "NO",
                "NZ",
                "PA",
                "PE",
                "PH",
                "PL",
                "PT",
                "PY",
                "SE",
                "SG",
                "SK",
                "SV",
                "TR",
                "TW",
                "UY"
            ],
            "external_urls": {
                "spotify": "https://open.spotify.com/album/4nCNj68SZym6hNxXDkRtjN"
            },
            "href": "https://api.spotify.com/v1/albums/4nCNj68SZym6hNxXDkRtjN",
            "id": "4nCNj68SZym6hNxXDkRtjN",
            "images": [
                {
                    "height": 640,
                    "url": "https://i.scdn.co/image/c735be011394f4e7cdf1ebbf95d112cb69fd3414",
                    "width": 640
                },
                {
                    "height": 300,
                    "url": "https://i.scdn.co/image/7f4221fda86e4daa539fd29233fadad039cc46d9",
                    "width": 300
                },
                {
                    "height": 64,
                    "url": "https://i.scdn.co/image/e1930bf1293d89799a0e382b40ebad5455b11857",
                    "width": 64
                }
            ],
            "name": "Broken Arrows (Remixes)",
            "type": "album",
            "uri": "spotify:album:4nCNj68SZym6hNxXDkRtjN"
        },
        {
            "album_type": "single",
            "artists": [
                {
                    "external_urls": {
                        "spotify": "https://open.spotify.com/artist/1vCWHaC5f2uS3yhpwWbIA6"
                    },
                    "href": "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6",
                    "id": "1vCWHaC5f2uS3yhpwWbIA6",
                    "name": "Avicii",
                    "type": "artist",
                    "uri": "spotify:artist:1vCWHaC5f2uS3yhpwWbIA6"
                }
            ],
            "available_markets": [
                "AD",
                "AR",
                "AT",
                "AU",
                "BE",
                "BG",
                "BO",
                "BR",
                "CH",
                "CL",
                "CO",
                "CR",
                "CY",
                "CZ",
                "DE",
                "DK",
                "DO",
                "EC",
                "EE",
                "ES",
                "FI",
                "FR",
                "GB",
                "GR",
                "GT",
                "HK",
                "HN",
                "HU",
                "ID",
                "IE",
                "IS",
                "IT",
                "JP",
                "LI",
                "LT",
                "LU",
                "LV",
                "MC",
                "MT",
                "MY",
                "NI",
                "NL",
                "NO",
                "NZ",
                "PA",
                "PE",
                "PH",
                "PL",
                "PT",
                "PY",
                "SE",
                "SG",
                "SK",
                "SV",
                "TR",
                "TW",
                "UY"
            ],
            "external_urls": {
                "spotify": "https://open.spotify.com/album/0G1HMRmXtK9LZAa4TijYLv"
            },
            "href": "https://api.spotify.com/v1/albums/0G1HMRmXtK9LZAa4TijYLv",
            "id": "0G1HMRmXtK9LZAa4TijYLv",
            "images": [
                {
                    "height": 640,
                    "url": "https://i.scdn.co/image/8202f8de7f1b57e676a2588b76beeefb9c3ff401",
                    "width": 640
                },
                {
                    "height": 300,
                    "url": "https://i.scdn.co/image/1eb44a79643485d3344f856b2a5d337bb62e5410",
                    "width": 300
                },
                {
                    "height": 64,
                    "url": "https://i.scdn.co/image/e42b67ce5361bbe87eb9b4a39b8a59b5ef1746a5",
                    "width": 64
                }
            ],
            "name": "For A Better Day (Remixes)",
            "type": "album",
            "uri": "spotify:album:0G1HMRmXtK9LZAa4TijYLv"
        },
        {
            "album_type": "single",
            "artists": [
                {
                    "external_urls": {
                        "spotify": "https://open.spotify.com/artist/1vCWHaC5f2uS3yhpwWbIA6"
                    },
                    "href": "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6",
                    "id": "1vCWHaC5f2uS3yhpwWbIA6",
                    "name": "Avicii",
                    "type": "artist",
                    "uri": "spotify:artist:1vCWHaC5f2uS3yhpwWbIA6"
                }
            ],
            "available_markets": [
                "AD",
                "AR",
                "AT",
                "AU",
                "BE",
                "BG",
                "BO",
                "BR",
                "CH",
                "CL",
                "CO",
                "CR",
                "CY",
                "CZ",
                "DE",
                "DK",
                "DO",
                "EC",
                "EE",
                "ES",
                "FI",
                "FR",
                "GB",
                "GR",
                "GT",
                "HK",
                "HN",
                "HU",
                "ID",
                "IE",
                "IS",
                "IT",
                "JP",
                "LI",
                "LT",
                "LU",
                "LV",
                "MC",
                "MT",
                "MY",
                "NI",
                "NL",
                "NO",
                "NZ",
                "PA",
                "PE",
                "PH",
                "PL",
                "PT",
                "PY",
                "SE",
                "SG",
                "SK",
                "SV",
                "TR",
                "TW",
                "UY"
            ],
            "external_urls": {
                "spotify": "https://open.spotify.com/album/1oRvqWh2IkWMDF90jDEgzz"
            },
            "href": "https://api.spotify.com/v1/albums/1oRvqWh2IkWMDF90jDEgzz",
            "id": "1oRvqWh2IkWMDF90jDEgzz",
            "images": [
                {
                    "height": 640,
                    "url": "https://i.scdn.co/image/2626556c707ad974cdf465d560375c3bdc7342fd",
                    "width": 640
                },
                {
                    "height": 300,
                    "url": "https://i.scdn.co/image/8878dee1c0f5c505a3c33ae184649153e2e06d63",
                    "width": 300
                },
                {
                    "height": 64,
                    "url": "https://i.scdn.co/image/b0ce4aaf8ce7ecd053df0ef57c28adec1fb7bf34",
                    "width": 64
                }
            ],
            "name": "For A Better Day (KSHMR Remix)",
            "type": "album",
            "uri": "spotify:album:1oRvqWh2IkWMDF90jDEgzz"
        }
    ],
    "limit": 5,
    "next": "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6/albums?offset=5&limit=5&album_type=single&market=ES",
    "offset": 0,
    "previous": null,
    "total": 45

}
|]


albums2 = [aesonQQ| {"items": [{"album_type": "single"}]} |]

simpleList = [aesonQQ| {"numbers": [1,2,3]}|]

