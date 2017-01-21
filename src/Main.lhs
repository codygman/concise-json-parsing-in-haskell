Ah, the beginning...

> module Main where

For convenience and ensuring the json stays valid for everyone, I'll use a json quasiquoter. To do that I have to enable QuasiQuotes:

> {-# LANGUAGE QuasiQuotes #-}

Some imports:

> import Data.Aeson.QQ
> import Data.Aeson.Lens
> import Control.Lens



print (artists ^?  nth 0)

> artists = [aesonQQ|

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

> |]

Since I made this a "Main" module main has to be defined, we'll just leave it undefined

> main :: IO ()
> main = undefined
