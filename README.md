Concise json parsing in Haskell with lenses
============================================================

First, here is what our json looks like:

```
albums
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
                ...
            ],
            "available_markets": [
                "AD",
                ...
                "UY"
            ],
            "external_urls": {
                "spotify": "https://open.spotify.com/album/1aHspryG74tpoMCOFLQGYV"
            },
            "href": "https://api.spotify.com/v1/albums/1aHspryG74tpoMCOFLQGYV",
            "id": "1aHspryG74tpoMCOFLQGYV",
            "images": [
                {
                    "height": 300,
                    "url": "https://i.scdn.co/image/52e0136ffdf197c1ea4b0eb31d652b95002e60d9",
                    "width": 300
                },
                ...
            ],
            "name": "Taste The Feeling (Avicii Vs. Conrad Sewell)",
            "type": "album",
            "uri": "spotify:album:1aHspryG74tpoMCOFLQGYV"
        }
    ],
    "limit": 5,
    "next": "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6/albums?offset=5&limit=5&album_type=single&market=ES",
    "offset": 0,
    "previous": null,
    "total": 45
}

```

I'll start off with the simplest task I can think of, getting the value of the href key in the top level json object.

First in Python:

```python
>>> import json
>>> albums = json.load(open('albums.json'))
>>> albums['href']
u'https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6/albums?offset=0&limit=5&album_type=single&market=ES'
```

Second, in Ruby:

```ruby
require 'json'
albums = JSON.parse(File.read('albums.json'))
puts albums['href']
u'https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6/albums?offset=0&limit=5&album_type=single&market=ES'
```

In Haskell:

```
λ> import qualified Data.Text.IO as T
λ> import Control.Lens
λ> import Data.Aeson.Lens
λ> albums <- T.readFile "albums.json"
λ> preview (\obj -> key "href" obj) albums
Just (String "https://api.spotify.com/v1/artists/1vCWHaC5f2uS3yhpwWbIA6/albums?offset=0&limit=5&album_type=single&market=ES")
```

From this point onwards I'll assume that you have the variable albums loaded in your Haskell (maybe Python too) repl.

Now we'll be a little more ambitious and descend into the key items, into the first item, and get it's album type.

Python:

```python
>>> albums['items'][0]['album_type']
u'single'
```

Ruby:

```puts albums['items'][0]['album_type']

Example two is: single
```

Haskell:

```haskell
λ> preview (\obj -> key "items" (nth 0 (key "album_type" obj))) albums
Just (String "single")
-- or to get rid of the 'String' in 'String "single"' we can do:
λ> preview (\obj -> key "items" (nth 0 (key "album_type" (_String obj)))) albums
Just "single"
```

Now let's do something more realistic like get the names of all the albums:

Python:
```python
# list comprehension version
>>> [item['name'] for item in albums['items']]
[u'Taste The Feeling (Avicii Vs. Conrad Sewell)', u'Pure Grinding (iSHi Remix)', u'Broken Arrows (Remixes)', u'For A Better Day (Remixes
)', u'For A Better Day (KSHMR Remix)']   

```ruby
list = []
for name in albums['items']
  list.push(name)
end
puts  list

```                                                                                              

# for loop version
lst = []
for name in albums['items']:
    lst.append(name)
```

Haskell:
```
λ> preview (\obj -> key "items" (values (key "name" (_String obj)))) albums
Just "Taste The Feeling (Avicii Vs. Conrad Sewell)"
```

Wait a second... That Haskell version is just giving us the name of the first one, what gives? It turns out that preview only gives us a single value, not a list of them. We need to use `toListOf` which takes a lens `toListOf` some thing.

Haskell:
```
λ> toListOf (\obj -> key "items" (values (key "name" (_String obj)))) albums
["Taste The Feeling (Avicii Vs. Conrad Sewell)","Pure Grinding (iSHi Remix)","Broken Arrows (Remixes)","For A Better Day (Remixes)","For A Better Day (KSHMR Remix)"]
```

Ah, that's **much** better!

In the next example, for every item's images we'll print each of their heights.

```python
>>> [image['height'] for item in albums['items'] for image in item['images']]
[640, 300, 64, 640, 300, 64, 640, 300, 64, 640, 300, 64, 640, 300, 64]

# nested for loop version
lst = []
for item in albums['items']:
    for image in item['images']:
        lst.append(image['height'])
```

```ruby
list2 = []
for item in albums['items']
  for image in item['images']
   list2.push(image['height'])
  end
end
```

```haskell
λ> toListOf (\obj -> key "items" (values (key "images" (values (key "height" (_Number obj)))))) albums
[640.0,300.0,64.0,640.0,300.0,64.0,640.0,300.0,64.0,640.0,300.0,64.0,640.0,300.0,64.0]
```

Now let's do something like we'd actually be asked to do at our jobs.

Boss: What albums are available in the HK and LU markets?!?

```python
>>> [item['name'] for item in albums['items'] if 'HK' in item['available_markets'] or 'LU' in item['available_markets']]
[u'Taste The Feeling (Avicii Vs. Conrad Sewell)', u'Pure Grinding (iSHi Remix)', u'Broken Arrows (Remixes)', u'For A Better Day (Remixes
)', u'For A Better Day (KSHMR Remix)']                                                                                                 
```

```haskell
λ> toListOf (\ item -> key "items" (values (filtered (anyOf (\ album -> key "available_markets" (values (_String album))) (\ album' -> elem album' (["HK", "LU"]))) (key "name" (_String item))))) albums
["Taste The Feeling (Avicii Vs. Conrad Sewell)","Pure Grinding (iSHi Remix)","Broken Arrows (Remixes)","For A Better Day (Remixes)","For A Better Day (KSHMR Remix)"]
```

Of course for me, this is well past the point where I usually start naming my lenses descriptively and then composing them together like below:

```haskell
λ> let items item = (key "items" (values item))
λ> let availableMarkets album = key "available_markets" (values (_String album))
λ> let isHkOrLU i = i `elem` ["HK","LU"]
λ> let albumName album = key "name" (_String album)
λ> toListOf (\album -> items (filtered (anyOf availableMarkets isHkOrLU) (albumName album))) albums
["Taste The Feeling (Avicii Vs. Conrad Sewell)","Pure Grinding (iSHi Remix)","Broken Arrows (Remixes)","For A Better Day (Remixes)","For A Better Day (KSHMR Remix)"]
```

Awesome! That is quite a bit more understandable. But wait... we didn't use any symbols. Is that good? Is that bad? Isn't lens most well known for it's heavy symbol usage?

Yes, but as I've just show you they can be totally avoided if you want. However, I belive they are more readable after you've used lenses a while and internalized examples like the above. So for the brave and interested reader:

Part 2: Lens symbols and function composition

Revisiting "I'll start off with the simplest task I can think of, getting the value of the href key in the top level json object":

I'm going to move a little quicker now that we've got basics out of the way. Let's start by converting our first example to use the symbol version of preview called `^?`.

```haskell
-- original
preview (\obj -> key "href" obj) albums

-- using symbol (notice the parens no longer necessary)
albums ^? \obj -> key "href" obj
```

Try to make sure you see what happened above before moving on, otherwise you're gonna have a bad time.

We'll also do an `eta reduction` that takes advantage of Haskell's currying. That all just means we don't have to have an explicit lambda and can count on obj to be passed in implicitly because it was already on the end of the lambda anyway.

```haskell
albums ^? key "href"
```

On the next examples I'll do these 3 transformations in line, one after another.

Revisiting "Now we'll be a little more ambitious and descend into the key items, into the first item, and get it's album type":

```haskell
-- original
preview (\obj -> key "items" (nth 0 (key "album_type" (_String obj)))) albums

-- using symbol
albums ^? \obj -> key "items" (nth 0 (key "album_type" (_String obj)))

-- using symbol and function composition
albums ^? \obj -> (key "items" . nth 0 . key "album_type" . _String) obj

-- using symbol, function composition and currying (notice parens aren't necessary anymore)
albums ^? key "items" . nth 0 . key "album_type" . _String
```


Revisiting "Now let's do something more realistic like get the names of all the albums":

Haskell:
```
-- original
toListOf (\obj -> key "items" (values (key "name" (_String obj)))) albums

-- using symbol
albums ^.. \obj -> key "items" (values (key "name" (_String obj)))

-- using symbol and function composition
albums ^.. \obj -> (key "items" . values . key "name" . _String) obj

-- using symbol, function composition, and currying
albums ^.. key "items" . values . key "name" . _String
```

Revisiting "for every item's images we'll print each of their heights":

```haskell
-- original
toListOf (\obj -> key "items" (values (key "images" (values (key "height" (_Number obj)))))) albums

-- using symbol
albums ^.. (\obj -> key "items" (values (key "images" (values (key "height" (_Number obj))))))

-- using symbol and function composition
albums ^.. \obj -> (key "items" . values . key "images" . values . key "height" . _Number) obj

-- using symbol, function composition, and currying
albums ^.. key "items" . values . key "images" . values . key "height" . _Number
```

Revisiting "Boss: What albums are available in the HK and LU markets?!?":

```haskell
-- original
toListOf (\item -> key "items" (values (filtered (anyOf (\ album -> key "available_markets" (values (_String album))) (\ album' -> elem album' (["HK", "LU"]))) (key "name" (_String item))))) albums

-- using symbol
albums ^.. \item -> key "items" (values (filtered (anyOf (\ album -> key "available_markets" (values (_String album))) (\ album' -> elem album' (["HK", "LU"]))) (key "name" (_String item))))

-- using symbol and function composition (WOW!!!)
albums ^.. \item -> (key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (`elem` ["HK", "LU"])) . key "name" . _String) item

-- using symbol, function composition, and currying
albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (`elem` ["HK", "LU"])) . key "name" . _String
```


If you are still a little lost with either the python list comprehensions, the final Haskell lens version, or both... Not to worry! I've included an ascii diagram of what is happening in the last example for both:

```
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
```

Remember the ideal version with named variables? Here is what it looks like after taking advantage of symbols, function composition, and currying:

```haskell
λ> let items= key "items" . values
λ> let availableMarkets = key "available_markets" . values . _String
λ> let isHkOrLU = (`elem` ["HK","LU"])
λ> let albumName = key "name" . _String
λ> albums ^.. items . filtered (anyOf availableMarkets isHkOrLU) . albumName
["Taste The Feeling (Avicii Vs. Conrad Sewell)","Pure Grinding (iSHi Remix)","Broken Arrows (Remixes)","For A Better Day (Remixes)","For A Better Day (KSHMR Remix)"]

```

We can code golf the one-liner version a tiny bit, but it was already almost as small as possible:

```haskell
-- original
albums ^.. key "items" . values . filtered (anyOf (key "available_markets" . values . _String) (`elem` ["HK", "LU"])) . key "name" . _String

-- replace key with more general ix (caution: ix being more general means you could have ambiguous types and have to resort to manually annotating them)
albums ^.. ix "items" . values . filtered (anyOf (ix "available_markets" . values . _String) (`elem` ["HK", "LU"])) . ix "name" . _String
```
