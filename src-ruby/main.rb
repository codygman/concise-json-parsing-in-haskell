require 'json'

# Part 1
# Example 1
albums = JSON.parse(File.read('albums.json'))
puts albums['href'] 

# Example 2
# Get albums type
puts albums['items'][0]['album_type']

# Example 3
# get the names of all albums
list = []
for name in albums['items']
  list.push(name)
end
puts list

# Example 4
list2 = []
for item in albums['items']
  for image in item['images']
   list2.push(image['height'])
  end
end

puts list2
