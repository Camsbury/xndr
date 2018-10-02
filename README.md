# xndr - A Priority Queue for Humans
### *Aristotle told me it was a good idea*

This tool is for keeping track of all the stuff floating around in your
head without requiring any organizational effort on your part. Just insert
topics as they come to you, and `xndr` will let you know what is worth doing.

This project will likely turn more intelligent over time, but the basic gist
is for you to tell it which of two topics is higher priority for you,
recursively, until you achieve nirvana. But really, it doesn't need to
ask you incessantly because of that logarithmic time complexity for insertion/deletion involving trees.

## API
Currently supported commands include:

- `xndr top` - get the top priority item
- `xndr pop` - pop the top priority item off of the queue
- `xndr list` - list the contents of the queue
- `xndr insert TOPIC`- insert the topic into the queue
- `xndr delete TOPIC`- delete the topic from the queue

Soon to support:

- `xndr describe TOPIC DESCRIPTION` append a description for the topic
- `xndr info TOPIC`- get the description list for the topic
- `xndr find TOPIC`- fuzzy find a topic in the queue
