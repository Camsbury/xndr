# xndr - A Priority Queue for Humans
### *Aristotle says this is cool*

This tool is for keeping track of all the stuff floating around in your
head without requiring any organizational effort on your part. Just insert
topics as they come to you, and `xndr` will let you know what is worth doing.

This project will likely turn more intelligent over time, but the basic gist
is for you to tell it which of two topics is higher priority for you,
recursively, until you achieve nirvana. But really, it doesn't need to
ask you incessantly because of that logarithmic time complexity for insertion/deletion involving trees.

## Installation
Should eventually work nicely as a nix package you can just grab off the interwebz, but for now:

- Make sure you have [nix](https://nixos.org/nix/) installed
- clone this directory
- `nix-shell` in the directory
- `cabal build`
- run the executable that is produced! (more granular details soon)

If it works
- run `nix-build` (will give details for `nix-env` and `nixos-rebuild switch`)

## API
Currently supported commands include:

- `xndr q ls` - list all queues
- `xndr q add QUEUE [DESCRIPTION]` - create a queue
- `xndr q rm QUEUE` - delete the queue
- `xndr q desc QUEUE DESCRIPTION` - append a description for the queue
- `xndr q info QUEUE` - get the description list for the queue
- `xndr q top [QUEUE]` - get the top priority item for one or all queues
- `xndr q pop QUEUE` - pop the top priority item off of the queue
- `xndr q view QUEUE` - list the contents of the queue

- `xndr t add QUEUE TOPIC [DESCRIPTION]` - insert a topic into the queue
- `xndr t rm TOPIC [QUEUE]` - delete the topic from one or all queues
- `xndr t desc TOPIC DESCRIPTION` - append a description for the topic
- `xndr t info TOPIC` - get the description list for the topic

To be supported:
- `xndr t find SEARCH_STRING [QUEUE]` - fuzzy find a topic (optionally in the queue)
