# HTTP/2 protocol [WIP]

This is a work in progress implementation of the HTTP/2 protocol stack in OCaml.

## What's available so far?

* Parser/Serializer for HTTP/2 frames. [link](https://github.com/anuragsoni/h2/tree/master/frames)

## What's left?

* Everything :)
	1. Priority
	2. Hpack
	3. HTTP/2 stream/connection state machines

## Notes

One of the goal of the implementation will be to make it easy to embed in other OCaml programs. This implementation will not handle TLS, HTTP/1 connection upgrades, etc.
