### For something that is much further along please see: https://github.com/anmonteiro/http2af



# HTTP/2 protocol [WIP]

This is a work in progress implementation of the HTTP/2 protocol stack in OCaml.

## What's available so far?

* Parser/Serializer for HTTP/2 frames.

## What's left?

* Everything :)
	1. Priority (A simple weighted priority queue is [**available.**](https://github.com/anuragsoni/h2/blob/a7e5bc6135be308d0da5453a2cb048b0057d65b2/lib/pqueue.ml) Priority Tree needs to be implemented.
	2. Hpack
	3. HTTP/2 stream/connection state machines

## Notes

One of the goal of the implementation will be to make it easy to embed in other OCaml programs. This implementation will not handle TLS, HTTP/1 connection upgrades, etc.
