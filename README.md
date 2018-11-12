# web-galaxy
A minimalist web framework for the Racket web-server

## Installation

The library is in a **very early stage of development**, but is already used in most of my web projects and available in the Racket package repository:

```
raco pkg install web-galaxy
```

## Example

You can find a very simple blog written with `web-galaxy` in the [test folder](https://github.com/euhmeuh/web-galaxy/tree/master/web-galaxy-test/tests/web-galaxy/pony-blog).

## Features

`web-galaxy` is heavily based on the standard Racket `web-server` library.
It provides simple tools for building a web site that `web-server` does not provide but are pretty much always required in web projects:
- a **translation** system based on the Scheme standard SRFI 29
- a **date** system based on the Scheme standard SRFI 19
- a simple HTML page generator based on s-expressions
- a component system for HTML elements (called **entities**)
- a preconfigured **logging** system
- a preconfigured server so that you don't have to know every inner-workings of the Racket `web-server`, configurable step by step using parameters

The aim of this library is to stay as simple and light as possible by using Scheme standards and basic Racket libraries, but still provide an easy "all-in-one" package to start developing web sites in Racket.
