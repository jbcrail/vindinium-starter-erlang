### About

The official Erlang library for [Vindinium](http://vindinium.org), an
Artificial Intelligence programming challenge.  The goal of this library
is to provide the basic tools needed to create a bot in Erlang.

### Installation

    $ git clone https://github.com/jbcrail/vindinium-starter-erlang
    $ cd vindinium-starter-erlang
    $ make

### Usage

To start a game from the command line without accessing the API, the
syntax is:

    scripts/vindinium <secret-key> [<[training|arena]> [<bot> [<turns>]]]

For example:

    $ scripts/vindinium secretKey training fighter_bot
    $ scripts/vindinium secretKey training random_bot 30
    $ scripts/vindinium secretKey arena random_bot 100

### API Usage

To incorporate the Vindinium API in your own Erlang module, you only
need two function calls:

```erlang
% Connects to server using a key, which is a string literal
{ok, Context} = vindinium:connect(Key).

% Runs a game given the returned context
{ok, State} = vindinium:play(Context).
```

### TODO

* Improve accessor functions for game state
