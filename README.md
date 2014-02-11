## vindinium-starter-erlang

### Installation

    $ git clone https://github.com/jbcrail/vindinium-starter-erlang
    $ cd vindinium-starter-erlang
    $ make

### Usage

To start a game from the command line without accessing the API, the
syntax is:

    scripts/vindinium <secret-key> [<[training|arena]> [<turns>]]

For example:

    $ scripts/vindinium secretKey training 30
    $ scripts/vindinium secretKey arena 100

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
* Allow user to pass custom bot modules to API
