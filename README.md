## vindinium-starter-erlang

### Installing

    $ git clone https://github.com/jbcrail/vindinium-starter-erlang
    $ cd vindinium-starter-erlang
    $ make

### Sample client

```erlang
% Connects to server using a key, which is a string literal
{ok, Context} = vindinium:connect(Key).

% Runs a game given the returned context
{ok, State} = vindinium:play(Context).
```
