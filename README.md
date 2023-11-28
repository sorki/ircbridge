# ircbridge

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/sorki/ircbridge/ci.yaml?branch=master)](https://github.com/sorki/ircbridge/actions/workflows/ci.yaml)

Type-safe swiss army knife for bi-directional IRC bridging.

# Usage

Preferred backend is AMQP using `rabbitmq`. See [./nixos/example.nix](./nixos/example.nix) for
an example NixOS deployment. The ZRE backend, while fully functional is a bit less
reliable and considered experimental. A simple UNIX socket backend
might be added in future.

## Sending messages to IRC

Use `ircbridge-amqp-cat` to send a message to IRC channel or user:

```sh
ircbridge-amqp-cat --chan "#bottest" "I'm a bot"
ircbridge-amqp-cat --chan "#bottest" --notice "Sent as a /notice"
ircbridge-amqp-cat --user "@srk" Hi
```

## Receiving messages from IRC

Use `ircbridge-amqp-tail` to print messages received by a bot:

## IRCCat compatible

`ircbridge-amqp-irccat` offers
[irccat](https://github.com/irccloud/irccat) compatible layer.

### `ircbridge-amqp-irccat`

allows to pipe target(s) and message to its stdin, for example:

```sh
echo "#bottest Helo" | ircbridge-amqp-irccat
echo "@user Hi" | ircbridge-amqp-irccat
```

Multiple targets are also supported

```sh
echo "#bottest,@user,#chan2 Hi all" | ircbridge-amqp-irccat
```

It is also possible to pass the default target via `--chan` or `--user`
arugments:

```sh
echo "Send to default target" | ircbridge-amqp-irccat --chan "#bottest"
```

To send message as a notice, use `--notice` flag

```sh
echo "#bottest Helo" | ircbridge-amqp-irccat --notice
```

The topic operations are not supported.

### `ircbridge-amqp-irccat-tcpserver`

Similar to `ircbridge-amqp-irccat` but running as a TCP server
on localhost and port `12345` (defaults, can be overriden using
`--host` and `--port`).

For `-tcpserver`, the default target must be supplied beforehand:

```sh
ircbridge-amqp-irccat-tcpserver --chan "#bottest"
```

Now you can send messages using e.g. `netcat`:

```
echo "Ahoy" | nc localhost 12345
echo "@user Hey" | nc localhost 12345
echo "#anotherChannel Non-default chan" | nc localhost 12345
echo "#chan,@user Mutliple recepients" | nc localhost 12345
```

### ZRE backend

Uses [`haskell-zre`](https://github.com/sorki/haskell-zre). Considered experimental.

#### Cat

Send messages using `ircbridge-zre-cat`, usage is similar to `ircbridge-amqp-cat`

#### Tail

Dump messages received from IRC over ZRE:

`ircbridge-zre-tail`

or using `zrecat`

```
zrecat ircInput
# another terminal
zrecat ircOutput
```

## Raw `AMQP`

You can use `konsum` from `amqp-utils` (`nix-shell -p haskellPackages.amqp-utils`)
to inspect the wire format of the AMQP transport:

Show messages received from IRC

```
konsum -x ircExchange -r irc.amqp
```

or snoop messages sent to IRC sent by other clients

```
konsum -x ircExchange -r amqp.irc
```

## Why there are so many packages?

Sort of an experiment in extreme modularity which allows reusing
code without pulling in dependencies not required for certain frontends/backends.
For example, `amqp` doesn't require `zre/zeromq`, nor `cereal` dependencies and
`zre` doesn't require `amqp`, but uses `cereal` for serialization.

The separation of concerns is also quite nice, for example `ircbridge-ircbot-*` subpackages
use their own set of command line arguments related to IRC network, user information, channels
to join while clients like `ircbridge-amqp-cat` only need to know about a target where to
send messages (channel or user).

## Related projects

- [ircbot](https://github.com/stepcut/ircbot/) used as an IRC frontend
- [xnand](https://github.com/sorki/xnand/) a bot using `ircbridge` as its backend (over AMQP)
- [irccat](https://github.com/irccloud/irccat) inspiration for `ircbridge-amqp-irccat`

