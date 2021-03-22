# ircbridge

Bidirectional IRC bridging.

# AMQP testing

Use `konsum` from `amqp-utils`

```
konsum -x ircExchange -r irc.amqp
```

or to snoop messages sent to IRC

```
konsum -x ircExchange -r amqp.irc
```

# ZRE testing

`ircbridge-zre-pretty`

or

`zrecat ircInput`
`zrecat ircOutput`
