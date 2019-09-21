# Mockchain

This package contains a library (`mockchain`) that defines the core types for a mock blockchain API, and an executable (also `mockchain`) that runs a simple `servant`-based HTTP server that exposes this API.

## Running

First, get the code:

```console
$ git clone https://github.com/duairc/mockchain
$ cd mockchain
```

To build, you can run:

```console
$ cabal v2-build
```

If you just want to run the HTTP server:

```console
$ cabal v2-run exe:mockchain
```

By default, this will run on port `8888`. To run on a different port, set the `PORT` environment variable, e.g.:

```console
$ PORT=5000 cabal v2-run exe:mockchain
```

Additional options are available also. To see these, run with `--help`:

```console
$ cabal v2-run exe:mockchain -- --help
```

## Installing

To install the executable, run:

```console
$ cabal v2-install exe:mockchain
```

Thereafter you can simply run the command `mockchain` in place of `cabal v2-run exe:mockchain [--]` in the above commands.

## Examples

### Broadcast

Assuming you have the `uuid` executable in your `$PATH` (which, on my system, comes from the `libossp_uuid` package), you can broadcast a transaction with `curl` using:

```console
$ printf '{"from": "%s", "to": "%s", "amount": 20, "fee": 5}' "$(uuid -v4)" "$(uuid -v4)" | curl -X POST -H "Content-Type: application/json" -d @- http://localhost:8888/
"01234567-89ab-cdef-0123-456789abcdef"
```

This broadcasts a new transaction from and to random addresses given `uuid` executable, with an `amount` of 20 units and a `fee` of 5 units.

If it's a valid transaction, you'll receive a TXID (Transaction ID), which looks like a UUID, encoded as a JSON string in response.

An invalid transaction might receive a response like this:

```console
$ printf '{"from": "%s", "to": "%s", "amount": -20, "fee": 5}' "$(uuid -v4)" "$(uuid -v4)" | curl -X POST -H "Content-Type: application/json" -d @- http://localhost:8888/
{"errors":["NonPositiveAmount","FeeNotLessThanAmount"]}
```

### Get

To check the status of your transaction, simply send a GET request to the TXID you were given:

```console
$ curl -s http://localhost:8888/01234567-89ab-cdef-0123-456789abcdef | jq .status
"Accepted"
```

A transaction's status can be either `Pending`, `Accepted` or `Rejected`. The criteria for acceptance are set in the command-line arguments of the `mockchain` executable. It can take up to 10 seconds for a transaction to be accepted or rejected.

## Testing

You can run tests with `cabal`:

```console
$ cabal v2-test --enable-tests
```
