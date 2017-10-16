# ETHI

Common lisp wrapper around ethereum's JSON RPC API. Interact with the ethereum blockchain from your common lisp program!

## API

The api is completely 1-to-1 with the one found [here](https://github.com/ethereum/wiki/wiki/JSON-RPC) except that it follows the following convention:

- it converts `_` inside method names with `/`
- it converts camel case method names with kebab-case.

So for example `web3_clientVersion` becomes `web3/client-version`.

If you have any doubts for a method name, just take a look at the exported symbols inside `src/ethi.lisp`. Or just paste in the REPL:

```
(let ((lst ()))
  (do-external-symbols (s (find-package 'ethi) lst)
    (push s lst))
  lst)
```

## Running tests

To run the tests locally you will need to run a local private testnet. 

1) Install `geth`
2) `cd` to the project's directory and start it with:

```
geth --rpc --nodiscover --maxpeers 0 --datadir "t/" init t/CustomGenesis.json
geth --nodiscover --maxpeers 0 --rpc --datadir t/client-data --networkid 555 --unlock "0,1" --password t/password.txt console
```

- `--rpc` enables the rpc. Duh...
- `--nodiscover` will make sure your node is not discoverable.
- `--maxpeers 0` will prevent the node from syncing.
- `init t/CustomGenesis.json` will ensure you create a custom testing blockchain. 
- `--unlock "0,1"` will unlock the first two accounts
- `--password` will use the password provided in the text file
- `console` will open the console

4) Run `(asdf:test-system :ethi)` inside your repl. I am assuming you cloned the project in a directory that `asdf` can see.

See the [official instructions](http://ethdocs.org/en/latest/network/test-networks.html#setting-up-a-local-private-testnet) for further reading on how to setup a local private testnet.
