# ETHI

## API

The api

## Running tests

To run the tests locally you will need to run a geth node.

1) Install `geth`
2) Start it with rpc anabled and attach to it

```
geth --rpc --maxpeers 0
geth attach
```

`--maxpeers 0` will prevent the node from syncing.

3) Create an account and unlock it.

Since we want to test all the provided functionallity we need to have an
unlocked account:

```
personal.newAccount("passphrase")
personal.unlockAccount(eth.accounts[0], "passphrase")`
```




