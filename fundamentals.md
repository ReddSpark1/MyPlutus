### Plutus Fundamentals


This page will contain quick notes I make on some key points I consider fundamental. Later on I will tidy them up into a proper page.

### Scripts

1. Every Plutus script is deterministic model and stored at a cardano address corresponding to the hash of that hard coded script. The script will still take in inputs, but if fed the same inputs it will always produce the same outputs. It is a closed system that does not have the ability to go outside of its sandboxed Plutus environment.
2. 

End users then interact with these scripts by submitting a transaction

Producing transaction has to provide the hash of script and hash of the data. Full data is optional.

Spending transaction has to provide the full script and full data

![Image](img\offchain001.jpg)

On chain code: All code that is executed by the nodes at validation time (UTXO validator script or monetary policy).

Off chain code: All code that is run by the wallet before sending the transaction. This code will construct the valid (or invalid) transaction to be send.