## ORIGINAL ASSIGNMENT
Modify a linear auction contract in Plutus to only allow ‘whitelisted’ wallets to bid on the auction.
This problem is based on the code examples from week 1 of the Plutus Pioneer program:
https://github.com/input-output-hk/plutus-pioneer-program/tree/main/code/week01
Add two endpoints to the _EnglishAuction.hs_ file: `register` & `approve`. Wallets should not be allowed to bid
on the auction without having registered and been approved by the seller.
Your submission should run in the Plutus playground.


## CURRENT ASSIGNMENT
Create repo with similar functionality.  Use unit tests, not playground.


## USAGE
`cabal run spectests`
