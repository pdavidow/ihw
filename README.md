## ORIGINAL ASSIGNMENT
Modify a linear auction contract in Plutus to only allow ‘whitelisted’ wallets to bid on the auction.
This problem is based on the code examples from week 1 of the Plutus Pioneer program:
https://github.com/input-output-hk/plutus-pioneer-program/tree/main/code/week01
Add two endpoints to the _EnglishAuction.hs_ file: `register` & `approve`. Wallets should not be allowed to bid
on the auction without having registered and been approved by the seller.
Your submission should run in the Plutus playground.

## CURRENT ASSIGNMENT
Branch off `week01-anchorPlusToken` to implement similar functionality.  Use unit tests, not playground.

## NOTES
1) Using plutus-apps tag `plutus-starter-devcontainer/v1.0.14`
2) Introduced `Anchor` such that auctioned NFT only serves one purpose. 

## USAGE
`cabal run spectests`
