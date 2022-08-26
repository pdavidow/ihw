ORIGINAL ASSIGNMENT
Modify a linear auction contract in Plutus to only allow ‘whitelisted’ wallets to bid on the auction.
This problem is based on the code examples from week 1 of the Plutus Pioneer program:
https://github.com/input-output-hk/plutus-pioneer-program/tree/main/code/week01
Add two endpoints to the EnglishAuction.hs file: ‘register’ & ‘approve’. Wallets should not be allowed to bid
on the auction without having registered and been approved by the seller.
Your submission should run in the Plutus playground.
---

CURRENT ASSIGNMENT
Create repo with similar functionality.  Use unit tests, not playground.
---

NOTES
0) Using plutus-apps tag: plutus-starter-devcontainer/v1.0.14

1) See branch "week01-anchorPlusToken" for starting point.

2) Introduced Anchor such that auctioned NFT only serves one purpose. 

3) Proliferation of modules is mostly due to avoidance of cyclic dependencies.

4) Error handling is not robust, giving inconsistent results. For example, unit test "2 bids: First lower than min, second at min, yes approved upfront" works, but commented-out test "1 bid at min, not registered but yes approved upfront" simply stops (using same "throwError") and never gets to close (shown in Spec.Auction.Trace.test1).
---

USAGE
cabal run spectests
