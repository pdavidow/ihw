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
1) See branch "base" for starting point.

2) Introduced Anchor such that auctioned NFT only serves one purpose. 

3) Proliferation of modules is mostly due to avoidance of cyclic dependencies.

4) Failing unit test: "1 bid at minimal bid, not registered but yes approved upfront"
For some mysterious reason, the "close" endpoint never gets called. See this in Spec.Auction.Trace
where commenting out "approve" makes it work. 
---

USAGE
cabal run spectests

To run the trace (as per note 4), swap "main"s in spec.hs