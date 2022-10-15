NOTE - The frontend is hosted at - https://capable-begonia-127106.netlify.app/ , to mint an nft, you need to have [this](https://devratroom.blogspot.com/p/cross-domain-cors-extension.html) extension installed. You can still test the main functionality (energize, withdraw) without the extension though.

# energize

Energize is a protocol that lets you put digital assets inside your NFTs. This protocol allows users to deposit FA1.2 or FA2 tokens (ANY tokens) into an NFT. Energize enables the creation of yield bearing DeFi NFTs (through a bridge to Yupana), time locked capsules, in game assets, and so much more.
The platform also has a minting station and a marketplace.

# How it works 

Every NFT that is energized is given its own smart wallet.

When a user mints an NFT, they can pick an asset (currently kUSD) they want to energize the NFT with. 
They will then deposit the asset into the nft using the interface, which sends the asset into the smart wallet.
The smart wallet is then responsible for depositing the asset tokens into the Yupana lending pool, and receiving back yTokens.

When withdrawing from the NFT Smart Wallet (which can be done by the owner of the nft only), the Yield Tokens are redeemed for the underlying Asset Tokens from Yupana. The Smart Wallet is responsible for handling both the Principal and Interest. 

#  Smart Contract Architecture

![Architecture](https://i.imgur.com/5ip4S9g.png)

# Smart Contracts 

The contracts can be found on the ghostnet at these addresses - 

Energize Contract - KT1RDB7Q8jKgQnarhDaHJx9xUyakemMnUk73

kUSD Wallet Manager - KT1Wgp6qSsDN7mCaDk5XDEQU52MezE8B9mr5

Example Smart Wallet With yKUSD tokens invested - KT1JqTPhNcaJaakNBQVHEFbgCmo7mGKsx61H

Energize Contract - 
  This is the only contract that is accessible to the client. It has the following entrypoints 
  - addWalletManager: admin only - used to add token assets that an nft can be energized with
  - energize: if a smart wallet for the nft exists, this transfers the asset amount to it. If a smart wallet doesn't exist, it forwards the call to the wallet manager. The wallet manager is a contract factory which originates the contract for the smart wallet and transfers the asset tokens to it.
  - energizeWithInterest - same as energize entrypoint with the added functionality of investing tokens to yupana for bearing interest
  - withdrawFa12 - allows the owner of the nft to withdraw the asset token from the nft.

# Use Cases

• 'Virtual Geocaching. Treasure hunts for hidden NFTs in the Metaverse! When
you find it you get to discharge the accrued interest and leave your name. The NFT stays for the next person to find... the longer it takes, the more accumulated interest (charge) to gain!

• Transfer collections of tokens in a single transaction (FA2 Transfer). Fill up your NFT with a basket of other tokens. This basket is easily transferable, could be used as a trust as the contents can be timelocked.

• Nest and time-lock tokens in an NFT for a vesting period. This NFT can be immediately traded or collateralized without the tokens hitting the market

• Transfer a multitude of assets in a single transaction.

• NFTs for Peer-to-Peer OTC trades. A single Escrow NFT can exchange
multiple tokens between parties in one trustless transaction.

• An Index Fund can be created by Energizing an NFT with a basket of digital
assets, and then fractionalizing it.

• Ready to exit crypto? Sell your portfolio in one stroke!

• Transfer a multitude of assets in a single transaction.

• A single DAO Capsule NFT can act as:
a. A membership/governance token for access and voting
b. A vault to time-lock tokens of equity
c. A mailbox/bank account to receive payments/dividends/rewards/NFTs

• Combine Harvesting': farm multiple protocols with a single NFT

• Turn your favorite NFT into a Goal-Based interest earning Piggy bank. The accumulating interest inside of your NFT changes the image of the artwork as time progresses. Visually seeing your goal-based savings account transforming closer and closer to that goal . The NFT image would change based on the amount of charge and helps people visualize what they’re saving for, e.g. the NFT’s image could be a gray car that turns into a colored-in one as you get
closer to your goal.

• Music NFT: A Meta “Album” NFT filled with (time-locked) individual songs
NFTs that have customizable release conditions

• Token-Gated Access Newsletters / Chats — Incorporating the accumulated interest as NFT has — incorporating this ‘proof of length of NFT ownership’ mechanic quite beautifully.

• Charity-Focused Art Sales. Portion of interest-generated always pays out to charity (or any tez address).

• Patreon-Style Donations. Fans can deposit tokens into an artist’s NFT. The artist’s NFT (pre-sale) can be accruing donations before being sold in the open market.


