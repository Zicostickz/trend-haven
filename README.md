# Trend Haven

A curated marketplace for trending lifestyle products powered by Stacks blockchain. This decentralized marketplace enables secure transactions between buyers and sellers while highlighting trending products based on real market activity.

## Smart Contract Infrastructure

The platform is built on four core smart contracts that work together to create a secure and transparent marketplace ecosystem:

### Marketplace Contract
The central contract that manages product listings and purchases. Key features include:
- Product listing creation and management
- Secure purchase processing
- Inventory tracking
- Platform fee handling
- Dispute resolution system

### Trend Tracker Contract
Implements algorithms to identify trending products based on:
- View counts
- Purchase frequency
- User engagement metrics
- Automated trend score calculations
- Real-time trending status updates

### Reputation Contract
Manages user reputation and trust within the marketplace:
- Transaction-based reputation scoring
- Review and rating system
- Seller verification
- Dispute impact tracking
- Progressive reputation building

### Payment Processor Contract
Handles all financial transactions with:
- Secure escrow system
- Multi-token support (STX and SIP-010 tokens)
- Automated fee processing
- Refund management
- Dispute resolution handling

## Core Features

- **Secure Transactions**: All purchases are processed through escrow for buyer and seller protection
- **Trending Products**: Real-time tracking of product popularity based on multiple metrics
- **Reputation System**: Build trust through successful transactions and reviews
- **Multi-Token Support**: Trade using STX or supported SIP-010 tokens
- **Dispute Resolution**: Built-in mechanisms for handling transaction disputes
- **Automated Fee Processing**: Transparent fee structure with automated processing
- **Curated Listings**: Quality control through seller verification and reputation requirements

## Getting Started

### For Buyers

1. Browse products through the marketplace interface
2. Create a purchase through the marketplace contract
3. Funds are held in escrow until delivery confirmation
4. Confirm delivery to release payment to seller
5. Leave reviews to help build the community

### For Sellers

1. Register as a seller through the reputation contract
2. Create product listings via the marketplace contract
3. Manage inventory and pricing
4. Ship products upon purchase
5. Receive payment after buyer confirmation
6. Build reputation through successful transactions

## Technical Documentation

### Key Functions

#### Marketplace Contract
```clarity
(create-listing (title string) (description string) (price uint) (inventory uint))
(purchase-product (listing-id uint) (quantity uint))
(confirm-delivery (purchase-id uint))
```

#### Trend Tracker Contract
```clarity
(record-product-view (product-id uint))
(record-product-purchase (product-id uint))
(get-trending-products)
```

#### Reputation Contract
```clarity
(submit-review (reviewee principal) (rating uint))
(get-reputation-score (user principal))
```

#### Payment Processor Contract
```clarity
(create-stx-payment (payment-id string) (seller principal) (amount uint))
(confirm-delivery (payment-id string))
(process-refund (payment-id string))
```

## Security Features

- Escrow-based payments
- Multi-signature requirements for disputes
- Automated fee processing
- Rate limiting on key actions
- Reputation requirements for high-value transactions
- Admin controls for emergency situations

## Platform Fees

- Standard transaction fee: 2.5%
- Fees are automatically processed and distributed
- Fee structure can be updated by platform governance

## Future Development

- Additional token support
- Enhanced trending algorithms
- Advanced dispute resolution mechanisms
- Community governance features
- Expanded reputation metrics

## Contributing

This project is built with Clarity smart contracts for the Stacks blockchain. Contributions are welcome through pull requests.

For technical questions or support, please open an issue in the repository.