# 23_cryptocurrencies - Grassroots Cryptocurrencies

Implementation of the Grassroots Flash payment system in GLP.

## Files

### Core Module
- `gc.glp` - Main implementation with agent process, request handlers, balance management, and redemption

### Plays
- `play_mutual_credit.glp` - Two agents establish mutual credit by exchanging personal coins
- `play_payment.glp` - Three-agent scenario: Alice/Bob credit, Alice pays Carol in Bob-coins
- `play_redemption.glp` - Bob redeems Alice-coins with preference list

### Tests
- `test_balance.glp` - Unit tests for get_balance/set_balance
- `test_repayments.glp` - Unit tests for compute_repayments

## Running

```
> play_mutual_credit.glp
> play_mutual_credit.

> play_payment.glp
> play_payment.

> play_redemption.glp
> play_redemption.

> test_balance.glp
> test_balance.

> test_repayments.glp
> test_repayments.
```

## References

- Lewis-Pye, Naor, Shapiro. "Grassroots Flash: A Payment System for Grassroots Cryptocurrencies" (arXiv:2309.13191)
- Shapiro. "Grassroots Currencies: Foundations for Grassroots Digital Economies" (arXiv:2202.05619)
