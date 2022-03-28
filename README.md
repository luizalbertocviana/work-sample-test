# Work Sample Test
## Backend Code

The prices are wrong!

Oh no! A team of accountants at a huge company have discovered that customers are
occasionally being charged too much or too little for various products.  They've
written you an angry email about it, and now you're tasked with coming up with
a data report about the situation.

They've sent you a directory containing the receipts in which they suspect
prices are wrong and a CSV file containing the product codes and the correct
price each one should have.

A receipt is just a plaintext file with rows formatted like:
- A product name
- A product code
- A price
- A random flag character (this exists for no other reason than to confuse you)

There's also a store number listed up top.

Sometimes, items might be voided.  This means that the previous line is null and
void, and the customer didn't pay for it, so it won't reflect in the total.  Your
program should ignore these voided products.

So, about that report...

Anyway, the angry accountants would like your program to output a
CSV which tallies losses or gains from all stores, listed individually:

| store | plusminus |
|-------+-----------|
|     1 |   -123.40 |
|     3 |   +200.10 |

That way, they can track down the people responsible!

In addition, your CSV should be sorted by the plusminus column from low to
high.  The accountants care more about the business losing money, I guess!

The accountants might email you again with more stuff, so it'd be smart for your
program to take a parameter, say, `directory`, and output the CSV file?  Then you
can run it again and again when they give you more receipts to analyze!  I don't
know, you're the expert.
