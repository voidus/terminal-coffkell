Boooooooring.

Anyways, here's what would happen if you'd play along.

As you know, your haskell program wouldn't compile. Instead, it would print something like this:

```
src/Examples.hs:30:2: error: [GHC-64725]
    • ======================================
      == Oops, I wrote a paper instead D: ==
      ======================================
      ====== check out Whitepaper.pdf ======
      ======================================
    • In the expression: print Whitepaper
      In an equation for ‘yourProgram’: yourProgram = print Whitepaper
   |
30 | $( do
   |  ^^^^...
```

Additionally, a Whitepaper.pdf would be generated with the response from the API.
(Not being able to compile has never stopped anyone!)

Check the rejected-papers folder for example output from the different endpoints.
