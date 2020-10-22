---
title: CRDT
revealOptions:
    transition: 'none'
    slideNumber: true
---
<link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.0.13/css/all.css" integrity="sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp" crossorigin="anonymous">
<h1>CRDT</h1>

---

<b>C</b>onflict-free <b>R</b>eplicated <b>D</b>ata<b>T</b>ype

---

<h2>I. Why?</h2>

---

__CAP Theorem__

Consistency, Availability, resistance to Partition. Choose two.
Collaboration using computers is hard.

---

But collaboration is now crucial. Between humans, using Google Docs, but also between machines,
using clustered databases.

---

<h2>II. State of the Art</h2>

---

<h3>Operational Transformations</h3>

---

The idea is to generate operations on the client then funnel them all through a central
server who will adjust them based on the operations of the other clients and then
distribute them :

<img src="assets/ot.png" height="400px">

---

This is the method used by Google Docs.

| + | - |
|---|---|
|Easier to implement | Requires central server |
|Less memory overhead | Limited scaling capabilities |

---

<h3>CRDTs</h3>

---

Clients are connected in a peer-to-peer fashion, the network can be intermittent,
but cannot be adversarial.  

The objective is that after enough time, if all the clients managed to exchange
with each other, every client has the same state (the state converges).

---

__State-based :__ exchange the whole state and merge it with your own (CmRDT)  
  
__Operation-based :__ exchange only operations and ensure it converges (CvRDT)  

---

1. Counter-based : grow-only counter, LWW counter
2. Set-based : grow-only set, 2P-set
3. More complex structures are based on these two !

---

This is still a very active research area.  
If you need a PhD...

---

<h2>III. Applications</h2>

---

__Databases__  
Redis, Riak, Apollo

---

__Frameworks__  
Phoenix, Y.js

---

__Applications__  
Apple Notes, Atom Editor, League of Legends chat, SoundCloud comments

---

<h2>IV. Example : Causal Trees</h2>

---

Imagine a text as a string of ordered letters, identified by unique IDs :

```
H    e    l    o
0.2  0.4  0.6  0.8
```

---

Adding a letter requires a new ID, correctly ordered (IDs must be dense) :

```
H    e    l    l    o
0.2  0.4  0.6  0.7  0.8
```

---

To make a CRDT you need to solve following problems :
- each ID must be unique and there must be a partial order
- each client must be able to generate IDs idependently

---

To make IDs unique and generatable, use a random host ID and an incrementing timestamp.

---

To ensure ordering, every letter is associated with its cause, building a *causal* tree.

<img src="assets/tree.svg" height="400px" style="background: white; padding: 10px"/>

---

To get the final text, walk the tree using DFS.

---

DEMO !  

<a href="https://exenon.github.com/crdt-presentation/crdt.html">https://exenon.github.com/crdt-presentation/demo.html</a>

