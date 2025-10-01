7. Operational Semantics (Microstep Machine)
We abstract a machine 
M
=
(
S
,
⇒
,
c
o
s
t
)
M=(S,⇒,cost):

State 
σ
=
(
K
,
Q
)
σ=(K,Q) with pending hook queue 
Q
Q.

Transition 
⇒
⇒ executes at most one hook per reaction.

Cost model: each macro reaction decomposes into at most 8 primitives:

constant-time dispatch,
2–3. bounded memory access,
4–6. pure effect on bounded footprint,

receipt hashing (bounded prefix),

deadline check & enqueue.

Let 
c
o
s
t
:
cost: primitive 
→
{
1
}
→{1} and total budget 
Θ
=
8
Θ=8.

Boundedness Theorem (Chatman). If each hook satisfies:

Dispatchability: 
O
(
1
)
O(1) selection independent of 
∣
K
∣
∣K∣.

Locality: effect reads/writes a constant-size fiber of 
K
K.

Receipt prefix bound: 
∣
c
(
K
′
)
∣
∣c(K 
′
 )∣ truncated to a constant-size digest input.
Then every reaction step 
σ
⇒
σ
′
σ⇒σ 
′
  consumes ≤ 
Θ
Θ primitives.

Proof sketch. Map sub-phases to primitives 1–8; each is constant-time under locality and bounded receipt; sum ≤ 8.

