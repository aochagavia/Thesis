# Preliminary results {#results}

## Measuring the impact of limitations 1 and 2

### People giving up because of style issues

Interactions through the web interface. How many people give up or have to start again because of style issues (limitation 1)?

It seems tricky to check style on incomplete programs. For instance, if the model solution expects you to use `concatMap` but you start with `concat ?`, you are no longer on the way to the model solution. Also, using concat is not a problem from a style perspective, so there is no style feedback you can use here. You would only get feedback when you use `concat (map ? ?)`.

### Causes of program matching failure

We analyze data from the Functional Programming course at Universiteit Utrecht.

The first assignment of the course consists: 111 submissions, 8 functions each (therefore 888 exercises). Note: would it be sensible to use sampling in order to bring down the amount of stuff to analyze? On the other hand, I think I should have enough time.

Interactions through the web interface.  How many people reach a full answer that is not matched because of style issues (limitation 2)?

## Approach to offering refactoring hints

What examples can you handle already?

What prototype have I built?

How can I generalize these results? What problems have I identified or do I expect?

Performance problems?
