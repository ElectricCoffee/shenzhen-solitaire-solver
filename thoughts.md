# Thoughts and Ideas
This file will contain the thoughts about the project structure that I make.

## Definitions
| Term        | Definition    |
|-------------|---------------|
| Board       | The play-area |
| Board State | The way the cards are currently laid out on the board |

## Problems
A few key problems come to mind when thinking of the way the program will execute is 
1. Identifying legal moves
2. Executing said moves

### Method 1: List Then Execute
This first method has one thread go through the current board state, listing the legal moves before spinning up a number of threads to then execute those moves.

#### Pros
- Finds all the legal moves in one pass (easier to debug)

#### Cons 
- Some sort of notation will have to be invented, which lists a source and a destination
- Then each child thread will have to spend time looking for the given card before it can perform the move

### Method 2: Execute as You Look
This method skips the listing step and deploys a thread whenever it finds a legal move.

#### Pros
- Skips potentially having to go through the list twice (minimal bonus tbh)

#### Cons
- Special notation _may_ still need to be required, as the thread will be sent its initial "legal move" instruction, though this could possibly be mitigated by spawning the thread with the source and destination already available?
- If the notation is still required, it has the same problem as Method 1, namely that time will be spent looking for source and destination

### Method 3: Execute All the Things!
This method basically involves having one thread per card per board state.
Possibly the most naïve approach.
If the thread cannot legally execute, it just dies.

1. Spawn 40 threads (one per card). 
2. Each of those threads will then attempt to perform every possible move concurrently.
3. If board solved, return path.
4. If move not legal, kill thread.
5. Else, goto 1.
6. Done.

#### Pros
- Doesn't suffer from the issue of having to send a message to the child process to tell it which card to move from and to, it simply spawns a thread and does it.
- Doesn't have to aggregate a list of possible moves.
#### Cons
- Possibly incredibly expensive, spawning at most 40 threads **per board state**.
  That being said, Erlang processes are incredibly lightweight, so this may not be a problem at all.
- Impossible to debug.

## Choosing a Solution
As fun and balls-to-the-wall as Method 3 may be, I think the best way of going about things is structured: spawn one thread if the move is legal, otherwise skipping it entirely and moving on.
In other words: Method 2, or a derivation thereof.