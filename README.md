# Concurrent Shenzhen Solitaire Solver

This project aims to write a program that will attempt to essentially brute-force a _Shenzhen Solitaire_ board in parallel.

The program will do so in the following steps:

1. Identify legal moves
    1. If no legal moves, kill thread (or halt)
    2. Otherwise, spawn a thread for each legal move
3. Copy current board state to each thread, then perform each move in each thread
4. Record move in each thread
5. Check board state
    1. If board isn't solved, goto 1
    2. Otherwise finish

## Why Shenzhen Solitaire, why not &lt;insert other solitaire here&gt;?
Simple, because it has relatively few cards in play (37 compared to 52), and because the entire board is visible at all times. 
There's no deck to draw from, and no cards are turned over.

## What's the point of this program?
There is no point, I'm just bored.