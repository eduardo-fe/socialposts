# What if Abstention Had Parliamentary Seats?  Replication Code.

**Simulating Escaños en Blanco in Spain's 2023 General Election**


## Overview

In many modern democracies, **abstention** is one of the largest "votes" — often larger than the leading party. Yet it remains invisible in parliaments: no seats, no voice, no impact on policy.

**Escaños en Blanco (EB)** is a real Spanish political party that offers a creative alternative: voters can support EB to win seats that are deliberately left **empty**. Elected candidates refuse the oath of office, occupy no salary, and leave the seat vacant for the entire legislature — visibly signaling voter discontent directly inside Parliament.

This repository contains code to simulate what would happen if a portion of Spain's **abstention** votes from the **July 2023 general election** were attributed to EB, using the official **D’Hondt proportional allocation** method.

Key questions explored:
- How many seats would EB win at 25%, 50%, or 100% attribution of abstention votes?
- How would the seat distribution change for PP, PSOE, Vox, Sumar, and regional/nationalist parties?
- Would majorities still form? Would fringe parties lose representation?
- What financial savings could empty seats represent?

## Main Findings (based on 2023 election data)

Using official 2023 results (Congress of Deputies: 350 seats):

- **Actual turnout** ≈ 70.4% → **abstention** ≈ 29.6% of registered voters (roughly 10–11 million people did not vote).
- Simulations assume different fractions of abstention votes are reassigned to EB.

### Simulated outcomes:

- **25% attribution to EB**  
  All major parties lose seats.  
  Some smaller nationalist/regional parties could fall below thresholds or lose all representation (e.g., certain Galician or other minority parties).  
  No bloc (left or right) reaches the 176-seat absolute majority → forces cooperation between PP (main right) and PSOE (main left).

- **50% attribution to EB**  
  EB becomes the clear **third-largest force**.  
  Continued legislative deadlock without broad agreements.

- **100% attribution to EB**  
  Very large EB bloc.  
  Any legislation requires coordination among PSOE, PP, **and** at least one other party (e.g. Vox or Sumar).

**Broader political effect**: Reduces kingmaker power of small/extremist/regional parties and pushes negotiation toward the two main statewide forces (PP & PSOE).

**Financial angle**: Each empty seat avoids ≈ **€386,000/year** in direct MP costs (2022 figure) plus associated party subsidies. With thousands of elected positions and advisors across Spain, the systemic savings and incentive realignment could be substantial.

## Repository Contents

- `reparto.R`: Core script applying D’Hondt method with abstention reallocation
- `vote2023.csv` — Party votes by constituency (sourced/adapted from official BOE/Junta Electoral data)
