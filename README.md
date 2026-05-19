### Final Fantasy VI Library

[![Donate via Stripe](https://img.shields.io/badge/Donate-Stripe-green.svg)](https://buy.stripe.com/00gbJZ0OdcNs9zi288)<br>

# `vis.lua`:

This is a visualizer of all the FF6 data.

It shows maps, doors, NPCs, touch-triggers, treasure chests ... monster formations, encounters, event scripts, character baseline stats ... the list goes on.

The mouse controls attempt to be intuitiive.  Double-click doors to travel through them.  Double-click events or NPCs to jump to that location in the event script.

I've got some animation stuff working as well.  Torches flicker and gears turn.

# `run.lua`:

This hacks out (tentatively) all the resources of the game.
It outputs text and data structures to stdout.
Images are written to png files.
Audio is written to BRR and WAV files.

Usage: `luajit run.lua <romfile> [<randomized-outfile>]`

There is a half-finished randomizer at the end of `run.lua`.  Pass a second argument to write out a randomized ROM.

# `sram.lua`:

This is a SRAM dump file.  I just made it to try to 100% as many encounter battles as I could.

# Requirements:

Code requirements are found in the `distinfo` list under `deps`.  This is a system in progress, as it doesn't list the dependent repos, but only their local directory/`require` names, but you can find matching repos either in my account with suffix/prefix `-lua`, or in the distributable release.

# Resources:

Resources:
- http://www.rpglegion.com/ff6/hack/ff3info.txt
- https://github.com/subtractionsoup/beyondchaos
- https://github.com/everything8215/ff6
- https://github.com/everything8215/ff6tool
- https://everything8215.github.io/ff6tools/ff6tools.html
- just asking everything8215
