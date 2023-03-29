# sdc.broadband-dev

## Description of the repository
  - The dashboard branch is where the pages are referencing

## Description on how the measure is selected
  - We look for the cheapest plan above 100 Mb / second download speed
  - For each block, we randomly select an address in the block
  - To aggregate up, we use the median of the minimum in the group
