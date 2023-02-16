# sdc.broadband-dev

## Description on how the measure is selected
  - We look for the cheapest plan above 100 Mb / second
  - For each block, we randomly select an address in the block
  - To aggregate up, we use the median of the minimum in the group

## Resources
  - Look at the National Address Database to find random per county addresses
  - Query broadbandnow with the address until you at least get one price per county
