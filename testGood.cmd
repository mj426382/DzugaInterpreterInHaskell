for /l %%x in (1, 1, 9) do (
   .\compiler.exe good/core00%%x.lat
)

for /l %%x in (10, 1, 32) do (
   .\compiler.exe good/core0%%x.lat
)
