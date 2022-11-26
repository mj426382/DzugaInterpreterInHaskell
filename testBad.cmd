for /l %%x in (1, 1, 9) do (
   .\compiler.exe bad/bad00%%x.lat
)

for /l %%x in (10, 1, 29) do (
   .\compiler.exe bad/bad0%%x.lat
)
