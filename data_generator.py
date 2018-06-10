import random

file = open('data.txt', 'w') 

for _ in range(1000):
    rgb = str(random.random()*255)+' '+str(random.random()*255)+' '+str(random.random()*255) +'\n'
    file.write(rgb)

file.close() 