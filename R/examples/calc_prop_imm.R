calc.prop.imm(0.9, 0.8) # Those who get first dose most likely to get second

calc.prop.imm(1,1) # Everyone gets both doses (yay!)

props <- calc.prop.imm(rbeta(1,3,1), rbeta(1,2,2)) 
sum(props[1:2,2]) # total proportion immune







