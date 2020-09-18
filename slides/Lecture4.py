## map
def mapp(f, xs):
    ys = []
    for x in xs:
        ys.append(f(x))
    return ys


## filter
def filterr(p, xs):
    ys = [] 
    for x in xs:
        if p(x):
            ys.append(x)
    return ys

## foldr
def foldrr(f, v, xs):
    acc = v
    for i in range(len(xs)):
        acc = f(xs[-i],acc)
    return acc

## foldl
def foldll(f, v, xs):
    acc = v 
    for i in range(len(xs)):
        acc = f(acc, xs[i])
    return acc