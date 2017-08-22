# Exercises for Chapter 1.

## Intermission: Equivalence Exercises (p.13)

1. `\xy.xz` is equivalent to `\mn.mz`.
2. `\xy.xxy` is equivalent to `\a.(\b.aab)`.
3. `\xyz.zx` is equivalent to `\tos.st`.

## Chapter Exercises (p.17)

### Combinators
1. `\x.xxx` is a combinator.
2. `\xy.zx` is not a combinator, `z` is free.
3. `\xyz.xy(zx)` is a combinator.
4. `\xyz.xy(zxy)` is a combinator
5. `\xy.xy(zxy)` is not a combinator, `z` is free

### Normal form or diverge?
1. `\x.xxx` is Normal
2. `(\z.zz)(\y.yy)` diverges
3. `(\x.xxx)z` is Normal


### Beta Reduce
1.  `(\abc.cba)zz(\wv.w)`

Reduction:
``` 
(\abc.cba)zz(\wv.w) ->
(\bc.cbz)z(\wv.w) ->
(\c.czz)(\wv.w) ->
(\wv.w)zz ->
(\v.z)z ->
z
```
  
2. `(\x.\y.xyy)(\a.a)b`

Reduction:
```
(\x.\y.xyy)(\a.a)b ->
(\y.(\a.a)yy)b ->
(\a.a)bb ->
bb
```

3. `(\y.y)(\x.xx)(\z.zq)`

Reduction:
(\y.y)(\x.xx)(\z.zq) ->
(\x.xx)(\z.zq) ->
(\z.zq)(\z.zq) ->
(\z.zq)q ->
(\z.zq)q ->
qq

4. `(\z.z)(\z.zz)(\z.zy)`

Reduction:
```
(\z.z)(\z.zz)(\z.zy) ->
(\z.zz)(\z.zy) ->
(\z.zy)(\z.zy) ->
(\z.zy)y ->
yy
```
5. `(\x.\y.xyy)(\y.y)y`

Reduction:
```
(\x.\y.xyy)(\y.y)y ->
(\y.(\y.y)yy)y
(\y.y)yy
yy
```

6. `(\a.aa)(\b.ba)c`
```
(\a.aa)(\b.ba)c ->
(\b.ba)(\b.ba)c ->
(\b.ba)ac ->
aac
```

7. `(\xyz.xz(yz))(\x.z)(\x.a)`

Reduction:
```
(\xyz.xz(yz))(\x.z)(\x.a) ->
(\z.(\x.z1)z((\x.a)z))
(\z.z1a)
```
