module C05.S9 where

import Prelude

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

sameCity :: Person -> Person -> Boolean
sameCity {address:{city:cx}} {address:{city:cy}} = cx == cy

person1 = {name:"hoge",address:{city:"Kawasaki",street:"Fuchu"}} :: Person
person2 = {name:"hige",address:{city:"Kawasaki",street:"Tsunashima"}} :: Person
person3 = {name:"hage",address:{city:"Yokohama",street:"Fuchu"}} :: Person

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [y] = y
fromSingleton x _ = x
