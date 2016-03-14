import SwiftFoundation

protocol WeakEquatable {
  func weakEqual(other: WeakEquatable) -> Bool
}

extension Int: WeakEquatable {
  func weakEqual(other: WeakEquatable) -> Bool {
    if other is Int {
      return self == (other as! Int)
    }
    return false
  }
}

extension Character: WeakEquatable {
  func weakEqual(other: WeakEquatable) -> Bool {
    if other is Character {
      return self == (other as! Character)
    }
    return false
  }
}

class EndOfList : WeakEquatable {
  func weakEqual(other: WeakEquatable) -> Bool {
    if other is EndOfList {
      return true
    }

    return false
  }
}

class PeanoZero : WeakEquatable {
  func weakEqual(other: WeakEquatable) -> Bool {
    if other is PeanoZero {
      return true
    }

    return false
  }
}

class PeanoSucc : WeakEquatable {
  func weakEqual(other: WeakEquatable) -> Bool {
    if other is PeanoSucc {
      return true
    }

    return false
  }
}

struct Variable : CustomStringConvertible, Hashable, WeakEquatable {
  let name: String

  var description: String {
    return name
  }

  var hashValue: Int {
    return name.hashValue
  }

  func weakEqual(other: WeakEquatable) -> Bool {
    if other is Variable {
      return self == (other as! Variable)
    }
    return false
  }
}

func ==(lhs: Variable, rhs: Variable) -> Bool {
  return lhs.name == rhs.name
}

struct Pair: CustomStringConvertible, WeakEquatable {
  let first: WeakEquatable
  let second: WeakEquatable

  var description: String {
    return "(\(first), \(second))"
  }

  func weakEqual(arg: WeakEquatable) -> Bool {
    if arg is Pair {
      let other = arg as! Pair
      return self.first.weakEqual(other.first) &&
        self.second.weakEqual(other.second)
    }
    return false
  }
}

struct State {
  let variables: [Variable]
  let values: [Variable: WeakEquatable]

  func createVariables(names: [String]) -> (State, [Variable]) {
    let newVariables = names.map({name in Variable(name: name)})

    return (
      State(
        variables: variables + newVariables,
        values: values
      ),
      newVariables
    )
  }

  func assignValues(newValues: [Variable: WeakEquatable]) -> State {
    var merged = values
    newValues.forEach { (key, value) in
      merged[key] = value
    }

    return State(
      variables: variables,
      values: merged
    )
  }

  func valueOf(key: WeakEquatable) -> WeakEquatable {
    if key is Variable {
      if let value = values[key as! Variable] {
        return valueOf(value)
      }
    }

    return key
  }

  func unify(x: WeakEquatable, y: WeakEquatable) -> State? {
    //print("unify:: x =", x, "y =", y, "::entry::")

    let (a, b) = (valueOf(x), valueOf(y))
    var result: State? = nil

    if a.weakEqual(b) {
      result = self
    } else if a is Variable {
      result = assignValues([(a as! Variable): b])
    } else if b is Variable {
      result = assignValues([(b as! Variable): a])
    } else if a is Pair && b is Pair {
      let (pa, pb) = (a as! Pair, b as! Pair)
      result = unify(pa.first, y: pb.first)?.unify(pa.second, y: pb.second)
    } else {
      result = nil
    }

    //print("unify:: x =", x, "y =", y, "result =", result)
    return result
  }

  func expand(key: WeakEquatable) -> WeakEquatable {
    let value = valueOf(key)
    //print("expand:: state =", self.values, " key =", key, "value =", value, "::entry::")

    var result: WeakEquatable = value

    if value is Pair {
      let pair = value as! Pair
      result = Pair(
        first: expand(pair.first),
        second: expand(pair.second)
      )
    }

    //print("expand:: key =", key, "value =", value, "result =", result)
    return result
  }

  func results(n: Int) -> [WeakEquatable] {
    return variables[0..<n].map { variable in
      return expand(variable)
    }
  }

  var result: WeakEquatable {
    return results(1).first!
  }
}

class StateYielder: SequenceType {
  let body: () -> State?

  init(body: () -> State?) {
    self.body = body
  }

  func generate() -> AnyGenerator<State> {
    return AnyGenerator {
      return self.body()
    }
  }
}

class Goal {
  let pursueIn: (State) -> StateYielder

  init(pursueIn: (State) -> StateYielder) {
    self.pursueIn = pursueIn
  }

  func pursueInAll(states: StateYielder) -> StateYielder {
    return pursueInAll(states.generate())
  }

  func pursueInAll(remaining: AnyGenerator<State>) -> StateYielder {
    var stillYielding = StateYielder { return nil }.generate()

    return StateYielder {
      if let next = stillYielding.next() {
        return next
      }

      if let first = remaining.next() {
        let (firstStream, remainingStreams) =
          (self.pursueIn(first), self.pursueInAll(remaining))

        stillYielding = Goal
          .interleave(firstStream, b: remainingStreams)
          .generate()
      }

      if let next = stillYielding.next() {
        return next
      }

      return nil
    }
  }

  class func equal(a: WeakEquatable, b: WeakEquatable) -> Goal {
    return Goal { (state) in
      let newState = state.unify(a, y: b)
      var yielded = false

      return StateYielder {
        if yielded { return nil }
        yielded = true
        return newState
      }
    }
  }

  class func tupleFrom<T: WeakEquatable>(
    t: (Int),
    a: [T]) ->
    (T) {
    return (a[0])
  }

  class func tupleFrom<T: WeakEquatable>(
    t: (Int, Int),
    a: [T]) ->
    (T, T) {
    return (a[0], a[1])
  }

  class func tupleFrom<T: WeakEquatable>(
    t: (Int, Int, Int),
    a: [T]) ->
    (T, T, T) {
    return (a[0], a[1], a[2])
  }

  class func tupleFrom<T: WeakEquatable>(
    t: (Int, Int, Int, Int),
    a: [T]) ->
    (T, T, T, T) {
    return (a[0], a[1], a[2], a[3])
  }

  class func tupleFrom<T: WeakEquatable>(
    t: (Int, Int, Int, Int, Int),
    a: [T]) ->
    (T, T, T, T, T) {
    return (a[0], a[1], a[2], a[3], a[4])
  }

  class func withVariables(n: Int, body: ([Variable]) -> Goal) -> Goal {
    let names = (0..<n).map { it in "var_\(it)_\(UUID().rawValue)" }

    return Goal { state in
      let (nextState, vars) = state.createVariables(names)
      return body(vars).pursueIn(nextState)
    }
  }

  class func interleave(a: StateYielder, b: StateYielder) -> StateYielder {
    let yielders = [a.generate(), b.generate()]
    var index = 0
    return StateYielder {
      index = (index + 1) % 2
      if let next = yielders[index].next() {
        return next
      }

      index = (index + 1) % 2
      if let next = yielders[index].next() {
        return next
      }

      return nil
    }
  }

  class func either(first: Goal, second: Goal) -> Goal {
    return Goal { state in
      return interleave(
        first.pursueIn(state),
        b: second.pursueIn(state)
      )
    }
  }

  class func both(first: Goal, second: Goal) -> Goal {
    return Goal { state in
      return second.pursueInAll(first.pursueIn(state))
    }
  }

  class func toList(string: String) -> WeakEquatable {
    return toList(string.characters.map { it in it })
  }

  class func toList(array: [WeakEquatable]) -> WeakEquatable {
    return toList(0, slice: array[0..<array.count])
  }

  class func toList(index: Int, slice: ArraySlice<WeakEquatable>) -> WeakEquatable {

    if slice.isEmpty {
      return EndOfList()
    }

    let newIndex = index + 1
    let rightIndex = index + slice.count
    let rest = slice[newIndex..<rightIndex]
    return Pair(
      first: slice[index],
      second: toList(newIndex, slice: rest)
    )
  }

  class func fromList(value: WeakEquatable) -> [WeakEquatable]? {
    if value is EndOfList {
      return []
    }

    if value is Pair {
      let list = value as! Pair

      if let rest = fromList(list.second) {
        var result = [list.first]
        result += rest
        return result
      }
    }

    return nil
  }

  class func stringFromList(list: WeakEquatable) -> String? {
    return fromList(list)?.reduce("") { (acc, item) in
      if let string = acc {
        if let char = item as? Character {
          return string + String(char)
        }
      }

      return nil
    }
  }

  class func append(
    first: WeakEquatable,
    second: WeakEquatable,
    result: WeakEquatable) -> Goal {
      return Goal.either(
        Goal.both(
          Goal.equal(first, b: EndOfList()),
          second: Goal.equal(second, b: result)
        ),
        second: Goal.withVariables(3) { vars in
          let (head, firstTail, resultTail) = (vars[0], vars[1], vars[2])
          return Goal.both(
            Goal.both(
              Goal.equal(first, b: Pair(first: head, second: firstTail)),
              second: Goal.equal(result, b: Pair(first: head, second: resultTail))
            ),
            second: append(firstTail, second: second, result: resultTail)
          )
        }
      )
  }

  class func toPeano(number: Int) -> WeakEquatable {
    if number == 0 {
      return PeanoZero()
    }

    return Pair(first: PeanoSucc(), second: toPeano(number - 1))
  }

  class func fromPeano(number: WeakEquatable) -> Int? {
    if number is PeanoZero {
      return 0
    }

    if let pair = number as? Pair {
      if let smaller = fromPeano(pair.second) {
        return 1 + smaller
      }
    }

    return nil
  }

  class func add(
    first: WeakEquatable,
    second: WeakEquatable,
    result: WeakEquatable) -> Goal {
      return Goal.either(
        Goal.both(
          Goal.equal(first, b: PeanoZero()),
          second: Goal.equal(second, b: result)
        ),

        second: Goal.withVariables(2) { vars in
          let (smallerFirst, smallerResult) = (vars[0], vars[1])
          return Goal.both(
            Goal.both(
              Goal.equal(
                first,
                b: Pair(first: PeanoSucc(), second: smallerFirst)
              ),

              second: Goal.equal(
                result,
                b: Pair(first: PeanoSucc(), second: smallerResult)
              )
            ),

            second: add(smallerFirst, second: second, result: smallerResult)
          )
        }
      )
    }
}

let s = State(variables: [], values: [:])
let (s1, vars) = s.createVariables(["x", "y", "z"])
let (x, y, z): (Variable, Variable, Variable) = (vars[0], vars[1], vars[2])
let s2 = s1.assignValues([x: y, y: z, z: 5])

let h = Variable(name: "h")

let g = Goal.equal(h, b: x)
for state in g.pursueIn(s2) {
  print(state.values)
}

print(Goal.withVariables(1) { vars in
  let (x) = Goal.tupleFrom((1), a: vars)
  return Goal.equal(x, b: 5)
}.pursueIn(s).generate().next()!.values)

let eitherSample = Goal.withVariables(1) { vars in
  let x = vars[0]
  return Goal.either(
    Goal.equal(x, b: 5),
    second: Goal.equal(x, b: 6)
  )
}

for state in eitherSample.pursueIn(s) {
  print("either sample:", state.values)
}

let bothSample = Goal.withVariables(2) { vars in
  let (x, y) = (vars[0], vars[1])
  return Goal.both(
    Goal.equal(x, b: 7),
    second: Goal.either(
      Goal.equal(y, b: 5),
      second: Goal.equal(y, b: 6)
    )
  )
}

let impossibleBoth = Goal.withVariables(1) { vars in
  let x = vars[0]
  return Goal.both(
    Goal.equal(x, b: 1),
    second: Goal.equal(2, b: x)
  )
}

print(bothSample.pursueIn(s).map { it in it.values })
print(impossibleBoth.pursueIn(s).map { it in it.values })

print(Goal.withVariables(2) { vars in
  let (x, y) = (vars[0], vars[1])
  return Goal.equal(
    Pair(first: 3, second: x),
    b: Pair(first: y, second: Pair(first: 5, second: y))
  )
}.pursueIn(s).map { it in it.result })

print(Goal.withVariables(3) { vars in
  let (x, y, z) = (vars[0], vars[1], vars[2])
  return Goal.equal(
    Goal.toList([x, 2, z]),
    b: Goal.toList([1, y, 3])
  )
}.pursueIn(s).map { it in it.results(3) })

func headTail(head: WeakEquatable, tail: WeakEquatable, list: WeakEquatable) -> Goal {
  return Goal.equal(list, b: Pair(first: head, second: tail))
}

func commonHead(a: WeakEquatable, b: WeakEquatable) -> Goal {
  return Goal.withVariables(3) { vars in
    let (head, firstTail, secondTail) = (vars[0], vars[1], vars[2])
    return Goal.both(
      headTail(head, tail: firstTail, list: a),
      second: headTail(head, tail: secondTail, list: b)
    )
  }
}

print(Goal.withVariables(1) { vars in
  return Goal.append(
    Goal.toList("hel"),
    second: Goal.toList("lo"),
    result: vars[0]
  )
}.pursueIn(s).map { it in Goal.stringFromList(it.result) })

print(Goal.withVariables(1) { vars in
  return Goal.append(
    vars[0],
    second: Goal.toList("orld"),
    result: Goal.toList("hello world")
  )
}.pursueIn(s).map { it in Goal.stringFromList(it.result) })

print(Goal.withVariables(2) { vars in
  return Goal.append(
    vars[0],
    second: vars[1],
    result: Goal.toList("hello world")
  )
}.pursueIn(s).map { it in
  it.results(2).map { it in
    return Goal.stringFromList(it)
  }
})

print(Goal.withVariables(1) { vars in
  let x = vars[0]
  return Goal.append(
    Goal.toList("he"),
    second: Goal.toList("llo"),
    result: x
  )
}.pursueIn(s).map { it in Goal.stringFromList(it.result) })

print(Goal.withVariables(1) { vars in
  let x = vars[0]
  return Goal.add(
    Goal.toPeano(5),
    second: Goal.toPeano(3),
    result: x
  )
}.pursueIn(s).map { it in Goal.fromPeano(it.result) })


print(Goal.withVariables(1) { vars in
  let x = vars[0]
  return Goal.add(
    Goal.toPeano(5),
    second: x,
    result: Goal.toPeano(17)
  )
}.pursueIn(s).map { it in Goal.fromPeano(it.result) })
