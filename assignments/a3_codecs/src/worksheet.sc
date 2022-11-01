



val personJson = Util.parseJson(""" { "name": "Bob", "age": 10 } """)


val person = personJson.flatMap(_.decodeAs[Person]) // This will crash until you implement it in this assignment


val implicitIntEncoder = implicitly[Encoder[Int]]


// Summon encoders for List[Int], List[Bool]
summon[Encoder[List[Int]]]
summon[Decoder[List[Boolean]]]

// JSON object codecs
val xField = ObjectEncoder.field[Int]("x")
val encodedXField = xField.encode(42)


val pointEncoder: ObjectEncoder[(Int, Int)] =
  val xField = ObjectEncoder.field[Int]("x")
  val yField = ObjectEncoder.field[Int]("y")
  xField.zip(yField)

// Person encoder / decoder
val personEncoder = implicitly[Encoder[Person]]
val personDecoder = implicitly[Decoder[Person]]

val person = Person("Alice", 42)
val encodedPerson = personEncoder.encode(person)
val personJson = Json.Obj(Map("name" -> Json.Str("Alice"), "age" -> Json.Num(42)))

assert(encodedPerson == personJson)
assert(person == personDecoder.decode(encodedPerson).get)

// Contact encoder / decoder
trait ContactsCodecs:

  // TODO Define the encoder and the decoder for `Contacts`
  // The JSON representation of a value of type `Contacts` should be
  // a JSON object with a single field named “people” containing an
  // array of values of type `Person` (reuse the `Person` codecs)

  given Encoder[Contacts] =
    ObjectEncoder.field[List[Person]]("people")
      .transform { case Contacts(people) => people }

  given Decoder[Contacts] =
    Decoder.field[List[Person]]("people")
    .transform {
      case x :: xs => Contacts(x :: xs)
      case Nil => Contacts(List())
    }

val contactsEncoder = implicitly[Encoder[Contacts]]
val contactsDecoder = implicitly[Decoder[Contacts]]

// Contacts

val contacts = Contacts(List(Person("Alice", 42)))
val encodedContacts = contactsEncoder.encode(contacts)
val contactsJson = Json.Obj(Map("people" ->
  Json.Arr(List(Json.Obj(Map("name" -> Json.Str("Alice"), "age" -> Json.Num(42)))))
))

assert(contactsJson == encodedContacts)
assert(contactsDecoder.decode(contactsJson) == contacts)
